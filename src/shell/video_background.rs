// SPDX-License-Identifier: GPL-3.0-only

//! Video wallpaper support for the COSMIC compositor.
//!
//! This module provides video rendering for wallpapers by decoding video
//! directly within the compositor.
//!
//! ## Architecture
//!
//! The video background manager handles:
//! 1. GStreamer pipeline creation and management
//! 2. Frame extraction from appsink
//! 3. Texture upload to smithay's renderer
//!
//! ## GPU Acceleration
//!
//! When available, the pipeline uses DMA-BUF for zero-copy GPU rendering:
//! - VA-API hardware decoding outputs directly to DMA-BUF
//! - DMA-BUF is imported into the compositor's renderer without CPU copies
//!
//! Fallback to CPU path when DMA-BUF is not available.
//!
//! ## Thread Safety
//!
//! For the KMS backend which uses a separate rendering thread, we provide
//! `SharedVideoFrames` - a thread-safe container for video frame data that
//! can be read by the KMS surface thread while the main thread updates frames.
//!
//! ## Integration
//!
//! Video backgrounds are rendered as the bottommost layer, behind all
//! layer shell surfaces including cosmic-bg.

use std::{
    collections::HashMap,
    os::fd::OwnedFd,
    path::{Path, PathBuf},
    sync::{Arc, RwLock},
};

use gstreamer as gst;
use gstreamer::prelude::*;
use gstreamer_allocators as gst_allocators;
use gstreamer_app as gst_app;
use gstreamer_video as gst_video;

use smithay::{
    backend::{
        allocator::{Fourcc, Modifier},
        renderer::{
            ImportDma, ImportMem, Renderer,
            element::texture::{TextureBuffer, TextureRenderElement},
            gles::GlesTexture,
        },
    },
    utils::{Physical, Point, Size, Transform},
};
use tracing::{debug, error, info, warn};

/// Error type for video background operations.
#[derive(Debug, thiserror::Error)]
pub enum VideoBackgroundError {
    #[error("GStreamer error: {0}")]
    GStreamer(String),
    #[error("Pipeline error: {0}")]
    Pipeline(String),
    #[error("Video file not found: {0}")]
    FileNotFound(PathBuf),
    #[error("Unsupported video format")]
    UnsupportedFormat,
    #[error("Renderer error: {0}")]
    Renderer(String),
}

impl From<gst::glib::Error> for VideoBackgroundError {
    fn from(e: gst::glib::Error) -> Self {
        VideoBackgroundError::GStreamer(e.to_string())
    }
}

impl From<gst::glib::BoolError> for VideoBackgroundError {
    fn from(e: gst::glib::BoolError) -> Self {
        VideoBackgroundError::GStreamer(e.to_string())
    }
}

impl From<gst::StateChangeError> for VideoBackgroundError {
    fn from(e: gst::StateChangeError) -> Self {
        VideoBackgroundError::GStreamer(format!("State change error: {:?}", e))
    }
}

/// Thread-safe video frame data for a single output.
///
/// This allows the main thread to update frame data while the KMS
/// surface thread reads it for rendering.
#[derive(Debug, Clone)]
pub struct SharedVideoFrame {
    /// Frame data (CPU pixels or DMA-BUF reference)
    pub data: Arc<RwLock<Option<VideoFrameData>>>,
}

/// DMA-BUF plane information for GPU import.
#[derive(Debug, Clone)]
pub struct DmaBufPlane {
    /// File descriptor for the DMA-BUF (owned)
    pub fd: std::sync::Arc<OwnedFd>,
    /// Offset into the buffer
    pub offset: u32,
    /// Stride (bytes per row)
    pub stride: u32,
}

/// DMA-BUF frame data for zero-copy GPU rendering.
#[derive(Debug, Clone)]
pub struct DmaBufFrameData {
    /// Planes of the DMA-BUF
    pub planes: Vec<DmaBufPlane>,
    /// Pixel format (fourcc)
    pub format: Fourcc,
    /// DRM format modifier
    pub modifier: Modifier,
    /// Frame dimensions
    pub size: Size<i32, Physical>,
}

/// Video frame source - either CPU pixels or DMA-BUF.
#[derive(Debug, Clone)]
pub enum VideoFrameSource {
    /// CPU pixel data (BGRA)
    Cpu(Vec<u8>),
    /// DMA-BUF for zero-copy GPU import
    DmaBuf(DmaBufFrameData),
}

/// Raw video frame data.
#[derive(Debug, Clone)]
pub struct VideoFrameData {
    /// Frame source (CPU or DMA-BUF)
    pub source: VideoFrameSource,
    /// Frame dimensions
    pub size: Size<i32, Physical>,
    /// Frame sequence number (incremented each new frame)
    pub sequence: u64,
}

impl VideoFrameData {
    /// Get pixel data if this is a CPU frame.
    pub fn pixels(&self) -> Option<&[u8]> {
        match &self.source {
            VideoFrameSource::Cpu(data) => Some(data),
            VideoFrameSource::DmaBuf(_) => None,
        }
    }

    /// Check if this is a DMA-BUF frame.
    pub fn is_dmabuf(&self) -> bool {
        matches!(&self.source, VideoFrameSource::DmaBuf(_))
    }

    /// Get DMA-BUF data if available.
    pub fn dmabuf(&self) -> Option<&DmaBufFrameData> {
        match &self.source {
            VideoFrameSource::DmaBuf(data) => Some(data),
            VideoFrameSource::Cpu(_) => None,
        }
    }
}

impl SharedVideoFrame {
    /// Create a new empty shared frame.
    pub fn new() -> Self {
        Self {
            data: Arc::new(RwLock::new(None)),
        }
    }

    /// Update the frame data with CPU pixels, reusing the existing buffer if possible.
    pub fn update(&self, pixels: Vec<u8>, size: Size<i32, Physical>, sequence: u64) {
        if let Ok(mut guard) = self.data.write() {
            match &mut *guard {
                Some(existing) => {
                    if let VideoFrameSource::Cpu(ref mut existing_pixels) = existing.source {
                        if existing_pixels.len() == pixels.len() {
                            // Reuse existing buffer - just copy data
                            existing_pixels.copy_from_slice(&pixels);
                            existing.size = size;
                            existing.sequence = sequence;
                            return;
                        }
                    }
                    // Size changed or was DMA-BUF, allocate new
                    *guard = Some(VideoFrameData {
                        source: VideoFrameSource::Cpu(pixels),
                        size,
                        sequence,
                    });
                }
                None => {
                    // First frame
                    *guard = Some(VideoFrameData {
                        source: VideoFrameSource::Cpu(pixels),
                        size,
                        sequence,
                    });
                }
            }
        }
    }

    /// Update the frame data by copying from a slice, reusing existing buffer.
    pub fn update_from_slice(&self, data: &[u8], size: Size<i32, Physical>, sequence: u64) {
        if let Ok(mut guard) = self.data.write() {
            match &mut *guard {
                Some(existing) => {
                    if let VideoFrameSource::Cpu(ref mut existing_pixels) = existing.source {
                        if existing_pixels.len() == data.len() {
                            // Reuse existing buffer - just copy data
                            existing_pixels.copy_from_slice(data);
                            existing.size = size;
                            existing.sequence = sequence;
                            return;
                        }
                    }
                    // Size changed or was DMA-BUF, allocate new
                    *guard = Some(VideoFrameData {
                        source: VideoFrameSource::Cpu(data.to_vec()),
                        size,
                        sequence,
                    });
                }
                None => {
                    // First frame
                    *guard = Some(VideoFrameData {
                        source: VideoFrameSource::Cpu(data.to_vec()),
                        size,
                        sequence,
                    });
                }
            }
        }
    }

    /// Update the frame with DMA-BUF data (zero-copy GPU path).
    pub fn update_dmabuf(&self, dmabuf_data: DmaBufFrameData, sequence: u64) {
        if let Ok(mut guard) = self.data.write() {
            let size = dmabuf_data.size;
            *guard = Some(VideoFrameData {
                source: VideoFrameSource::DmaBuf(dmabuf_data),
                size,
                sequence,
            });
        }
    }

    /// Get a clone of the current frame data.
    pub fn get(&self) -> Option<VideoFrameData> {
        self.data.read().ok()?.clone()
    }

    /// Check if there's frame data available.
    pub fn has_data(&self) -> bool {
        self.data.read().ok().map(|g| g.is_some()).unwrap_or(false)
    }
}
impl Default for SharedVideoFrame {
    fn default() -> Self {
        Self::new()
    }
}

/// Thread-safe container for all video frames (for KMS backend).
///
/// The main thread updates frames via `VideoBackgroundManager::poll_frames()`,
/// and the KMS surface thread reads them via this shared container.
#[derive(Debug, Clone, Default)]
pub struct SharedVideoFrames {
    /// Per-output frames
    outputs: Arc<RwLock<HashMap<String, SharedVideoFrame>>>,
    /// Default frame for outputs without specific config
    default: SharedVideoFrame,
    /// Whether video wallpaper is active
    active: Arc<RwLock<bool>>,
}

impl SharedVideoFrames {
    /// Create a new shared frames container.
    pub fn new() -> Self {
        Self {
            outputs: Arc::new(RwLock::new(HashMap::new())),
            default: SharedVideoFrame::new(),
            active: Arc::new(RwLock::new(false)),
        }
    }

    /// Get or create a shared frame for an output.
    pub fn get_or_create_output(&self, output_name: &str) -> SharedVideoFrame {
        let mut outputs = self.outputs.write().unwrap();
        outputs
            .entry(output_name.to_string())
            .or_insert_with(SharedVideoFrame::new)
            .clone()
    }

    /// Get the shared frame for an output (or default).
    pub fn get(&self, output_name: &str) -> Option<VideoFrameData> {
        // Try output-specific first
        if let Ok(outputs) = self.outputs.read() {
            if let Some(frame) = outputs.get(output_name) {
                if let Some(data) = frame.get() {
                    return Some(data);
                }
            }
        }
        // Fall back to default
        self.default.get()
    }

    /// Get the default shared frame.
    pub fn default_frame(&self) -> &SharedVideoFrame {
        &self.default
    }

    /// Check if video is active.
    pub fn is_active(&self) -> bool {
        *self.active.read().unwrap()
    }

    /// Set whether video is active.
    pub fn set_active(&self, active: bool) {
        *self.active.write().unwrap() = active;
    }

    /// Check if there's video data for an output.
    pub fn has_video(&self, output_name: &str) -> bool {
        if !self.is_active() {
            return false;
        }
        if let Ok(outputs) = self.outputs.read() {
            if outputs
                .get(output_name)
                .map(|f| f.has_data())
                .unwrap_or(false)
            {
                return true;
            }
        }
        self.default.has_data()
    }
}

/// Per-output video state.
struct OutputVideo {
    /// GStreamer pipeline
    pipeline: gst::Pipeline,
    /// AppSink for frame extraction
    appsink: gst_app::AppSink,
    /// Current frame data (BGRA pixels) - only used in CPU mode
    current_frame: Option<Vec<u8>>,
    /// Video dimensions
    video_size: Option<Size<i32, Physical>>,
    /// Video file path
    path: PathBuf,
    /// Cached texture buffer
    texture_buffer: Option<TextureBuffer<GlesTexture>>,
    /// Whether we have a new frame to upload
    needs_upload: bool,
    /// Shared frame for KMS thread access
    shared_frame: SharedVideoFrame,
    /// Frame sequence counter
    frame_sequence: u64,
    /// Whether pipeline is using DMA-BUF output
    uses_dmabuf: bool,
}

/// Manages video wallpapers for all outputs.
pub struct VideoBackgroundManager {
    /// Per-output video state
    outputs: HashMap<String, OutputVideo>,
    /// Default video for outputs without specific config
    default_video: Option<OutputVideo>,
    /// Whether GStreamer has been initialized
    initialized: bool,
    /// Shared frames for thread-safe access (for KMS backend)
    shared_frames: SharedVideoFrames,
}

impl std::fmt::Debug for VideoBackgroundManager {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("VideoBackgroundManager")
            .field("outputs", &self.outputs.keys().collect::<Vec<_>>())
            .field("has_default", &self.default_video.is_some())
            .field("initialized", &self.initialized)
            .finish()
    }
}

impl VideoBackgroundManager {
    /// Create a new video background manager.
    pub fn new() -> Self {
        Self {
            outputs: HashMap::new(),
            default_video: None,
            initialized: false,
            shared_frames: SharedVideoFrames::new(),
        }
    }

    /// Get a clone of the shared frames container for use by KMS threads.
    pub fn shared_frames(&self) -> SharedVideoFrames {
        self.shared_frames.clone()
    }

    /// Initialize GStreamer.
    pub fn init(&mut self) -> Result<(), VideoBackgroundError> {
        if self.initialized {
            return Ok(());
        }

        // Disable legacy gstreamer-vaapi plugin to prefer modern 'va' plugin
        // which has better DMABuf support
        unsafe {
            std::env::set_var("GST_PLUGIN_FEATURE_RANK", "vaapi:0");
            // Force GStreamer GL to use EGL on GBM (headless GPU rendering)
            std::env::set_var("GST_GL_PLATFORM", "egl");
            std::env::set_var("GST_GL_WINDOW", "gbm");
            // Specify render device
            std::env::set_var("GST_GL_DRM_DEVICE", "/dev/dri/renderD128");
        }

        gst::init()?;
        info!("GStreamer initialized for video wallpapers");
        self.initialized = true;

        Ok(())
    }

    /// Set the video for a specific output.
    pub fn set_video_for_output(
        &mut self,
        output_name: &str,
        path: &Path,
    ) -> Result<(), VideoBackgroundError> {
        if !self.initialized {
            self.init()?;
        }

        if !path.exists() {
            return Err(VideoBackgroundError::FileNotFound(path.to_path_buf()));
        }

        // Stop existing video for this output
        if let Some(old) = self.outputs.remove(output_name) {
            let _ = old.pipeline.set_state(gst::State::Null);
        }

        let shared_frame = self.shared_frames.get_or_create_output(output_name);
        let video = Self::create_video_pipeline(path, shared_frame)?;
        self.outputs.insert(output_name.to_string(), video);
        self.shared_frames.set_active(true);

        info!(output = output_name, path = %path.display(), "Video wallpaper set");
        Ok(())
    }

    /// Set the default video for all outputs without specific config.
    pub fn set_default_video(&mut self, path: &Path) -> Result<(), VideoBackgroundError> {
        if !self.initialized {
            self.init()?;
        }

        if !path.exists() {
            return Err(VideoBackgroundError::FileNotFound(path.to_path_buf()));
        }

        // Stop existing default video
        if let Some(old) = self.default_video.take() {
            let _ = old.pipeline.set_state(gst::State::Null);
        }

        let shared_frame = self.shared_frames.default_frame().clone();
        self.default_video = Some(Self::create_video_pipeline(path, shared_frame)?);
        self.shared_frames.set_active(true);

        info!(path = %path.display(), "Default video wallpaper set");
        Ok(())
    }

    /// Create a GStreamer pipeline for video playback.
    ///
    /// Tries DMA-BUF output for zero-copy GPU rendering first (using VA-API),
    /// falls back to CPU path with multi-threaded videoconvert if unavailable.
    fn create_video_pipeline(
        path: &Path,
        shared_frame: SharedVideoFrame,
    ) -> Result<OutputVideo, VideoBackgroundError> {
        let path_str = path
            .to_str()
            .ok_or_else(|| VideoBackgroundError::FileNotFound(path.to_path_buf()))?;

        // Try DMA-BUF pipeline first for zero-copy GPU rendering
        match Self::create_dmabuf_pipeline(path_str, path, shared_frame.clone()) {
            Ok(video) => return Ok(video),
            Err(e) => {
                warn!("DMA-BUF pipeline failed: {}, falling back to CPU", e);
            }
        }

        // Fall back to CPU pipeline
        Self::create_cpu_pipeline(path_str, path, shared_frame)
    }

    /// Create a DMA-BUF pipeline for zero-copy GPU rendering.
    ///
    /// Uses vapostproc to output DMA-BUF with ARGB format directly from VA-API.
    /// The key is implementing propose_allocation to advertise VideoMeta support,
    /// which is required by vapostproc for DMA-BUF output.
    fn create_dmabuf_pipeline(
        path_str: &str,
        path: &Path,
        shared_frame: SharedVideoFrame,
    ) -> Result<OutputVideo, VideoBackgroundError> {
        use gst_video::VideoMeta;

        warn!(
            "DEBUG: Creating DMA-BUF pipeline with VideoMeta support for: {}",
            path_str
        );

        // Create pipeline elements manually
        let pipeline = gst::Pipeline::new();

        let filesrc = gst::ElementFactory::make("filesrc")
            .property("location", path_str)
            .build()
            .map_err(|e| VideoBackgroundError::Pipeline(format!("filesrc: {}", e)))?;

        let decodebin = gst::ElementFactory::make("decodebin")
            .build()
            .map_err(|e| VideoBackgroundError::Pipeline(format!("decodebin: {}", e)))?;

        let vapostproc = gst::ElementFactory::make("vapostproc")
            .build()
            .map_err(|e| VideoBackgroundError::Pipeline(format!("vapostproc: {}", e)))?;

        // Caps filter to request DMA-BUF with ARGB format (AR24 in DRM fourcc)
        let capsfilter = gst::ElementFactory::make("capsfilter")
            .property(
                "caps",
                gst::Caps::builder("video/x-raw")
                    .features(["memory:DMABuf"])
                    .field("format", "DMA_DRM")
                    .field("drm-format", "AR24")
                    .build(),
            )
            .build()
            .map_err(|e| VideoBackgroundError::Pipeline(format!("capsfilter: {}", e)))?;

        // Create appsink with propose_allocation callback to support VideoMeta
        let appsink = gst_app::AppSink::builder()
            .name("sink")
            .sync(true)
            .max_buffers(2)
            .drop(true)
            .caps(
                &gst::Caps::builder("video/x-raw")
                    .features(["memory:DMABuf"])
                    .field("format", "DMA_DRM")
                    .field("drm-format", "AR24")
                    .build(),
            )
            .build();

        // Set up propose_allocation callback to advertise VideoMeta support
        // This is CRITICAL - vapostproc requires downstream to support VideoMeta for DMABuf output
        appsink.set_callbacks(
            gst_app::AppSinkCallbacks::builder()
                .propose_allocation(|_appsink, query| {
                    // Add VideoMeta support to the allocation query
                    // This tells upstream (vapostproc) that we can handle VideoMeta
                    query.add_allocation_meta::<VideoMeta>(None);
                    true
                })
                .build(),
        );

        // Add elements to pipeline
        pipeline
            .add_many([
                &filesrc,
                &decodebin,
                &vapostproc,
                &capsfilter,
                appsink.upcast_ref(),
            ])
            .map_err(|e| VideoBackgroundError::Pipeline(format!("add_many: {}", e)))?;

        // Link static elements
        filesrc
            .link(&decodebin)
            .map_err(|e| VideoBackgroundError::Pipeline(format!("link filesrc: {}", e)))?;
        vapostproc
            .link(&capsfilter)
            .map_err(|e| VideoBackgroundError::Pipeline(format!("link vapostproc: {}", e)))?;
        capsfilter
            .link(&appsink)
            .map_err(|e| VideoBackgroundError::Pipeline(format!("link capsfilter: {}", e)))?;

        // Handle decodebin's dynamic pad
        let vapostproc_weak = vapostproc.downgrade();
        decodebin.connect_pad_added(move |_decodebin, src_pad| {
            let Some(vapostproc) = vapostproc_weak.upgrade() else {
                return;
            };

            // Only link video pads
            let caps = src_pad
                .current_caps()
                .or_else(|| Some(src_pad.query_caps(None)));
            if let Some(caps) = caps {
                if let Some(structure) = caps.structure(0) {
                    if !structure.name().starts_with("video/") {
                        return;
                    }
                }
            }

            if let Some(sink_pad) = vapostproc.static_pad("sink") {
                if !sink_pad.is_linked() {
                    if let Err(e) = src_pad.link(&sink_pad) {
                        warn!("Failed to link decodebin to vapostproc: {}", e);
                    } else {
                        info!("Linked decodebin to vapostproc for DMA-BUF pipeline");
                    }
                }
            }
        });

        warn!("DEBUG: Setting pipeline to PAUSED");
        pipeline
            .set_state(gst::State::Paused)
            .map_err(|e| VideoBackgroundError::Pipeline(format!("Failed to pause: {:?}", e)))?;

        warn!("DEBUG: Waiting for preroll (10 second timeout)");

        // Wait for preroll with timeout
        let (result, state, _) = pipeline.state(gst::ClockTime::from_seconds(10));

        if result.is_err() || state != gst::State::Paused {
            // Check bus for the actual error
            let error_msg = if let Some(bus) = pipeline.bus() {
                let mut msg = String::new();
                while let Some(bus_msg) = bus.pop_filtered(&[gst::MessageType::Error]) {
                    if let gst::MessageView::Error(err) = bus_msg.view() {
                        msg = format!("{}: {:?}", err.error(), err.debug());
                        break;
                    }
                }
                msg
            } else {
                String::new()
            };

            pipeline.set_state(gst::State::Null).ok();
            return Err(VideoBackgroundError::Pipeline(format!(
                "DMA-BUF pipeline failed to preroll: {:?}, state={:?}, error={}",
                result, state, error_msg
            )));
        }

        // Check for errors on the bus
        if let Some(bus) = pipeline.bus() {
            while let Some(msg) = bus.pop_filtered(&[gst::MessageType::Error]) {
                if let gst::MessageView::Error(err) = msg.view() {
                    pipeline.set_state(gst::State::Null).ok();
                    return Err(VideoBackgroundError::Pipeline(format!(
                        "DMA-BUF pipeline error: {}",
                        err.error()
                    )));
                }
            }
        }

        // Start playing
        pipeline
            .set_state(gst::State::Playing)
            .map_err(|e| VideoBackgroundError::Pipeline(format!("Failed to play: {:?}", e)))?;

        // Wait for first sample to verify DMA-BUF is working
        let sample = appsink.try_pull_sample(gst::ClockTime::from_mseconds(1000));

        let is_dmabuf = if let Some(ref sample) = sample {
            if let Some(buffer) = sample.buffer() {
                if let Some(memory) = buffer.memory(0) {
                    let is_dma = memory.is_memory_type::<gst_allocators::DmaBufMemory>()
                        || memory.is_memory_type::<gst_allocators::FdMemory>();
                    if is_dma {
                        if let Some(caps) = sample.caps() {
                            info!(caps = %caps, "DMA-BUF memory confirmed");
                        }
                    } else {
                        warn!("First sample is not DMA-BUF memory");
                    }
                    is_dma
                } else {
                    false
                }
            } else {
                false
            }
        } else {
            warn!("Failed to pull first sample within timeout");
            false
        };

        if !is_dmabuf {
            pipeline.set_state(gst::State::Null).ok();
            return Err(VideoBackgroundError::Pipeline(
                "Pipeline not outputting DMA-BUF memory".to_string(),
            ));
        }

        info!(path = %path_str, "Video pipeline created (VA-API DMA-BUF zero-copy)");

        Ok(OutputVideo {
            pipeline,
            appsink,
            current_frame: None,
            video_size: None,
            path: path.to_path_buf(),
            texture_buffer: None,
            needs_upload: false,
            shared_frame,
            frame_sequence: 0,
            uses_dmabuf: true,
        })
    }

    /// Create a CPU-based pipeline with multi-threaded videoconvert.
    fn create_cpu_pipeline(
        path_str: &str,
        path: &Path,
        shared_frame: SharedVideoFrame,
    ) -> Result<OutputVideo, VideoBackgroundError> {
        // Try VA-API pipeline first (full GPU acceleration)
        // - decodebin uses hardware decoder (vaav1dec, vavp9dec, etc.)
        // - vapostproc does GPU-based color conversion to BGRA
        // Falls back to CPU videoconvert if vapostproc not available
        //
        // sync=true: GStreamer controls timing based on PTS
        // max-buffers=1 + drop=true: always have latest frame ready
        let vaapi_pipeline_str = format!(
            "filesrc location=\"{}\" ! \
             decodebin ! \
             vapostproc ! \
             video/x-raw,format=BGRA ! \
             appsink name=sink sync=true max-buffers=1 drop=true",
            path_str.replace('"', "\\\"")
        );

        debug!(pipeline = %vaapi_pipeline_str, "Trying VA-API pipeline with vapostproc");

        // Try VA-API pipeline
        if let Ok(pipeline) = gst::parse::launch(&vaapi_pipeline_str) {
            if let Ok(pipeline) = pipeline.downcast::<gst::Pipeline>() {
                pipeline.set_state(gst::State::Paused).ok();
                let (result, state, _) = pipeline.state(gst::ClockTime::from_seconds(5));

                if result.is_ok() && state == gst::State::Paused {
                    // Check bus for errors
                    let mut has_error = false;
                    if let Some(bus) = pipeline.bus() {
                        while let Some(msg) = bus.pop_filtered(&[gst::MessageType::Error]) {
                            if let gst::MessageView::Error(err) = msg.view() {
                                warn!(
                                    error = %err.error(),
                                    debug = ?err.debug(),
                                    "VA-API pipeline startup error"
                                );
                                has_error = true;
                            }
                        }
                    }

                    if !has_error {
                        pipeline.set_state(gst::State::Playing).ok();

                        if let Some(appsink) = pipeline.by_name("sink") {
                            if let Ok(appsink) = appsink.downcast::<gst_app::AppSink>() {
                                info!(path = %path_str, "Video pipeline created (VA-API GPU accelerated)");
                                return Ok(OutputVideo {
                                    pipeline,
                                    appsink,
                                    current_frame: None,
                                    video_size: None,
                                    path: path.to_path_buf(),
                                    texture_buffer: None,
                                    needs_upload: false,
                                    shared_frame,
                                    frame_sequence: 0,
                                    uses_dmabuf: false,
                                });
                            }
                        }
                    }
                } else {
                    warn!(
                        ?result,
                        ?state,
                        "VA-API pipeline failed to reach PAUSED state"
                    );
                }

                // Clean up failed pipeline
                pipeline.set_state(gst::State::Null).ok();
            }
        }

        warn!("VA-API pipeline failed, falling back to CPU videoconvert");

        // Fallback: CPU-based color conversion
        // sync=true: GStreamer controls timing based on PTS
        // max-buffers=1: always have latest frame
        let num_threads = std::thread::available_parallelism()
            .map(|n| n.get().min(8))
            .unwrap_or(4);

        let cpu_pipeline_str = format!(
            "filesrc location=\"{}\" ! \
             decodebin ! \
             videoconvert n-threads={} ! \
             video/x-raw,format=BGRA ! \
             appsink name=sink sync=true max-buffers=1 drop=true",
            path_str.replace('"', "\\\""),
            num_threads
        );

        debug!(pipeline = %cpu_pipeline_str, "Creating CPU video pipeline");

        let pipeline = gst::parse::launch(&cpu_pipeline_str)?;
        let pipeline = pipeline
            .downcast::<gst::Pipeline>()
            .map_err(|_| VideoBackgroundError::Pipeline("Not a pipeline".to_string()))?;

        // First preroll by going to PAUSED state
        pipeline.set_state(gst::State::Paused)?;

        // Wait for PAUSED state (preroll complete) with timeout
        let (result, state, _pending) = pipeline.state(gst::ClockTime::from_seconds(10));
        if result.is_err() || state != gst::State::Paused {
            warn!(
                ?result,
                ?state,
                "Pipeline failed to preroll to PAUSED state"
            );
        } else {
            debug!("Pipeline prerolled to PAUSED state");
        }

        // Now start playing
        pipeline.set_state(gst::State::Playing)?;

        info!(path = %path_str, threads = num_threads, "Video pipeline created (CPU mode, 30fps limited, downscaled to 2560x1440)");

        // Get the appsink
        let appsink = pipeline
            .by_name("sink")
            .ok_or_else(|| VideoBackgroundError::Pipeline("AppSink not found".to_string()))?
            .downcast::<gst_app::AppSink>()
            .map_err(|_| VideoBackgroundError::Pipeline("Not an appsink".to_string()))?;

        Ok(OutputVideo {
            pipeline,
            appsink,
            current_frame: None,
            video_size: None,
            path: path.to_path_buf(),
            texture_buffer: None,
            needs_upload: false,
            shared_frame,
            frame_sequence: 0,
            uses_dmabuf: false,
        })
    }

    /// Poll for new video frames for all outputs.
    ///
    /// Should be called once per frame before rendering.
    /// Returns true if any new frame was received.
    pub fn poll_frames(&mut self) -> bool {
        let mut had_new_frame = false;

        for video in self.outputs.values_mut() {
            if Self::poll_frame_internal(video) {
                had_new_frame = true;
            }
        }

        if let Some(ref mut video) = self.default_video {
            if Self::poll_frame_internal(video) {
                had_new_frame = true;
            }
        }

        // Handle looping
        self.check_and_loop();

        had_new_frame
    }

    /// Poll for a new video frame.
    /// Returns true if a new frame was received.
    fn poll_frame_internal(video: &mut OutputVideo) -> bool {
        // Check pipeline state
        let state = video.pipeline.current_state();
        if state != gst::State::Playing {
            // Don't spam logs, just skip
            return false;
        }

        // Non-blocking pull - get frame if one is ready
        // With sync=true, GStreamer releases frames at their PTS,
        // so we get frames at the correct video timing
        let sample = match video.appsink.try_pull_sample(gst::ClockTime::ZERO) {
            Some(s) => s,
            None => return false,
        };

        let buffer = match sample.buffer() {
            Some(b) => b,
            None => return false,
        };

        let caps = match sample.caps() {
            Some(c) => c,
            None => return false,
        };

        // Get video info from caps
        let video_info = match gst_video::VideoInfo::from_caps(caps) {
            Ok(info) => info,
            Err(e) => {
                warn!("Failed to get video info: {}", e);
                return false;
            }
        };

        let width = video_info.width() as i32;
        let height = video_info.height() as i32;

        // Update video size if needed
        if video.video_size.is_none()
            || video
                .video_size
                .as_ref()
                .map(|s| s.w != width || s.h != height)
                .unwrap_or(false)
        {
            video.video_size = Some(Size::from((width, height)));
            video.texture_buffer = None; // Force texture recreation
            debug!(
                width,
                height,
                uses_dmabuf = video.uses_dmabuf,
                "Video dimensions updated"
            );
        }

        video.frame_sequence += 1;

        // Handle DMA-BUF or CPU path
        if video.uses_dmabuf {
            Self::handle_dmabuf_frame(video, buffer, &video_info, caps)
        } else {
            Self::handle_cpu_frame(video, buffer, width, height)
        }
    }

    /// Handle a DMA-BUF frame from VA-API.
    fn handle_dmabuf_frame(
        video: &mut OutputVideo,
        buffer: &gst::BufferRef,
        video_info: &gst_video::VideoInfo,
        _caps: &gst::CapsRef,
    ) -> bool {
        let width = video_info.width() as i32;
        let height = video_info.height() as i32;
        let n_planes = video_info.n_planes() as usize;

        // Debug logging for first few frames
        if video.frame_sequence <= 3 {
            warn!(
                width,
                height,
                n_planes,
                format = ?video_info.format(),
                n_memories = buffer.n_memory(),
                "DMA-BUF frame debug"
            );
        }

        // For DMA_DRM format, the number of planes comes from buffer memories, not video_info
        let actual_n_planes = if n_planes == 0 {
            // DMA_DRM format - use buffer memory count
            buffer.n_memory() as usize
        } else {
            n_planes
        };

        if actual_n_planes == 0 {
            warn!("No planes in DMA-BUF buffer");
            return false;
        }

        // For VA memory, we need to get the DMA-BUF fd from each plane
        // VA-API surfaces are stored as DMA-BUF internally
        let mut planes = Vec::with_capacity(actual_n_planes);

        for plane_idx in 0..actual_n_planes {
            let memory = match buffer.memory(plane_idx) {
                Some(m) => m,
                None => {
                    // NV12 has 2 planes but might be stored in single memory block
                    if plane_idx > 0 {
                        break;
                    }
                    warn!("No memory for plane {}", plane_idx);
                    return false;
                }
            };

            // Try to get DMA-BUF fd from the memory
            // VA memory implements the fd memory interface
            let raw_fd = if memory.is_memory_type::<gst_allocators::DmaBufMemory>() {
                // Direct DMA-BUF memory
                match memory.downcast_memory_ref::<gst_allocators::DmaBufMemory>() {
                    Some(dmabuf_mem) => dmabuf_mem.fd(),
                    None => {
                        warn!("Failed to downcast DmaBufMemory");
                        return false;
                    }
                }
            } else if memory.is_memory_type::<gst_allocators::FdMemory>() {
                // FD memory (includes VA memory)
                match memory.downcast_memory_ref::<gst_allocators::FdMemory>() {
                    Some(fd_mem) => fd_mem.fd(),
                    None => {
                        warn!("Failed to downcast FdMemory");
                        return false;
                    }
                }
            } else {
                // Not a DMA-BUF compatible memory type
                if video.frame_sequence <= 3 {
                    warn!("Buffer memory is not DMA-BUF/FD type");
                }
                return false;
            };

            // Duplicate the fd so we own it
            let owned_fd = unsafe {
                use std::os::fd::BorrowedFd;
                let borrowed = BorrowedFd::borrow_raw(raw_fd);
                match rustix::io::fcntl_dupfd_cloexec(borrowed, 0) {
                    Ok(new_fd) => new_fd,
                    Err(e) => {
                        warn!("Failed to duplicate DMA-BUF fd: {}", e);
                        return false;
                    }
                }
            };

            let offset = if actual_n_planes > 1 && n_planes > 0 {
                video_info.offset()[plane_idx] as u32
            } else {
                memory.offset() as u32
            };

            // For DMA_DRM format, we need to calculate stride from width
            // AR24 (ARGB8888) has 4 bytes per pixel
            let stride = if n_planes > 0 && video_info.stride()[plane_idx] > 0 {
                video_info.stride()[plane_idx] as u32
            } else {
                // Calculate stride assuming 4 bytes per pixel (ARGB)
                // Round up to 64-byte alignment as is common for GPUs
                (((width as u32) * 4 + 63) / 64) * 64
            };

            if video.frame_sequence <= 3 {
                warn!(
                    plane_idx,
                    offset,
                    stride,
                    mem_offset = memory.offset(),
                    mem_size = memory.size(),
                    "Plane info"
                );
            }

            planes.push(DmaBufPlane {
                fd: std::sync::Arc::new(owned_fd),
                offset,
                stride,
            });
        }

        if planes.is_empty() {
            warn!("No valid DMA-BUF planes found");
            return false;
        }

        // Map GStreamer format to DRM fourcc
        // For DMA_DRM format, we need to check the drm-format cap
        let fourcc = if video_info.format() == gst_video::VideoFormat::DmaDrm {
            // Extract drm-format from caps
            if let Some(structure) = _caps.structure(0) {
                let drm_format_result = structure.get::<&str>("drm-format");
                if video.frame_sequence <= 3 {
                    warn!(drm_format = ?drm_format_result, "Checking drm-format from caps");
                }
                if let Ok(drm_format) = drm_format_result {
                    match drm_format {
                        "AR24" | "XR24" => Fourcc::Argb8888,
                        "AB24" | "XB24" => Fourcc::Abgr8888,
                        "RA24" | "RX24" => Fourcc::Rgba8888,
                        "BA24" | "BX24" => Fourcc::Bgra8888,
                        "NV12" => Fourcc::Nv12,
                        "P010" => Fourcc::P010,
                        other => {
                            warn!(drm_format = other, "Unsupported DRM format");
                            return false;
                        }
                    }
                } else {
                    warn!("No drm-format in caps for DMA_DRM");
                    return false;
                }
            } else {
                warn!("No structure in DMA_DRM caps");
                return false;
            }
        } else {
            match video_info.format() {
                gst_video::VideoFormat::Nv12 => Fourcc::Nv12,
                gst_video::VideoFormat::P01010le => Fourcc::P010,
                gst_video::VideoFormat::Bgra => Fourcc::Argb8888,
                gst_video::VideoFormat::Argb => Fourcc::Bgra8888,
                gst_video::VideoFormat::Rgba => Fourcc::Abgr8888,
                gst_video::VideoFormat::Rgbx => Fourcc::Xbgr8888,
                gst_video::VideoFormat::Bgrx => Fourcc::Xrgb8888,
                other => {
                    warn!(format = ?other, "Unsupported video format for DMA-BUF");
                    return false;
                }
            }
        };

        // VA-API typically uses linear or tiled modifiers
        // For now assume linear - proper modifier detection would need VA-API queries
        let modifier = Modifier::Linear;

        let size = Size::from((width, height));

        if video.frame_sequence <= 3 || video.frame_sequence % 300 == 0 {
            warn!(
                format = ?video_info.format(),
                ?fourcc,
                width,
                height,
                n_planes = planes.len(),
                stride0 = planes[0].stride,
                frame = video.frame_sequence,
                "DMA-BUF frame SUCCESS"
            );
        }

        // Create DMA-BUF frame data
        let dmabuf_data = DmaBufFrameData {
            planes,
            format: fourcc,
            modifier,
            size,
        };

        // Update shared frame with DMA-BUF data
        video
            .shared_frame
            .update_dmabuf(dmabuf_data, video.frame_sequence);
        video.needs_upload = true;

        true
    }

    /// Handle a CPU frame (BGRA pixels).
    fn handle_cpu_frame(
        video: &mut OutputVideo,
        buffer: &gst::BufferRef,
        width: i32,
        height: i32,
    ) -> bool {
        // Map the buffer to read pixels
        let map = match buffer.map_readable() {
            Ok(m) => m,
            Err(e) => {
                warn!("Failed to map video buffer: {}", e);
                return false;
            }
        };

        let data = map.as_slice();
        let expected_size = (width * height * 4) as usize;

        if data.len() < expected_size {
            warn!(
                actual = data.len(),
                expected = expected_size,
                "Video buffer size mismatch"
            );
            return false;
        }

        // Copy frame data to local buffer for main thread use
        if video
            .current_frame
            .as_ref()
            .map(|f| f.len() != expected_size)
            .unwrap_or(true)
        {
            video.current_frame = Some(vec![0u8; expected_size]);
        }

        if let Some(ref mut frame) = video.current_frame {
            frame.copy_from_slice(&data[..expected_size]);
            video.needs_upload = true;

            // Update shared frame for KMS thread access (avoids extra clone)
            video.shared_frame.update_from_slice(
                &data[..expected_size],
                video.video_size.unwrap(),
                video.frame_sequence,
            );
            return true;
        }

        false
    }

    /// Get the texture buffer for an output, uploading new frame data if needed.
    ///
    /// Returns None if no video is configured or no frame is available.
    pub fn get_texture_buffer<R>(
        &mut self,
        renderer: &mut R,
        output_name: &str,
    ) -> Option<&TextureBuffer<GlesTexture>>
    where
        R: Renderer<TextureId = GlesTexture> + ImportMem + ImportDma,
    {
        let video = if let Some(v) = self.outputs.get_mut(output_name) {
            v
        } else {
            self.default_video.as_mut()?
        };

        // Check availability of new frame data
        let frame_data = video.shared_frame.get();

        // Upload new texture if needed
        if video.needs_upload || video.texture_buffer.is_none() {
            if let Some(data) = frame_data {
                match data.source {
                    VideoFrameSource::Cpu(pixels) => {
                        let size = data.size;
                        match renderer.import_memory(
                            &pixels,
                            smithay::backend::allocator::Fourcc::Argb8888,
                            (size.w, size.h).into(),
                            false,
                        ) {
                            Ok(texture) => {
                                video.texture_buffer = Some(TextureBuffer::from_texture(
                                    renderer,
                                    texture,
                                    1,
                                    Transform::Normal,
                                    None,
                                ));
                                video.needs_upload = false;
                                // debug!("Video texture uploaded (CPU)");
                            }
                            Err(e) => {
                                warn!("Failed to import video texture (CPU): {:?}", e);
                                return None;
                            }
                        }
                    }
                    VideoFrameSource::DmaBuf(dmabuf) => {
                        let mut dmabuf_obj = smithay::backend::allocator::dmabuf::Dmabuf::builder(
                            (dmabuf.size.w, dmabuf.size.h),
                            dmabuf.format,
                            dmabuf.modifier,
                            smithay::backend::allocator::dmabuf::DmabufFlags::empty(),
                        );

                        for (i, plane) in dmabuf.planes.iter().enumerate() {
                            dmabuf_obj.add_plane(
                                plane.fd.try_clone().expect("Failed to dup fd"),
                                i as u32,
                                plane.offset,
                                plane.stride,
                            );
                        }

                        let dmabuf_obj = match dmabuf_obj.build() {
                            Some(d) => d,
                            None => {
                                warn!("Failed to build DMABuf object");
                                return None;
                            }
                        };

                        match renderer.import_dmabuf(
                            &dmabuf_obj,
                            None, // damage
                        ) {
                            Ok(texture) => {
                                video.texture_buffer = Some(TextureBuffer::from_texture(
                                    renderer,
                                    texture,
                                    1,
                                    Transform::Normal,
                                    None,
                                ));
                                video.needs_upload = false;
                                // debug!("Video texture uploaded (DMA-BUF)");
                            }
                            Err(e) => {
                                warn!("Failed to import video texture (DMA-BUF): {:?}", e);
                                return None;
                            }
                        }
                    }
                }
            } else if video.texture_buffer.is_none() {
                // No frame data and no texture
                return None;
            }
        }

        video.texture_buffer.as_ref()
    }

    /// Get a render element for an output's video background.
    ///
    /// This method uploads the frame if needed and returns a TextureRenderElement
    /// positioned at (0, 0) suitable for rendering as a background.
    ///
    /// Returns None if no video is configured or no frame is available.
    pub fn get_render_element<R>(
        &mut self,
        renderer: &mut R,
        output_name: &str,
    ) -> Option<TextureRenderElement<GlesTexture>>
    where
        R: Renderer<TextureId = GlesTexture> + ImportMem + ImportDma,
    {
        // First ensure texture is uploaded
        let _ = self.get_texture_buffer(renderer, output_name)?;

        // Now get the video and create render element
        let video = if let Some(v) = self.outputs.get(output_name) {
            v
        } else {
            self.default_video.as_ref()?
        };

        let texture_buffer = video.texture_buffer.as_ref()?;

        // Create render element at position (0, 0)
        Some(TextureRenderElement::from_texture_buffer(
            Point::from((0.0, 0.0)),
            texture_buffer,
            None, // no alpha override
            None, // no src rect
            None, // no size override
            smithay::backend::renderer::element::Kind::Unspecified,
        ))
    }

    /// Get the video size for an output.
    pub fn video_size(&self, output_name: &str) -> Option<Size<i32, Physical>> {
        if let Some(v) = self.outputs.get(output_name) {
            v.video_size
        } else {
            self.default_video.as_ref()?.video_size
        }
    }

    /// Check if video wallpaper is configured for an output.
    pub fn has_video(&self, output_name: &str) -> bool {
        self.outputs.contains_key(output_name) || self.default_video.is_some()
    }

    /// Check if any video wallpaper is active (playing).
    pub fn is_active(&self) -> bool {
        !self.outputs.is_empty() || self.default_video.is_some()
    }

    /// Stop video for a specific output.
    pub fn stop_video(&mut self, output_name: &str) {
        if let Some(video) = self.outputs.remove(output_name) {
            let _ = video.pipeline.set_state(gst::State::Null);
            info!(output = output_name, "Video wallpaper stopped");
        }
    }

    /// Stop all videos.
    pub fn stop_all(&mut self) {
        for (name, video) in self.outputs.drain() {
            let _ = video.pipeline.set_state(gst::State::Null);
            debug!(output = name, "Video wallpaper stopped");
        }

        if let Some(video) = self.default_video.take() {
            let _ = video.pipeline.set_state(gst::State::Null);
            debug!("Default video wallpaper stopped");
        }

        self.shared_frames.set_active(false);
    }

    /// Check pipeline state and handle EOS (end of stream) for looping.
    fn check_and_loop(&mut self) {
        for video in self.outputs.values_mut() {
            Self::check_and_loop_video(video);
        }

        if let Some(ref mut video) = self.default_video {
            Self::check_and_loop_video(video);
        }
    }

    fn check_and_loop_video(video: &mut OutputVideo) {
        // Check for EOS on the bus (non-blocking)
        if let Some(bus) = video.pipeline.bus() {
            while let Some(msg) = bus.pop() {
                match msg.view() {
                    gst::MessageView::Eos(_) => {
                        debug!(path = %video.path.display(), "Video reached end, looping");
                        // Seek back to start
                        let _ = video.pipeline.seek_simple(
                            gst::SeekFlags::FLUSH | gst::SeekFlags::KEY_UNIT,
                            gst::ClockTime::ZERO,
                        );
                    }
                    gst::MessageView::Error(err) => {
                        error!(
                            path = %video.path.display(),
                            error = %err.error(),
                            debug = ?err.debug(),
                            "Video playback error"
                        );
                    }
                    _ => {}
                }
            }
        }
    }
}

impl Default for VideoBackgroundManager {
    fn default() -> Self {
        Self::new()
    }
}

impl Drop for VideoBackgroundManager {
    fn drop(&mut self) {
        self.stop_all();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_manager_creation() {
        let manager = VideoBackgroundManager::new();
        assert!(!manager.initialized);
        assert!(manager.outputs.is_empty());
        assert!(manager.default_video.is_none());
    }
}
