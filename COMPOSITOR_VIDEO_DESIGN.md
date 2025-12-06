# Compositor-Internal Video Wallpaper Design

## Executive Summary

This document outlines the architecture for video wallpapers by moving video decoding directly into `cosmic-comp`. This eliminates Wayland protocol overhead and simplifies the architecture.

## Current Architecture (cosmic-bg)

```
cosmic-bg Process                         cosmic-comp Process
├─────────────────────────────────────────────────────────────────┤
│                                         │                       │
│  GStreamer Pipeline                     │   Compositor          │
│  ┌──────────────────────────────────┐   │   ┌─────────────────┐ │
│  │ nvh264dec → glupload → gldownload│   │   │                 │ │
│  │              ↓                    │   │   │   wl_surface    │ │
│  │         [CPU pixels] ─────────────────────→ import buffer  │ │
│  │                                   │   │   │   render        │ │
│  └──────────────────────────────────┘   │   └─────────────────┘ │
│                                         │                       │
│  ~4-5ms per frame (4K60fps)             │                       │
├─────────────────────────────────────────────────────────────────┤
```

**Bottlenecks:**
1. `gldownload` - GPU→CPU copy (~2ms)
2. Wayland protocol - buffer attach, damage, commit
3. cosmic-comp import - CPU→GPU texture upload

## Proposed Architecture (Compositor-Internal)

```
cosmic-comp Process
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  GStreamer Pipeline                                              │
│  ┌──────────────────────────────────────────────────────────┐   │
│  │ filesrc → decodebin → videoconvert → BGRA → appsink      │   │
│  │                                        ↓                  │   │
│  │                                 [CPU pixels]              │   │
│  │                                        │                  │   │
│  └────────────────────────────────────────│──────────────────┘   │
│                                           ↓                      │
│  Compositor Rendering                                            │
│  ┌─────────────────────────────────────────────────────────┐    │
│  │                                                          │    │
│  │  workspace_elements()                                    │    │
│  │      ...all other elements...                            │    │
│  │      VideoBackground (at end = behind everything)        │    │
│  │          └─→ TextureRenderElement::from_texture_buffer   │    │
│  │                                                          │    │
│  └─────────────────────────────────────────────────────────┘    │
│                                                                  │
│  Benefits: No Wayland protocol, simpler architecture             │
├─────────────────────────────────────────────────────────────────┤
```

**Benefits:**
1. **No Wayland protocol overhead** - Direct texture upload
2. **Simpler architecture** - No IPC, no buffer management
3. **Easier configuration** - Video path in compositor config
4. **Hardware decode** - decodebin uses nvdec/vaapi when available

## Implementation Details

### 1. VideoBackgroundManager

Located in `src/shell/video_background.rs`:

```rust
pub struct VideoBackgroundManager {
    outputs: HashMap<String, OutputVideo>,
    default_video: Option<OutputVideo>,
    initialized: bool,
}

impl VideoBackgroundManager {
    pub fn new() -> Self;
    pub fn init() -> Result<(), VideoBackgroundError>;
    pub fn set_video_for_output(&mut self, output: &str, path: &Path) -> Result<()>;
    pub fn set_default_video(&mut self, path: &Path) -> Result<()>;
    pub fn poll_frames(&mut self);  // Call once per frame
    pub fn get_texture_buffer<R>(&mut self, renderer: &mut R, output: &str) 
        -> Option<&TextureBuffer<GlesTexture>>;
}
```

### 2. GStreamer Pipeline

```
filesrc location={path} ! 
decodebin ! 
videoconvert ! 
video/x-raw,format=BGRA ! 
appsink name=sink emit-signals=true sync=true max-buffers=1 drop=true
```

- `decodebin` automatically selects hardware decoders (nvdec, vaapi)
- `videoconvert` handles format conversion
- `appsink` provides CPU-accessible frames

### 3. Render Pipeline Integration

In `workspace_elements()` in `render/mod.rs`, after all stages are processed:

```rust
// Add video background at the end (rendered behind everything)
#[cfg(feature = "video-wallpaper")]
if let Some(ref mut video_manager) = state.video_background {
    video_manager.poll_frames();
    let output_name = output.name();
    if let Some(texture_buffer) = video_manager.get_texture_buffer(renderer, &output_name) {
        elements.push(
            TextureRenderElement::from_texture_buffer(
                (0, 0).into(),
                texture_buffer,
                None, // alpha
                None, // src rect
                None, // size
                Kind::Unspecified,
            ).into()
        );
    }
}
```

## Cargo.toml Dependencies

```toml
[features]
video-wallpaper = ["gstreamer", "gstreamer-video", "gstreamer-app"]

[dependencies]
gstreamer = { version = "0.23", optional = true }
gstreamer-video = { version = "0.23", optional = true }
gstreamer-app = { version = "0.23", optional = true }
```

## Performance Expectations

| Metric | cosmic-bg | Compositor-internal |
|--------|-----------|---------------------|
| Wayland IPC | Yes | No |
| Frame latency | ~4-5ms | ~3-4ms (estimate) |
| Architecture | Complex | Simpler |
| Config integration | Separate | Unified |

## Configuration

Option 1: Extend cosmic-bg-config with `Source::Video(PathBuf)`
Option 2: New cosmic-comp config key for video wallpapers

Recommended: Option 1 for consistency with existing wallpaper config.

## Next Steps

1. ✅ Design document (this file)
2. ✅ Add feature-gated GStreamer dependencies
3. ✅ Implement VideoBackgroundManager module
4. ✅ Integrate with render pipeline (CosmicElement::VideoBackground variant)
5. ✅ Add to State struct (Common.video_background)
6. ✅ Environment variable config (COSMIC_VIDEO_WALLPAPER)
7. ⬜ Test with video file
8. ⬜ Add cosmic-config integration
9. ⬜ Benchmark and compare with cosmic-bg

## Testing

To test video wallpapers with the winit backend:

```bash
# Build with video-wallpaper feature
cargo build --features video-wallpaper

# Run with a test video
COSMIC_VIDEO_WALLPAPER=/path/to/video.mp4 ./target/debug/cosmic-comp --winit

# Or export the variable
export COSMIC_VIDEO_WALLPAPER=/path/to/video.mp4
cargo run --features video-wallpaper -- --winit
```

**Note:** The video background is rendered behind all layer shells, including cosmic-bg.
If you have cosmic-bg running with a static wallpaper, you won't see the video.
For testing, either disable cosmic-bg or use a transparent wallpaper.
