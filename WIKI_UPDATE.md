# Wiki Update: Nu Rendering Page

## Summary

Created a comprehensive Nu Rendering wiki page documenting the engine's rendering capabilities.

## Changes Made

### 1. Created Nu-Rendering.md Wiki Page

Location: `Nu.wiki/Nu-Rendering.md`

The page includes:
- Overview of Nu's rendering capabilities
- 2D rendering features (sprites, text, Spine animation, layering)
- 3D rendering features (PBR, deferred rendering, models, instancing, terrain)
- Advanced effects documentation:
  - Shadows (CSM, point/spot light shadows)
  - SSAO (Screen-Space Ambient Occlusion)
  - SSRL (Screen-Space Reflections)
  - SSRR (Screen-Space Refraction)
  - SSVF (Screen-Space Volumetric Fog)
  - SSS (Subsurface Scattering)
  - Bloom (physically-based with Karis average)
  - Tone mapping (AgX, ACES, Reinhard, Uncharted)
  - FXAA
  - Fog (linear and exponential)
- Detailed documentation of configuration types:
  - `Renderer3dConfig` - Global feature toggles
  - `Lighting3dConfig` - Scene lighting parameters
- How to disable features in the editor (Gaia)
- How to disable features in code:
  - Runtime configuration with `World.configureRenderer3d`
  - Entity-based lighting configuration with `Lighting3dConfigDispatcher`
  - Direct message enqueueing
- Configuration via App.config
- Default values for all rendering features
- Performance considerations and recommendations
- Code examples for common scenarios

### 2. Updated Home.md

Added link to the new Nu Rendering page in the introductory material section, placed between "Playing Music and Sound" and "Skinning Entities with Overlays" for logical flow.

## Wiki Repository Commit

The changes have been committed to the Nu.wiki repository:
- Commit: fb6dd3f
- Message: "Add Nu Rendering wiki page with comprehensive rendering documentation"

**Note:** The commit was created locally but could not be pushed due to lack of GitHub credentials in the sandboxed environment. The repository maintainer will need to push these changes manually or they will be automatically synced when accessed through GitHub's wiki interface.

## Verification

The wiki page can be viewed at:
https://github.com/bryanedds/Nu/wiki/Nu-Rendering

(After the changes are pushed)

## Technical Details

Information was gathered from:
- `/Nu/Nu/Render/Renderer3d.fs` - 3D renderer implementation
- `/Nu/Nu/Render/Renderer2d.fs` - 2D renderer implementation
- `/Nu/Nu/World/WorldRender.fs` - World rendering functions
- `/Nu/Nu/Core/Constants.fs` - Rendering constants and defaults
- `/Nu/Nu.Gaia/Gaia.fs` - Editor rendering configuration
- Existing wiki pages for style and format consistency
