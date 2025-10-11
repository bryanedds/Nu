# Instructions for Adding TileMap Wiki Page to GitHub Wiki

This repository contains a comprehensive wiki page about TileMap and Custom Properties support in Nu Engine.

## File Location
- **Wiki Page**: `TileMap-and-Custom-Properties.md`

## How to Add to GitHub Wiki

### Option 1: Using GitHub Web Interface (Recommended)

1. Go to the [Nu repository Wiki](https://github.com/bryanedds/Nu/wiki)
2. Click "Create new page" or edit an existing page
3. Set the page title to: **TileMap and Custom Properties**
4. Copy the entire contents of `TileMap-and-Custom-Properties.md`
5. Paste it into the wiki editor
6. Click "Save Page"

### Option 2: Using Git Clone of Wiki

GitHub wikis are git repositories. You can clone and commit to them directly:

```bash
# Clone the wiki repository
git clone https://github.com/bryanedds/Nu.wiki.git

# Copy the wiki page
cp TileMap-and-Custom-Properties.md Nu.wiki/TileMap-and-Custom-Properties.md

# Commit and push
cd Nu.wiki
git add TileMap-and-Custom-Properties.md
git commit -m "Add TileMap and Custom Properties documentation"
git push origin master
```

## Page Content Overview

The wiki page covers:

- **Overview** - Introduction to Tiled integration
- **Creating Tile Maps** - Basic workflow
- **Facets and Dispatchers** - TileMapFacet vs TmxMapFacet
- **Custom Properties Reference**:
  - `C` - Collision property with all shape options
  - `A` - Animation property
  - `I` - Info property (user-defined)
  - `E` - Elevation property (layers)
  - `Image` - TileSet image asset property
- **Physics and Collision** - How collision shapes are generated
- **Rendering** - Parallax, culling, and visual effects
- **Code Architecture** - Key modules and functions
- **Examples** - Practical code examples
- **Best Practices** - Tips for effective usage
- **Known Issues** - TiledSharp compression bug (Issue #582)
- **Additional Resources** - Links to Tiled docs and example projects

## Related Code Files

The documentation is based on these source files:
- `Nu/Nu/World/WorldTmxMap.fs` - Core TmxMap functions
- `Nu/Nu/World/WorldFacets.fs` - TileMapFacet and TmxMapFacet
- `Nu/Nu/World/WorldDispatchers.fs` - TileMapDispatcher and TmxMapDispatcher
- `Nu/Nu/Core/Constants.fs` - Custom property constants
- `Nu/Nu/AssetGraph/AssetTypes.fs` - TileSet extensions

## Maintenance

When updating the wiki page:
1. Edit `TileMap-and-Custom-Properties.md` in this repository
2. Commit changes with a descriptive message
3. Copy updated content to GitHub Wiki
4. Keep both versions in sync

This ensures the documentation is version-controlled and easy to maintain.
