# TileMap and Custom Properties in Nu Engine

This guide explains how Nu Engine integrates with [Tiled Map Editor](https://www.mapeditor.org/) and how to use custom properties to control tile behavior.

## Overview

Nu Engine uses the **TiledSharp** library to load and render tile maps created in Tiled. Tile maps in Nu support various custom properties that allow you to define collision shapes, animations, elevation offsets, and custom metadata for individual tiles or layers.

## Creating Tile Maps in Tiled

1. Create your tile map in [Tiled Map Editor](https://www.mapeditor.org/)
2. Export it as a `.tmx` file
3. Place the `.tmx` file in your Nu project's `Assets` directory
4. Reference it in your game using a `TileMap AssetTag`

## TileMap Facets and Dispatchers

Nu provides two main ways to use tile maps:

### TileMapFacet
- **Purpose**: For asset-defined tile maps
- **Asset-based**: Loads tile maps from TMX asset files
- **Properties**: 
  - `TileMap` - The asset tag reference to the TMX file
  - `TileLayerClearance` - Spacing between layers (default: 2.0f)
  - `TileSizeDivisor` - Divides tile size for scaling (default: 1)
  - `TileIndexOffset` - Offset for tile indices
  - `TileIndexOffsetRange` - Range of tiles to apply offset to
  - `BodyEnabled` - Enable/disable physics body
  - `Friction`, `Restitution` - Physics properties
  - `CollisionCategories`, `CollisionMask` - Collision filtering

### TmxMapFacet
- **Purpose**: For user-defined tile maps
- **Runtime-defined**: Create and manipulate TmxMap objects programmatically
- **Same properties** as TileMapFacet
- **Use case**: Dynamic tile map generation or modification

### Dispatchers
- **TileMapDispatcher**: Entity dispatcher that includes TileMapFacet
- **TmxMapDispatcher**: Entity dispatcher that includes TmxMapFacet

## Custom Properties Reference

Nu recognizes several custom properties that can be added to tiles and layers in Tiled. These properties are **engine-defined** and have specific meanings within Nu.

### Tile Properties

These properties are added to individual tiles in a tileset:

#### `C` - Collision Property
**Property Name**: `C` (defined in `Constants.TileMap.CollisionPropertyName`)

Defines the collision shape for a tile. This is the most commonly used property.

**Values**:
- **Empty string `""`**: Creates a full tile box collider
- **`"Top"`**: Creates a half-height box on the top half of the tile
- **`"Bottom"`**: Creates a half-height box on the bottom half of the tile
- **`"Left"`**: Creates a half-width box on the left half of the tile
- **`"Right"`**: Creates a half-width box on the right half of the tile
- **`"TopLeft"`**: Creates a quarter-size box in the top-left corner
- **`"TopRight"`**: Creates a quarter-size box in the top-right corner
- **`"BottomLeft"`**: Creates a quarter-size box in the bottom-left corner
- **`"BottomRight"`**: Creates a quarter-size box in the bottom-right corner
- **Custom BodyShape**: You can provide a serialized `BodyShape` expression for complex collision shapes

**Example in Tiled**:
```xml
<tile id="0">
  <properties>
    <property name="C" value=""/>
  </properties>
</tile>
```

**Optimization**: Adjacent tiles with full collision (empty `C` value) on the same row are automatically combined into horizontal strips for better physics performance.

#### `A` - Animation Property
**Property Name**: `A` (defined in `Constants.TileMap.AnimationPropertyName`)

Defines animated tile behavior using a `TileAnimationDescriptor`.

**Format**: Serialized `TileAnimationDescriptor` object

**Usage**: Allows tiles to animate by cycling through different tile indices over time.

#### `I` - Info Property
**Property Name**: `I` (defined in `Constants.TileMap.InfoPropertyName`)

Reserved for user-defined metadata. The engine defines this constant but **does not use it internally**. You can use this property to store any custom information about tiles that your game logic needs to access.

**Use cases**:
- Tile type identifiers
- Gameplay properties (damage, healing, etc.)
- Spawn point markers
- Custom tile behavior flags

### Layer Properties

These properties are added to entire tile layers:

#### `E` - Elevation Property
**Property Name**: `E` (defined in `Constants.TileMap.ElevationPropertyName`)

Specifies the rendering elevation offset for a layer.

**Format**: Numeric value (float)

**Behavior**:
- If not specified, layers are automatically spaced using: `layerIndex * TileLayerClearance`
- When specified, the elevation is: `baseElevation + E_value`
- Allows precise control over layer depth ordering

**Example in Tiled**:
```xml
<layer id="1" name="Background" width="32" height="13">
  <properties>
    <property name="E" value="0.0"/>
  </properties>
  ...
</layer>
```

## TileSet Image Property

### `Image` - TileSet Image Asset
**Property Name**: `Image` (TileSet custom property)

Specifies the image asset to use for a tileset.

**Format**: `[PackageName AssetName]`

**Behavior**:
- If not specified, Nu attempts to infer the image asset from the tileset's image source filename
- If inference fails, a `TileSetPropertyNotFoundException` is thrown
- This property ensures the correct image asset is loaded, especially when asset names differ from file names

**Example in Tiled**:
Add a custom property to the tileset:
```
Property Name: Image
Property Type: string
Property Value: [Default RoadTileSet]
```

## Physics and Collision

TileMaps in Nu automatically generate static physics bodies based on the `C` property:

1. **Tile Collision Detection**: Each tile with a `C` property generates a collision shape
2. **Shape Optimization**: Adjacent full-tile boxes are merged into strips
3. **Static Bodies**: TileMap bodies are always static (non-moving)
4. **Collision Categories**: Use `CollisionCategories` and `CollisionMask` to filter collisions

**Example: Setting up collision filtering**
```fsharp
entity.SetCollisionCategories "1" world
entity.SetCollisionMask "2,3,4" world  // Collides with categories 2, 3, and 4
```

## Rendering

TileMaps are rendered with:
- **View culling**: Only visible tiles are rendered
- **Layer support**: Multiple layers with parallax scrolling
- **Color tinting**: Apply color and emission via entity properties
- **Clipping**: Optional clipping region support

**Parallax Scrolling**: Layers automatically respect Tiled's parallax X/Y properties for depth effects.

## Code Architecture

### Key Modules

#### `WorldTmxMap.fs`
Contains the core TmxMap functionality:
- `TmxMap.makeFromFilePath` - Load from file
- `TmxMap.makeFromText` - Load from string
- `TmxMap.makeFromStream` - Load from stream
- `TmxMap.getDescriptor` - Get tile map descriptor
- `TmxMap.getBodyProperties` - Generate physics body properties
- `TmxMap.getLayeredMessages2d` - Generate rendering messages
- `TmxMap.getTileLayerBodyShapes` - Extract collision shapes from tiles

#### `WorldFacets.fs`
Defines `TileMapFacet` and `TmxMapFacet`:
- Handles entity registration and physics setup
- Manages tile map rendering
- Auto-calculates entity bounds from tile map size

#### `WorldDispatchers.fs`
Provides ready-to-use dispatchers:
- `TileMapDispatcher` - Asset-based tile maps
- `TmxMapDispatcher` - User-defined tile maps

## Examples

### Example 1: Basic TileMap Entity (Asset-based)

```fsharp
// In your entity definition
Content.entity<TileMapDispatcher> "TileMap"
    [Entity.TileMap := Assets.Gameplay.MyTileMap
     Entity.TileLayerClearance := 2.0f
     Entity.Position := v3 0.0f 0.0f 0.0f
     Entity.Elevation := 0.0f]
```

### Example 2: Reading Custom Tile Properties

```fsharp
// Access tile information at runtime
let tileMap = entity.GetTmxMap world
let tileLayer = tileMap.TileLayers |> Seq.head
let tileIndex = 0
let tile = tileLayer.Tiles.[tileIndex]

// Get the tile from the tileset
let gid = tile.Gid
let tileset = tileMap.Tilesets |> Seq.find (fun ts -> ts.FirstGid <= gid)
let tileId = gid - tileset.FirstGid
let tilesetTile = tileset.Tiles.[tileId]

// Read custom "I" property
match tilesetTile.Properties.TryGetValue "I" with
| (true, infoValue) -> 
    // Use your custom info value
    printfn "Tile info: %s" infoValue
| (false, _) -> ()
```

### Example 3: Using TileSizeDivisor for Scaling

```fsharp
// Scale down the tile map by 2x
Content.entity<TileMapDispatcher> "TileMap"
    [Entity.TileMap := Assets.Gameplay.MyTileMap
     Entity.TileSizeDivisor := 2  // Each 64x64 tile becomes 32x32]
```

## Best Practices

1. **Use Empty Collision Strings**: For simple full-tile collision, use an empty string for the `C` property to allow optimization
2. **Layer Naming**: Use descriptive layer names in Tiled - they don't affect functionality but help with organization
3. **Tile Grouping**: Group tiles with similar collision properties to make property assignment easier
4. **Custom Info**: Use the `I` property consistently across your project for custom tile metadata
5. **Compression**: See the note below about TiledSharp performance issues with compression

## Known Issues

### ⚠️ TiledSharp Compression Performance Bug (Issue #582)

**Problem**: TiledSharp's base64 + zlib decompression can be extremely slow on some machines (particularly newer hardware) when loading large maps.

**Symptoms**: Long load times when opening maps with base64/zlib compression.

**Workaround**: Disable compression in Tiled when exporting large maps:

1. In Tiled, go to **Map → Map Properties**
2. Set **Tile Layer Format** to **Base64 (uncompressed)** instead of **Base64 (zlib compressed)**

![Tiled compression setting](https://github.com/bryanedds/Nu/assets/1625560/8e75de61-47bb-4bf8-a847-f9689ce3357c)

**Status**: This is a known issue with the TiledSharp library. See [Issue #582](https://github.com/bryanedds/Nu/issues/582) for details.

## Additional Resources

- [Tiled Map Editor Documentation](https://doc.mapeditor.org/)
- [Nu Engine Documentation](https://github.com/bryanedds/Nu)
- Example games using TileMaps:
  - **Blaze Vector** - Located in `Projects/Blaze Vector Mmcc/`
  - **OmniBlade** - Check the Nu repository for OmniBlade examples

## Summary

Nu's TileMap system provides a powerful integration with Tiled Map Editor:

- **Custom Properties**: `C` (Collision), `A` (Animation), `E` (Elevation), `I` (Info)
- **Automatic Physics**: Collision shapes generated from tile properties
- **Flexible Rendering**: Layer support, parallax, culling, and color tinting
- **Two Facet Types**: Asset-based (TileMapFacet) or runtime (TmxMapFacet)
- **Performance**: Adjacent tiles are optimized into strips

Start by creating simple tile maps with collision properties, then explore advanced features like animations, custom info, and layer elevation as your needs grow!
