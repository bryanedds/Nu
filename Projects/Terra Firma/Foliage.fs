namespace TerraFirma
open System
open System.Collections.Generic
open System.Numerics
open Prime
open Nu

/// A cell of generated tufts.
type [<ReferenceEquality>] FoliageCell =
    { Bounds : Box3
      Items : StaticModelSurfacePreBatchItem array
      PreBatchIds : Guid array } // by surface index

/// The generated state of a foliage layer, covering its entire terrain.
type [<ReferenceEquality>] FoliageField =
    { TerrainBounds : Box3
      Resolution : Vector2i
      Positions : Vector3 array
      Normals : Vector3 array
      Variation : Vector4 array // tileable variation texture - RG = lean, B = density, A = height
      ControlOpt : (Vector2i * Vector4 array) option // control map - RG = lean, B = density, A = height
      ModelBounds : Box3
      SurfaceCount : int
      Cells : FoliageCell array }

[<AutoOpen>]
module FoliageFacetExtensions =
    type Entity with
        member this.GetFoliageControlImageOpt world : Image AssetTag option = this.Get (nameof this.FoliageControlImageOpt) world
        member this.SetFoliageControlImageOpt (value : Image AssetTag option) world = this.Set (nameof this.FoliageControlImageOpt) value world
        member this.FoliageControlImageOpt = lens (nameof this.FoliageControlImageOpt) this this.GetFoliageControlImageOpt this.SetFoliageControlImageOpt
        member this.GetFoliageTerrain world : Entity Address = this.Get (nameof this.FoliageTerrain) world
        member this.SetFoliageTerrain (value : Entity Address) world = this.Set (nameof this.FoliageTerrain) value world
        member this.FoliageTerrain = lens (nameof this.FoliageTerrain) this this.GetFoliageTerrain this.SetFoliageTerrain
        member this.GetFoliageDensity world : single = this.Get (nameof this.FoliageDensity) world
        member this.SetFoliageDensity (value : single) world = this.Set (nameof this.FoliageDensity) value world
        member this.FoliageDensity = lens (nameof this.FoliageDensity) this this.GetFoliageDensity this.SetFoliageDensity
        member this.GetFoliageCellSize world : single = this.Get (nameof this.FoliageCellSize) world
        member this.SetFoliageCellSize (value : single) world = this.Set (nameof this.FoliageCellSize) value world
        member this.FoliageCellSize = lens (nameof this.FoliageCellSize) this this.GetFoliageCellSize this.SetFoliageCellSize
        member this.GetFoliageDrawDistance world : single = this.Get (nameof this.FoliageDrawDistance) world
        member this.SetFoliageDrawDistance (value : single) world = this.Set (nameof this.FoliageDrawDistance) value world
        member this.FoliageDrawDistance = lens (nameof this.FoliageDrawDistance) this this.GetFoliageDrawDistance this.SetFoliageDrawDistance
        member this.GetFoliageCollapseStart world : single = this.Get (nameof this.FoliageCollapseStart) world
        member this.SetFoliageCollapseStart (value : single) world = this.Set (nameof this.FoliageCollapseStart) value world
        member this.FoliageCollapseStart = lens (nameof this.FoliageCollapseStart) this this.GetFoliageCollapseStart this.SetFoliageCollapseStart
        member this.GetFoliageRoughenEnd world : single = this.Get (nameof this.FoliageRoughenEnd) world
        member this.SetFoliageRoughenEnd (value : single) world = this.Set (nameof this.FoliageRoughenEnd) value world
        member this.FoliageRoughenEnd = lens (nameof this.FoliageRoughenEnd) this this.GetFoliageRoughenEnd this.SetFoliageRoughenEnd
        member this.GetFoliageHeightRange world : Vector2 = this.Get (nameof this.FoliageHeightRange) world
        member this.SetFoliageHeightRange (value : Vector2) world = this.Set (nameof this.FoliageHeightRange) value world
        member this.FoliageHeightRange = lens (nameof this.FoliageHeightRange) this this.GetFoliageHeightRange this.SetFoliageHeightRange
        member this.GetFoliageWidth world : single = this.Get (nameof this.FoliageWidth) world
        member this.SetFoliageWidth (value : single) world = this.Set (nameof this.FoliageWidth) value world
        member this.FoliageWidth = lens (nameof this.FoliageWidth) this this.GetFoliageWidth this.SetFoliageWidth
        member this.GetFoliageLeanMax world : single = this.Get (nameof this.FoliageLeanMax) world
        member this.SetFoliageLeanMax (value : single) world = this.Set (nameof this.FoliageLeanMax) value world
        member this.FoliageLeanMax = lens (nameof this.FoliageLeanMax) this this.GetFoliageLeanMax this.SetFoliageLeanMax
        member this.GetFoliageTerrainLean world : single = this.Get (nameof this.FoliageTerrainLean) world
        member this.SetFoliageTerrainLean (value : single) world = this.Set (nameof this.FoliageTerrainLean) value world
        member this.FoliageTerrainLean = lens (nameof this.FoliageTerrainLean) this this.GetFoliageTerrainLean this.SetFoliageTerrainLean
        member this.GetFoliageSlopeFade world : Vector2 = this.Get (nameof this.FoliageSlopeFade) world
        member this.SetFoliageSlopeFade (value : Vector2) world = this.Set (nameof this.FoliageSlopeFade) value world
        member this.FoliageSlopeFade = lens (nameof this.FoliageSlopeFade) this this.GetFoliageSlopeFade this.SetFoliageSlopeFade
        member this.GetFoliageVariationTileSize world : single = this.Get (nameof this.FoliageVariationTileSize) world
        member this.SetFoliageVariationTileSize (value : single) world = this.Set (nameof this.FoliageVariationTileSize) value world
        member this.FoliageVariationTileSize = lens (nameof this.FoliageVariationTileSize) this this.GetFoliageVariationTileSize this.SetFoliageVariationTileSize
        member this.GetFoliageDensityVariation world : single = this.Get (nameof this.FoliageDensityVariation) world
        member this.SetFoliageDensityVariation (value : single) world = this.Set (nameof this.FoliageDensityVariation) value world
        member this.FoliageDensityVariation = lens (nameof this.FoliageDensityVariation) this this.GetFoliageDensityVariation this.SetFoliageDensityVariation
        member this.GetFoliageHeightVariation world : single = this.Get (nameof this.FoliageHeightVariation) world
        member this.SetFoliageHeightVariation (value : single) world = this.Set (nameof this.FoliageHeightVariation) value world
        member this.FoliageHeightVariation = lens (nameof this.FoliageHeightVariation) this this.GetFoliageHeightVariation this.SetFoliageHeightVariation
        member this.GetFoliageColorVariation world : single = this.Get (nameof this.FoliageColorVariation) world
        member this.SetFoliageColorVariation (value : single) world = this.Set (nameof this.FoliageColorVariation) value world
        member this.FoliageColorVariation = lens (nameof this.FoliageColorVariation) this this.GetFoliageColorVariation this.SetFoliageColorVariation
        member this.GetFoliageSeed world : int = this.Get (nameof this.FoliageSeed) world
        member this.SetFoliageSeed (value : int) world = this.Set (nameof this.FoliageSeed) value world
        member this.FoliageSeed = lens (nameof this.FoliageSeed) this this.GetFoliageSeed this.SetFoliageSeed
        member this.GetFoliageWindDirection world : Vector2 = this.Get (nameof this.FoliageWindDirection) world
        member this.SetFoliageWindDirection (value : Vector2) world = this.Set (nameof this.FoliageWindDirection) value world
        member this.FoliageWindDirection = lens (nameof this.FoliageWindDirection) this this.GetFoliageWindDirection this.SetFoliageWindDirection
        member this.GetFoliageWindStrength world : single = this.Get (nameof this.FoliageWindStrength) world
        member this.SetFoliageWindStrength (value : single) world = this.Set (nameof this.FoliageWindStrength) value world
        member this.FoliageWindStrength = lens (nameof this.FoliageWindStrength) this this.GetFoliageWindStrength this.SetFoliageWindStrength
        member this.GetFoliageWindSpeed world : single = this.Get (nameof this.FoliageWindSpeed) world
        member this.SetFoliageWindSpeed (value : single) world = this.Set (nameof this.FoliageWindSpeed) value world
        member this.FoliageWindSpeed = lens (nameof this.FoliageWindSpeed) this this.GetFoliageWindSpeed this.SetFoliageWindSpeed
        member this.GetFoliageFlutterStrength world : single = this.Get (nameof this.FoliageFlutterStrength) world
        member this.SetFoliageFlutterStrength (value : single) world = this.Set (nameof this.FoliageFlutterStrength) value world
        member this.FoliageFlutterStrength = lens (nameof this.FoliageFlutterStrength) this this.GetFoliageFlutterStrength this.SetFoliageFlutterStrength
        member this.GetFoliageWindCycle world : single = this.Get (nameof this.FoliageWindCycle) world
        member this.SetFoliageWindCycle (value : single) world = this.Set (nameof this.FoliageWindCycle) value world
        member this.FoliageWindCycle = lens (nameof this.FoliageWindCycle) this this.GetFoliageWindCycle this.SetFoliageWindCycle
        member this.GetFoliageFieldOpt world : FoliageField option = this.Get (nameof this.FoliageFieldOpt) world
        member this.SetFoliageFieldOpt (value : FoliageField option) world = this.Set (nameof this.FoliageFieldOpt) value world
        member this.FoliageFieldOpt = lens (nameof this.FoliageFieldOpt) this this.GetFoliageFieldOpt this.SetFoliageFieldOpt

/// Augments an entity with wind-blown foliage covering a terrain.
/// NOTE: the optional control image is sampled across the terrain with its first row at the terrain's minimum z and
/// its first column at the terrain's minimum x. Its RG channels are a lean in x and z encoded like a normal map, B is
/// density, and A is height. It should be uncompressed, such as by giving its name a _u suffix.
type FoliageFacet () =
    inherit Facet (false, false, false)

    // attempt to get the terrain that this foliage covers
    static let tryGetTerrain (entity : Entity) world =
        match tryResolve (entity.GetFoliageTerrain world) entity with
        | Some terrain when terrain.GetExists world && terrain.Has<TerrainFacet> world -> Some terrain
        | Some _ | None -> None

    // attempt to read the pixels of an uncompressed image as colors in [0, 1]
    static let tryReadImage (image : Image AssetTag) =
        match Metadata.tryGetFilePath image with
        | Some filePath ->
            match Vulkan.TextureData.tryCreate false filePath with
            | Some textureData ->
                let metadata = textureData.Metadata
                let (compressed, bytes) = textureData.Bytes
                Vulkan.TextureData.destroy textureData
                if not compressed then
                    let scalar = 1.0f / single Byte.MaxValue
                    let pixels = Array.init (metadata.TextureWidth * metadata.TextureHeight) (fun i ->
                        v4 (single bytes[i * 4 + 2] * scalar) (single bytes[i * 4 + 1] * scalar) (single bytes[i * 4] * scalar) (single bytes[i * 4 + 3] * scalar))
                    Some (v2i metadata.TextureWidth metadata.TextureHeight, pixels)
                else Log.infoOnce ("Foliage image '" + scstring image + "' must be uncompressed."); None
            | None -> None
        | None -> None

    // compute the vertex normals of a terrain the same way the renderer does
    static let computeTerrainNormals (resolution : Vector2i) (positions : Vector3 array) normalImageOpt =
        let normalsOpt =
            match normalImageOpt with
            | Some normalImage ->
                match tryReadImage normalImage with
                | Some (imageResolution, pixels) when imageResolution = resolution ->
                    pixels
                    |> Array.map (fun pixel ->
                        let tangent = (v3 pixel.X pixel.Y pixel.Z * 2.0f - v3One).Normalized
                        v3 tangent.X tangent.Z -tangent.Y)
                    |> Some
                | Some _ | None -> None
            | None -> None
        match normalsOpt with
        | Some normals -> normals
        | None ->
            [|for y in 0 .. dec resolution.Y do
                for x in 0 .. dec resolution.X do
                    if x > 0 && y > 0 && x < dec resolution.X && y < dec resolution.Y then
                        let v  = positions[resolution.X * y + x]
                        let n  = positions[resolution.X * dec y + x]
                        let ne = positions[resolution.X * dec y + inc x]
                        let e  = positions[resolution.X * y + inc x]
                        let s  = positions[resolution.X * inc y + x]
                        let sw = positions[resolution.X * inc y + dec x]
                        let w  = positions[resolution.X * y + dec x]
                        let normalSum =
                            Vector3.Cross (ne - v, n - v) +
                            Vector3.Cross (e - v,  ne - v) +
                            Vector3.Cross (s - v,  e - v) +
                            Vector3.Cross (sw - v, s - v) +
                            Vector3.Cross (w - v,  sw - v) +
                            Vector3.Cross (n - v,  w - v)
                        normalSum.Normalized
                    else v3Up|]

    // generate a tileable texture of smooth noise with an independent pattern in each channel
    static let generateVariation (seed : int) =
        let resolution = Constants.Foliage.VariationResolution
        let period = Constants.Foliage.VariationPeriod
        let random = Random seed
        let gradients = Array.init (4 * period * period) (fun _ -> let a = random.NextSingle () * MathF.Tau in v2 (cos a) (sin a))
        let noise channel (x : single) (y : single) =
            let x0 = int (floor x)
            let y0 = int (floor y)
            let fx = x - single x0
            let fy = y - single y0
            let dot ix iy dx dy =
                let gradient = gradients[(channel * period + iy % period) * period + ix % period]
                gradient.X * dx + gradient.Y * dy
            let sx = fx * fx * fx * (fx * (fx * 6.0f - 15.0f) + 10.0f)
            let sy = fy * fy * fy * (fy * (fy * 6.0f - 15.0f) + 10.0f)
            let n0 = Math.Lerp (dot x0 y0 fx fy, dot (inc x0) y0 (fx - 1.0f) fy, sx)
            let n1 = Math.Lerp (dot x0 (inc y0) fx (fy - 1.0f), dot (inc x0) (inc y0) (fx - 1.0f) (fy - 1.0f), sx)
            Math.Lerp (n0, n1, sy)
        let scale = single period / single resolution
        Array.init (resolution * resolution) (fun i ->
            let x = (single (i % resolution) + 0.5f) * scale
            let y = (single (i / resolution) + 0.5f) * scale
            let channel c = Math.Clamp (noise c x y * 0.75f + 0.5f, 0.0f, 1.0f)
            v4 (channel 0) (channel 1) (channel 2) (channel 3))

    // sample the variation texture at a world position, wrapping it, with a second octave at a non-integer ratio
    // to hide repetition
    static let sampleVariation (variation : Vector4 array) (tileSize : single) (x : single) (z : single) =
        let resolution = Constants.Foliage.VariationResolution
        let sample (u : single) (v : single) =
            let u = u * single resolution - 0.5f
            let v = v * single resolution - 0.5f
            let u0 = int (floor u)
            let v0 = int (floor v)
            let fu = u - single u0
            let fv = v - single v0
            let texel (iu : int) (iv : int) =
                let iu = ((iu % resolution) + resolution) % resolution
                let iv = ((iv % resolution) + resolution) % resolution
                variation[iv * resolution + iu]
            Vector4.Lerp (Vector4.Lerp (texel u0 v0, texel (inc u0) v0, fu), Vector4.Lerp (texel u0 (inc v0), texel (inc u0) (inc v0), fu), fv)
        let tileSize = max tileSize 0.01f
        let octave0 = sample (x / tileSize) (z / tileSize)
        let octave1 = sample (x * Constants.Foliage.VariationOctaveRatio / tileSize) (z * Constants.Foliage.VariationOctaveRatio / tileSize)
        let mixed = octave0 * 0.65f + octave1 * 0.35f
        Vector4.Clamp ((mixed - v4Dup 0.5f) * 1.5f + v4Dup 0.5f, v4Zero, v4One)

    // sample the control image at a world position, defaulting to no lean, full density, and full height
    static let sampleControl (field : FoliageField) (x : single) (z : single) =
        match field.ControlOpt with
        | Some (resolution, pixels) ->
            let bounds = field.TerrainBounds
            let u = Math.Clamp ((x - bounds.Min.X) / bounds.Size.X, 0.0f, 1.0f) * single (dec resolution.X)
            let v = Math.Clamp ((z - bounds.Min.Z) / bounds.Size.Z, 0.0f, 1.0f) * single (dec resolution.Y)
            let u0 = min (int u) (resolution.X - 2 |> max 0)
            let v0 = min (int v) (resolution.Y - 2 |> max 0)
            let u1 = min (inc u0) (dec resolution.X)
            let v1 = min (inc v0) (dec resolution.Y)
            let fu = u - single u0
            let fv = v - single v0
            let texel iu iv = pixels[iv * resolution.X + iu]
            Vector4.Lerp (Vector4.Lerp (texel u0 v0, texel u1 v0, fu), Vector4.Lerp (texel u0 v1, texel u1 v1, fu), fv)
        | None -> v4 0.5f 0.5f 1.0f 1.0f

    // sample the terrain's height and interpolated normal at a world position, interpolating within the same
    // triangles that the renderer splits each quad into
    static let sampleTerrain (field : FoliageField) (x : single) (z : single) =
        let bounds = field.TerrainBounds
        let resolution = field.Resolution
        let gx = (x - bounds.Min.X) / bounds.Size.X * single (dec resolution.X)
        let gz = (z - bounds.Min.Z) / bounds.Size.Z * single (dec resolution.Y)
        let ix = int (floor gx) |> max 0 |> min (resolution.X - 2)
        let iz = int (floor gz) |> max 0 |> min (resolution.Y - 2)
        let fx = Math.Clamp (gx - single ix, 0.0f, 1.0f)
        let fz = Math.Clamp (gz - single iz, 0.0f, 1.0f)
        let i00 = resolution.X * iz + ix
        let i10 = i00 + 1
        let i01 = i00 + resolution.X
        let i11 = i01 + 1
        let (ia, ib, ic, wa, wb, wc) =
            if fx + fz <= 1.0f
            then (i00, i10, i01, 1.0f - fx - fz, fx, fz) // (x, z), (x + 1, z), (x, z + 1)
            else (i11, i10, i01, fx + fz - 1.0f, 1.0f - fz, 1.0f - fx) // (x + 1, z + 1), (x + 1, z), (x, z + 1)
        let height = field.Positions[ia].Y * wa + field.Positions[ib].Y * wb + field.Positions[ic].Y * wc
        let normal = (field.Normals[ia] * wa + field.Normals[ib] * wb + field.Normals[ic] * wc).Normalized
        struct (height, normal)

    // attempt to create a foliage field covering its terrain
    static let tryCreateField (entity : Entity) (terrain : Entity) world =

        // gather the inputs to generation, all up front since cells are generated in parallel
        let terrainBounds = terrain.GetBounds world
        let heightMap = terrain.GetHeightMap world
        let normalImageOpt = terrain.GetNormalImageOpt world
        let controlImageOpt = entity.GetFoliageControlImageOpt world
        let staticModel = entity.GetStaticModel world
        let materialProperties = entity.GetMaterialProperties world
        let castShadow = entity.GetCastShadow world
        let density = entity.GetFoliageDensity world
        let cellSize = max (entity.GetFoliageCellSize world) 1.0f
        let heightRange = entity.GetFoliageHeightRange world
        let width = entity.GetFoliageWidth world
        let leanMaxDegrees = entity.GetFoliageLeanMax world
        let terrainLean = entity.GetFoliageTerrainLean world
        let slopeFade = entity.GetFoliageSlopeFade world
        let variationTileSize = entity.GetFoliageVariationTileSize world
        let densityVariation = entity.GetFoliageDensityVariation world
        let heightVariation = entity.GetFoliageHeightVariation world
        let colorVariation = entity.GetFoliageColorVariation world
        let seed = entity.GetFoliageSeed world

        // attempt to create field
        match HeightMap.tryGetMetadata Metadata.tryGetFilePath terrainBounds v2One heightMap with
        | ValueSome heightMapMetadata ->
            match Metadata.tryGetStaticModelMetadata staticModel with
            | ValueSome model ->
                let resolution = heightMapMetadata.Resolution
                let positions = Array.map fst' heightMapMetadata.PositionsAndTexCoordses
                let field =
                    { TerrainBounds = terrainBounds
                      Resolution = resolution
                      Positions = positions
                      Normals = computeTerrainNormals resolution positions normalImageOpt
                      Variation = generateVariation seed
                      ControlOpt = Option.bind tryReadImage controlImageOpt
                      ModelBounds = model.Bounds
                      SurfaceCount = model.Surfaces.Length
                      Cells = [||] }

                // generate the tufts of a cell
                let generateCell (cellX : int) (cellZ : int) =

                    // compute cell extent and candidate spacing
                    let bounds = terrainBounds
                    let cellMin = v2 (bounds.Min.X + single cellX * cellSize) (bounds.Min.Z + single cellZ * cellSize)
                    let candidatesPerAxis = cellSize * sqrt (max density 0.0f) |> ceil |> int |> max 1
                    let spacing = cellSize / single candidatesPerAxis
                    let random = Random ((seed * 92821 + cellX) * 68917 + cellZ)

                    // compute placement constants
                    let leanMax = sin (Math.DegreesToRadiansF (Math.Clamp (leanMaxDegrees, 0.0f, 90.0f)))
                    let slopeFadeMin = min slopeFade.X slopeFade.Y
                    let slopeFadeMax = max slopeFade.X slopeFade.Y
                    let modelBounds = field.ModelBounds
                    let modelRadius = [|modelBounds.Min.X; modelBounds.Max.X; modelBounds.Min.Z; modelBounds.Max.Z|] |> Array.map abs |> Array.max
                    let modelHeight = max modelBounds.Max.Y 0.01f
                    let albedo = materialProperties.Albedo

                    // generate tufts
                    let items = List<StaticModelSurfacePreBatchItem> ()
                    for j in 0 .. dec candidatesPerAxis do
                        for i in 0 .. dec candidatesPerAxis do

                            // draw every random value up front so that each candidate consumes the same randomness
                            let jitterX = random.NextSingle ()
                            let jitterZ = random.NextSingle ()
                            let acceptance = random.NextSingle ()
                            let yaw = random.NextSingle () * MathF.Tau
                            let heightRandom = random.NextSingle ()
                            let leanRandom = v2 (random.NextSingle () * 2.0f - 1.0f) (random.NextSingle () * 2.0f - 1.0f)
                            let colorRandom = v2 (random.NextSingle ()) (random.NextSingle ())

                            // place candidate within the terrain
                            let x = cellMin.X + (single i + jitterX) * spacing
                            let z = cellMin.Y + (single j + jitterZ) * spacing
                            if x >= bounds.Min.X && x <= bounds.Max.X && z >= bounds.Min.Z && z <= bounds.Max.Z then

                                // compute density from control, variation, and slope
                                let control = sampleControl field x z
                                let variation = sampleVariation field.Variation variationTileSize x z
                                let struct (height, terrainNormal) = sampleTerrain field x z
                                let slope = Math.RadiansToDegreesF (acos (Math.Clamp (terrainNormal.Y, -1.0f, 1.0f)))
                                let slopeDensity =
                                    if slopeFadeMax > slopeFadeMin
                                    then 1.0f - Math.Clamp ((slope - slopeFadeMin) / (slopeFadeMax - slopeFadeMin), 0.0f, 1.0f)
                                    elif slope < slopeFadeMin then 1.0f
                                    else 0.0f
                                let density = control.Z * Math.Lerp (1.0f, variation.Z, Math.Clamp (densityVariation, 0.0f, 1.0f)) * slopeDensity
                                if acceptance < density then

                                    // compute lean from control, variation, and randomness relative to world up, then
                                    // blend it toward the terrain normal
                                    let leanControl = v2 (control.X * 2.0f - 1.0f) (control.Y * 2.0f - 1.0f)
                                    let leanVariation = v2 (variation.X * 2.0f - 1.0f) (variation.Y * 2.0f - 1.0f) * leanMax
                                    let lean = leanControl + leanVariation + leanRandom * leanMax * 0.35f
                                    let lean = if lean.Magnitude > 0.95f then lean.Normalized * 0.95f else lean
                                    let up = v3 lean.X (sqrt (1.0f - lean.MagnitudeSquared)) lean.Y
                                    let up = (Vector3.Lerp (up, terrainNormal, Math.Clamp (terrainLean, 0.0f, 1.0f))).Normalized

                                    // compute height
                                    let heightScalar = control.W * (1.0f + Math.Clamp (heightVariation, 0.0f, 1.0f) * (variation.W * 2.0f - 1.0f))
                                    let tuftHeight = Math.Lerp (heightRange.X, heightRange.Y, heightRandom) * max heightScalar 0.1f / modelHeight

                                    // compute transform
                                    let tilt =
                                        let axis = Vector3.Cross (v3Up, up)
                                        if axis.MagnitudeSquared > 0.000001f
                                        then Quaternion.CreateFromAxisAngle (axis.Normalized, acos (Math.Clamp (up.Y, -1.0f, 1.0f)))
                                        else Quaternion.Identity
                                    let rotation = tilt * Quaternion.CreateFromAxisAngle (v3Up, yaw)
                                    let scale = v3 (tuftHeight * width) tuftHeight (tuftHeight * width)
                                    let position = v3 x height z
                                    let modelMatrix = Matrix4x4.CreateScale scale * Matrix4x4.CreateFromQuaternion rotation * Matrix4x4.CreateTranslation position

                                    // compute bounds generously enough to cover lean and wind
                                    let height = modelHeight * tuftHeight
                                    let radius = modelRadius * scale.X + height * 0.75f
                                    let tuftBounds = box3 (position - v3 radius (height * 0.25f) radius) (v3 (radius * 2.0f) (height * 1.25f) (radius * 2.0f))

                                    // compute material properties, varying the entity's albedo
                                    let colorVariation = Math.Clamp (colorVariation, 0.0f, 1.0f)
                                    let brightness = 1.0f - colorVariation * colorRandom.X
                                    let yellowing = colorVariation * colorRandom.Y
                                    let albedoVariation = color (brightness * (1.0f + yellowing * 0.35f)) brightness (brightness * (1.0f - yellowing * 0.6f)) 1.0f
                                    let properties = { materialProperties with AlbedoOpt = ValueSome (albedo * albedoVariation) }

                                    // add tuft
                                    items.Add
                                        { ModelMatrix = modelMatrix
                                          CastShadow = castShadow
                                          Presence = Exterior
                                          Inset = box2Zero
                                          MaterialProperties = properties
                                          Bounds = tuftBounds }

                    // compute cell bounds and pre-batch ids
                    let items = items.ToArray ()
                    let cellBounds =
                        if items.Length > 0
                        then items |> Array.fold (fun (bounds : Box3) item -> bounds.Combine item.Bounds) items[0].Bounds
                        else box3 (v3 cellMin.X bounds.Min.Y cellMin.Y) (v3 cellSize bounds.Size.Y cellSize)
                    let preBatchIds = Array.init field.SurfaceCount (fun _ -> Gen.id)
                    { Bounds = cellBounds; Items = items; PreBatchIds = preBatchIds }

                // generate every cell of the terrain, in parallel since each cell is seeded independently
                let cellsX = terrainBounds.Size.X / cellSize |> ceil |> int |> max 1
                let cellsZ = terrainBounds.Size.Z / cellSize |> ceil |> int |> max 1
                let cells = Array.Parallel.init (cellsX * cellsZ) (fun cellIndex -> generateCell (cellIndex % cellsX) (cellIndex / cellsX))
                Some { field with Cells = cells }
            | ValueNone -> Log.infoOnce ("Could not find foliage model '" + scstring staticModel + "'."); None
        | ValueNone -> None

    // discard the generated field so that the next render regenerates it from its changed inputs
    static let handleGenerationChange (evt : Event<ChangeData, Entity>) (world : World) =
        evt.Subscriber.SetFoliageFieldOpt None world
        Cascade

    // watch the terrain's inputs to generation
    // NOTE: using manual unsubscription (in Unregister, which also runs when the facet is removed, and when the terrain
    // address changes) since the terrain can change during the life of the facet.
    static let subscribeTerrain (entity : Entity) world =
        match tryGetTerrain entity world with
        | Some terrain ->
            World.subscribeFacetPlus Gen.id64 (nameof handleGenerationChange) (terrain.ChangeEvent (nameof terrain.Transform)) entity (nameof FoliageFacet) world |> ignore
            World.subscribeFacetPlus Gen.id64 (nameof handleGenerationChange) terrain.HeightMap.ChangeEvent entity (nameof FoliageFacet) world |> ignore
            World.subscribeFacetPlus Gen.id64 (nameof handleGenerationChange) terrain.NormalImageOpt.ChangeEvent entity (nameof FoliageFacet) world |> ignore
        | None -> ()

    // stop watching the terrain at the given address, which may no longer exist
    static let unsubscribeTerrain (terrainAddress : Entity Address) (entity : Entity) world =
        match tryResolve terrainAddress entity with
        | Some terrain ->
            World.unsubscribeFacet5 (nameof handleGenerationChange) (terrain.ChangeEvent (nameof terrain.Transform)) entity (nameof FoliageFacet) world
            World.unsubscribeFacet5 (nameof handleGenerationChange) terrain.HeightMap.ChangeEvent entity (nameof FoliageFacet) world
            World.unsubscribeFacet5 (nameof handleGenerationChange) terrain.NormalImageOpt.ChangeEvent entity (nameof FoliageFacet) world
        | None -> ()

    // move the terrain subscriptions from the previously addressed terrain to the newly addressed one and discard the
    // field generated for the old one
    static let handleTerrainChange (evt : Event<ChangeData, Entity>) (world : World) =
        let entity = evt.Subscriber
        unsubscribeTerrain (evt.Data.Previous :?> Entity Address) entity world
        subscribeTerrain entity world
        entity.SetFoliageFieldOpt None world
        Cascade

    static member Properties =
        [define Entity.Presence Omnipresent
         define Entity.Static true
         define Entity.CastShadow false
         define Entity.StaticModel Assets.Gameplay.GrassTuftModel
         define Entity.Material
            { Material.empty with
                AlbedoImageOpt = ValueSome Assets.Gameplay.GrassTuftAlbedoImage
                UserDefinedImage0Opt = ValueSome Assets.Gameplay.FoliageWindNoiseImage
                CullModeOpt = ValueSome CullBack // the default grass tuft is double-sided
                ClippedOpt = ValueSome false // the default grass tuft's geometry gives its shape
                PipelineNameOpt = ValueSome Constants.Foliage.PipelineName }
         define Entity.MaterialProperties
            { MaterialProperties.empty with
                ScatterTypeOpt = ValueSome FoliageScatter }
         define Entity.FoliageControlImageOpt None
         define Entity.FoliageTerrain Address.parent
         define Entity.FoliageDensity 2.5f // tufts per square meter at full density
         define Entity.FoliageCellSize 16.0f // edge length in meters of the square cells that tufts are generated in
         define Entity.FoliageDrawDistance 64.0f // distance at which tufts are fully collapsed into their roots
         define Entity.FoliageCollapseStart 0.75f // fraction of the draw distance at which tufts begin collapsing
         define Entity.FoliageRoughenEnd 1.0f // fraction of the draw distance at which tufts become fully rough
         define Entity.FoliageHeightRange (v2 0.5f 0.9f) // range of tuft heights in meters before height variation
         define Entity.FoliageWidth 1.0f // horizontal scale of tufts relative to their height
         define Entity.FoliageLeanMax 20.0f // maximum generated lean in degrees
         define Entity.FoliageTerrainLean 0.35f // portion of lean taken from the terrain normal, from 0 to 1
         define Entity.FoliageSlopeFade (v2 35.0f 45.0f) // terrain slopes in degrees across which density fades out
         define Entity.FoliageVariationTileSize 48.0f // meters covered by one tile of the generated variation texture
         define Entity.FoliageDensityVariation 0.75f // how much the variation texture thins density, from 0 to 1
         define Entity.FoliageHeightVariation 0.5f // how much the variation texture scales height, from 0 to 1
         define Entity.FoliageColorVariation 0.2f // how much tuft albedo varies at random, from 0 to 1
         define Entity.FoliageSeed 0 // seed of tuft placement and of the generated variation texture
         define Entity.FoliageWindDirection (v2 1.0f 0.35f) // horizontal direction (x, z) that the wind blows toward
         define Entity.FoliageWindStrength 0.35f // how far gusts bend tufts
         define Entity.FoliageWindSpeed 6.0f // meters per second that gusts travel
         define Entity.FoliageFlutterStrength 0.06f // how far flutter shakes tufts
         define Entity.FoliageWindCycle 60.0f // seconds after which the wind's motion repeats
         nonPersistent Entity.FoliageFieldOpt None]

    override this.Register (entity, world) =
        World.sense (nameof handleTerrainChange) entity.FoliageTerrain.ChangeEvent entity (nameof FoliageFacet) world
        World.sense (nameof handleGenerationChange) entity.FoliageControlImageOpt.ChangeEvent entity (nameof FoliageFacet) world
        World.sense (nameof handleGenerationChange) entity.StaticModel.ChangeEvent entity (nameof FoliageFacet) world
        World.sense (nameof handleGenerationChange) entity.MaterialProperties.ChangeEvent entity (nameof FoliageFacet) world
        World.sense (nameof handleGenerationChange) entity.CastShadow.ChangeEvent entity (nameof FoliageFacet) world
        World.sense (nameof handleGenerationChange) entity.FoliageDensity.ChangeEvent entity (nameof FoliageFacet) world
        World.sense (nameof handleGenerationChange) entity.FoliageCellSize.ChangeEvent entity (nameof FoliageFacet) world
        World.sense (nameof handleGenerationChange) entity.FoliageHeightRange.ChangeEvent entity (nameof FoliageFacet) world
        World.sense (nameof handleGenerationChange) entity.FoliageWidth.ChangeEvent entity (nameof FoliageFacet) world
        World.sense (nameof handleGenerationChange) entity.FoliageLeanMax.ChangeEvent entity (nameof FoliageFacet) world
        World.sense (nameof handleGenerationChange) entity.FoliageTerrainLean.ChangeEvent entity (nameof FoliageFacet) world
        World.sense (nameof handleGenerationChange) entity.FoliageSlopeFade.ChangeEvent entity (nameof FoliageFacet) world
        World.sense (nameof handleGenerationChange) entity.FoliageVariationTileSize.ChangeEvent entity (nameof FoliageFacet) world
        World.sense (nameof handleGenerationChange) entity.FoliageDensityVariation.ChangeEvent entity (nameof FoliageFacet) world
        World.sense (nameof handleGenerationChange) entity.FoliageHeightVariation.ChangeEvent entity (nameof FoliageFacet) world
        World.sense (nameof handleGenerationChange) entity.FoliageColorVariation.ChangeEvent entity (nameof FoliageFacet) world
        World.sense (nameof handleGenerationChange) entity.FoliageSeed.ChangeEvent entity (nameof FoliageFacet) world
        subscribeTerrain entity world

    override this.Unregister (entity, world) =
        unsubscribeTerrain (entity.GetFoliageTerrain world) entity world

    override this.Render (renderPass, entity, world) =

        // generate the field if it was discarded, doing so when rendering rather than updating so that foliage can be
        // static, and so that it's generated only when it's rendered
        if (entity.GetFoliageFieldOpt world).IsNone then
            match tryGetTerrain entity world with
            | Some terrain ->
                match tryCreateField entity terrain world with
                | Some field -> entity.SetFoliageFieldOpt (Some field) world
                | None -> ()
            | None -> ()

        // render the field
        match entity.GetFoliageFieldOpt world with
        | Some field when field.Cells.Length > 0 ->

            // determine which cells to render in the render pass, if any, and the point to render them nearest first
            // from. Shadows are rendered with the engine's static shadow pipeline, so they're not animated by wind,
            // collapsed, or alpha clipped. Light maps are rendered from their light probe's origin, which isn't known
            // here, so their cells are found by the light map's bounds alone, leaving the shader to collapse the tufts
            // beyond the draw distance from that origin.
            let drawDistance = max (entity.GetFoliageDrawDistance world) 0.0f
            let eyeCenter = World.getEye3dCenter world
            let withinDrawDistance (cell : FoliageCell) =
                let dx = max (max (cell.Bounds.Min.X - eyeCenter.X) (eyeCenter.X - cell.Bounds.Max.X)) 0.0f
                let dz = max (max (cell.Bounds.Min.Z - eyeCenter.Z) (eyeCenter.Z - cell.Bounds.Max.Z)) 0.0f
                dx * dx + dz * dz <= drawDistance * drawDistance
            let cullingOpt =
                match renderPass with
                | NormalPass ->
                    let frustum = World.getEye3dFrustumView world
                    Some (eyeCenter, fun (cell : FoliageCell) -> withinDrawDistance cell && frustum.Intersects cell.Bounds)
                | ShadowPass (_, _, _, _, _, shadowFrustum) ->
                    if entity.GetCastShadow world && (World.getRenderer3dConfig world).LightShadowingEnabled
                    then Some (eyeCenter, fun (cell : FoliageCell) -> withinDrawDistance cell && shadowFrustum.Intersects cell.Bounds)
                    else None
                | LightMapPass (_, lightMapBounds) ->
                    Some (lightMapBounds.Center, fun (cell : FoliageCell) -> lightMapBounds.Intersects cell.Bounds)
                | ReflectionPass (_, _) -> None
            match cullingOpt with
            | Some (sortCenter, cellVisible) ->

                // compute wind, collapse, and roughening settings as an override of the items' user-defined settings,
                // rounding the scrolls made over a cycle to whole numbers so that the cycle wraps seamlessly
                let cycle = max (entity.GetFoliageWindCycle world) 0.001f
                let windDirection = entity.GetFoliageWindDirection world
                let phase = single ((world.ScaledTime.Seconds / float cycle) % 1.0)
                let direction = if windDirection.MagnitudeSquared > 0.0f then windDirection.Normalized else v2 1.0f 0.0f
                let gustScale = 1.0f / Constants.Foliage.GustTileSize
                let gustScroll = direction * entity.GetFoliageWindSpeed world * cycle * gustScale
                let windProperties =
                    { MaterialProperties.empty with
                        UserDefinedSetting0Opt = ValueSome phase
                        UserDefinedSetting1Opt = ValueSome (entity.GetFoliageWindStrength world)
                        UserDefinedSetting2Opt = ValueSome (round gustScroll.X)
                        UserDefinedSetting3Opt = ValueSome (round gustScroll.Y)
                        UserDefinedSetting4Opt = ValueSome (drawDistance * Math.Clamp (entity.GetFoliageRoughenEnd world, 0.0f, 1.0f))
                        UserDefinedSetting5Opt = ValueSome (drawDistance * Math.Clamp (entity.GetFoliageCollapseStart world, 0.0f, 1.0f))
                        UserDefinedSetting6Opt = ValueSome drawDistance
                        UserDefinedSetting7Opt = ValueSome (entity.GetFoliageFlutterStrength world) }

                // collect the visible cells, nearest first so that they render first and occlude what's behind them
                let cellsVisible =
                    field.Cells
                    |> Array.filter cellVisible
                    |> Array.sortBy (fun cell -> Vector3.DistanceSquared (cell.Bounds.Center, sortCenter))

                // build pre-batches of the visible cells from their cached items
                let material = entity.GetMaterial world
                let staticModel = entity.GetStaticModel world
                let preBatches = List<StaticModelSurfacePreBatch> ()
                for cell in cellsVisible do
                    for surfaceIndex in 0 .. dec field.SurfaceCount do
                        preBatches.Add
                            { PreBatchId = cell.PreBatchIds[surfaceIndex]
                              PreBatchItems = cell.Items
                              Material = material
                              MaterialPropertiesOverride = ValueSome windProperties
                              StaticModel = staticModel
                              SurfaceIndex = surfaceIndex
                              RenderType = DeferredRenderType }

                // render pre-batches
                if preBatches.Count > 0 then
                    World.enqueueRenderMessage3d
                        (RenderStaticModelSurfacePreBatches
                            { StaticModelSurfacePreBatches = preBatches.ToArray ()
                              RenderPass = renderPass })
                        world

            | None -> ()
        | Some _ | None -> ()

/// Gives an entity the base behavior of wind-blown foliage covering a terrain.
type FoliageDispatcher () =
    inherit Entity3dDispatcher (false, false, false)

    static member Facets =
        [typeof<FoliageFacet>]