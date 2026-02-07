// Nu Game Engine.
// Required Notice:
// Copyright (C) Bryan Edds.
// Nu Game Engine is licensed under the Nu Game Engine Noncommercial License.
// See https://github.com/bryanedds/Nu/blob/master/License.md.

namespace Nu.BlockMap
open System
open System.Numerics
open Prime
open Nu

type TypeName =
    string // some way to identify a type at runtime

type Style =
    { Color : Color
      Description : string
      Properties : Map<string, TypeName * Symbol> }

    static member make color description properties =
        { Color = color; Description = description; Properties = properties }

type Palette =
    { Styles : Style array } // TODO: change Styles to the proposed F# block type when available.

    static member BaseColorNames =
        [nameof Color.Gray
         nameof Color.SlateBlue; nameof Color.Aquamarine; nameof Color.Blue; nameof Color.Navy; nameof Color.SteelBlue
         nameof Color.Teal; nameof Color.LimeGreen; nameof Color.ForestGreen
         nameof Color.Olive; nameof Color.Yellow; nameof Color.Gold
         nameof Color.DarkSlateGray
         nameof Color.Purple; nameof Color.Indigo; nameof Color.Magenta; nameof Color.Orchid
         nameof Color.IndianRed; nameof Color.Red; nameof Color.Maroon
         nameof Color.Chocolate; nameof Color.Brown
         nameof Color.Orange; nameof Color.OrangeRed]

    static member BaseColorValues =
        List.map (fun name -> (typeof<Color>.GetProperty name).GetValue null :?> Color) Palette.BaseColorNames

    static member tryGetStyle index palette =
        if index >= 0 && index < Array.length palette.Styles
        then Some palette.Styles.[index]
        else None

    static member addStyle style palette =
        { Styles = Array.add style palette.Styles }

    static member removeStyle index palette =
        { Styles = Array.removeAt index palette.Styles }

    static member initial =
        let blockStyles =
            [|for (name, color) in List.zip Palette.BaseColorNames Palette.BaseColorValues do
                Style.make color name Map.empty|]
        { Styles = blockStyles }

type Block =
    { StyleIndex : int
      ColorShift : int // -3 .. +3. Each shift can signify an additional level of adornment, such as stacking a pen on a book on a desk.
      Properties : Map<string, TypeName * Symbol> }

    static member shiftColor shift (color : Color) =
        let shiftAmount = 0.1f * single shift
        let r = saturate (color.R + shiftAmount)
        let g = saturate (color.G + shiftAmount)
        let b = saturate (color.B + shiftAmount)
        Color (r, g, b, color.A)

    static member tryGetColor block palette =
        match Palette.tryGetStyle block.StyleIndex palette with
        | Some style -> Some (Block.shiftColor block.ColorShift style.Color)
        | None -> None

    static member make styleIndex colorShift properties =
        { StyleIndex = styleIndex; ColorShift = colorShift; Properties = properties }

type Granulator =
    { Granulation : Vector3i
      GranulatorFnName : string }

and GranulatorFn =
    Vector3i -> Chunk -> Chunk

and Combiner =
    { Combination : Vector3i
      CombinerFnName : string }

and CombinerFn =
    Vector3i -> Chunk -> Chunk

and Chunk =
    { BoundsI : Box3i
      Blocks : Map<Vector3i, Block> }

    static member tryGetBlock positionI chunk =
        Map.tryFind positionI chunk.Blocks

    static member trySetBlock (positionI : Vector3i) block chunk =
        if chunk.BoundsI.ContainsExclusive positionI <> ContainmentType.Disjoint then
            let blocks = Map.add positionI block chunk.Blocks
            let chunk = { chunk with Blocks = blocks }
            Some chunk
        else None

    static member granulate : Granulator -> Chunk -> Chunk =
        failwithnie ()
    
    static member combine : Combiner -> Chunk -> Chunk =
        failwithnie ()

    static member make boundsI blocks =
        { BoundsI = boundsI; Blocks = blocks }

    static member initial =
        Chunk.make (box3i v3iZero (v3i 24 12 24)) Map.empty

type Cursor =
    { PositionI : Vector3i }

    static member initial =
        { PositionI = v3iDup 3 }

type Selection =
    | SelectionVolume of Vector3i * Vector3i
    | SelectionAdHoc of Vector3i Set

    static member initial =
        SelectionAdHoc Set.empty

type ProcessParam =
    | LightParam of float * LightType // ...and so on...
    | StaticModelParam of StaticModel AssetTag
    | StaticModelSurfaceParam of StaticModel AssetTag * int
    | RigidModelParam of StaticModel AssetTag
    | RigidModelSurfaceParam of StaticModel AssetTag * int
    | SymbolParam of Symbol

type Processor =
    { ProcessorName : string
      ProcessParams : Map<string, ProcessParam>
      ProcessFnName : string }

    static member make name processParams processFnName =
        { ProcessorName = name
          ProcessParams = processParams
          ProcessFnName = processFnName }

type ProcessEffect<'p, 'w> =
    'p -> 'w -> unit

type ProcessFn<'p, 'w> =
    Vector3i -> Affine -> Map<string, ProcessParam> -> Chunk -> (ProcessEffect<'p, 'w> * Chunk) option

type Pass =
    { Processors : Processor array }

    static member addProcessor processor pass =
        { pass with Processors = Array.add processor pass.Processors }

    static member removeProcessor processorIndex pass =
        { pass with Processors = Array.removeAt processorIndex pass.Processors }

    static member replaceProcessor processor pass =
        { pass with
            Processors =
                Array.map
                    (fun processor' -> if processor'.ProcessorName = processor.ProcessorName then processor else processor')
                    pass.Processors }

    static member tryGetProcessor name pass =
        Array.tryFind (fun processor -> processor.ProcessorName = name) pass.Processors

    static member initial =
        { Processors = [||] }

type EditPlane =
    | XNeg | XPos | YNeg | YPos | ZNeg | ZPos

type Config =
    { CastShadows : bool }

    static member initial =
        { CastShadows = true }

type [<SymbolicExpansion>] BlockMap =
    { Generated : bool
      EditPlane : EditPlane // plane currently containing cursor
      LayersVisible : int
      Cursor : Cursor
      Selection : Selection
      Palette : Palette
      PaletteSelection : int
      PaintHeight : int
      Passes : Map<string, Pass>
      Config : Config
      Scale : Vector3
      Chunk : Chunk }

    member this.Size =
        this.Chunk.BoundsI.Size.V3 * this.Scale

    static member getBounds position (blockMap : BlockMap) =
        let size = blockMap.Size
        Box3 (position - size * 0.5f, size)

    static member tryGetBlock positionI blockMap =
        Chunk.tryGetBlock positionI blockMap.Chunk

    static member trySetBlock (positionI : Vector3i) block blockMap =
        match Chunk.trySetBlock positionI block blockMap.Chunk with
        | Some chunk -> Some { blockMap with Chunk = chunk }
        | None -> None

    static member tryGetSelectedStyle blockMap =
        Palette.tryGetStyle blockMap.PaletteSelection blockMap.Palette

    static member tryGetSelectedColor blockMap =
        match BlockMap.tryGetSelectedStyle blockMap with
        | Some style -> Some style.Color
        | None -> None

    static member tryGetBlockStyle block blockMap =
        Palette.tryGetStyle block.StyleIndex blockMap.Palette

    static member tryGetBlockColor block blockMap =
        Block.tryGetColor block blockMap.Palette

    static member setChunk chunk blockMap =
        if chunk.Blocks.Count = 0 then
            failwith "Block map chunk must contain a block chunk with at least one block."
        { blockMap with Chunk = chunk }

    static member setEditPlane plane blockMap =
        { blockMap with EditPlane = plane }

    static member setLayersVisible layersVisible blockMap =
        { blockMap with LayersVisible = layersVisible }

    static member setCursor cursor blockMap =
        if blockMap.Chunk.BoundsI.ContainsExclusive cursor.PositionI = ContainmentType.Disjoint then
            failwith "Block cursor position must be within the block map chunk bounds."
        { blockMap with Cursor = cursor }

    static member setSelection selection blockMap =
        // TODO: check selection for appropriate boundedness.
        { blockMap with Selection = selection }

    static member setPalette palette blockMap =
        if palette.Styles.Length = 0 then
            failwith "Block palette must contain at least one block style."
        let paletteSelection =
            if blockMap.PaletteSelection < Array.length palette.Styles
            then blockMap.PaletteSelection
            else 0
        { blockMap with
            Palette = palette
            PaletteSelection = paletteSelection }

    static member setPaletteSelection paletteSelection blockMap =
        if paletteSelection < 0 || paletteSelection >= Array.length blockMap.Palette.Styles then
            failwith "Block palette selection must be within the range of the block palette styles."
        { blockMap with PaletteSelection = paletteSelection }

    static member tryPickPositionI (ray : Ray3) blockMapPosition (blockMap : BlockMap) =
        let bounds = BlockMap.getBounds blockMapPosition blockMap
        match blockMap.EditPlane with
        | YNeg | YPos ->
            let gridY = single blockMap.Cursor.PositionI.Y * single blockMap.Scale.Y - bounds.Size.Y * 0.5f
            let gridCenter = bounds.Center + v3 0.0f gridY 0.0f
            let plane = Plane3 (gridCenter, v3Up)
            let intersectionTOpt = plane.Intersection ray
            if intersectionTOpt.HasValue then
                let intersectionT = intersectionTOpt.Value
                let intersection = intersectionT - bounds.Min
                if intersection.X >= 0.0f && intersection.Z >= 0.0f then
                    let positionI =
                        v3i
                            (int (intersection.X / blockMap.Scale.X))
                            (blockMap.Cursor.PositionI.Y + if blockMap.EditPlane.IsYNeg then -1 else 0)
                            (int (intersection.Z / blockMap.Scale.Z))
                    if blockMap.Chunk.BoundsI.ContainsExclusive positionI <> ContainmentType.Disjoint
                    then Some positionI
                    else None
                else None
            else None
        | XNeg | XPos ->
            let gridX = single blockMap.Cursor.PositionI.X * single blockMap.Scale.X - bounds.Size.X * 0.5f
            let gridCenter = bounds.Center + v3 gridX 0.0f 0.0f
            let plane = Plane3 (gridCenter, v3Right)
            let intersectionTOpt = plane.Intersection ray
            if intersectionTOpt.HasValue then
                let intersectionT = intersectionTOpt.Value
                let intersection = intersectionT - bounds.Min
                if intersection.Y >= 0.0f && intersection.Z >= 0.0f then
                    let positionI =
                        v3i
                            (blockMap.Cursor.PositionI.X + if blockMap.EditPlane.IsXNeg then -1 else 0)
                            (int (intersection.Y / blockMap.Scale.Y))
                            (int (intersection.Z / blockMap.Scale.Z))
                    if blockMap.Chunk.BoundsI.ContainsExclusive positionI <> ContainmentType.Disjoint
                    then Some positionI
                    else None
                else None
            else None
        | ZNeg | ZPos ->
            let gridZ = single blockMap.Cursor.PositionI.Z * single blockMap.Scale.Z - bounds.Size.Z * 0.5f
            let gridCenter = bounds.Center + v3 0.0f 0.0f gridZ
            let plane = Plane3 (gridCenter, v3Forward)
            let intersectionTOpt = plane.Intersection ray
            if intersectionTOpt.HasValue then
                let intersectionT = intersectionTOpt.Value
                let intersection = intersectionT - bounds.Min
                if intersection.X >= 0.0f && intersection.Y >= 0.0f then
                    let positionI =
                        v3i
                            (int (intersection.X / blockMap.Scale.X))
                            (int (intersection.Y / blockMap.Scale.Y))
                            (blockMap.Cursor.PositionI.Z + if blockMap.EditPlane.IsZNeg then -1 else 0)
                    if blockMap.Chunk.BoundsI.ContainsExclusive positionI <> ContainmentType.Disjoint
                    then Some positionI
                    else None
                else None
            else None

    static member tryPaintBlock (positionI : Vector3i) blockMap =
        match BlockMap.tryGetSelectedStyle blockMap with
        | Some style ->
            let mutable chunk = blockMap.Chunk
            for i in 0 .. dec blockMap.PaintHeight do
                let offsetI =
                    match blockMap.EditPlane with
                    | XPos -> v3i i 0 0
                    | XNeg -> v3i -i 0 0
                    | YPos -> v3i 0 i 0
                    | YNeg -> v3i 0 -i 0
                    | ZPos -> v3i 0 0 i
                    | ZNeg -> v3i 0 0 -i
                let block = Block.make blockMap.PaletteSelection 0 style.Properties
                match Chunk.trySetBlock (positionI + offsetI) block chunk with
                | Some chunk' -> chunk <- chunk'
                | None -> ()
            Some (BlockMap.setChunk chunk blockMap)
        | None -> None

    static member addPass passName pass blockMap =
        { blockMap with Passes = Map.add passName pass blockMap.Passes }

    static member removePass passName blockMap =
        { blockMap with Passes = Map.remove passName blockMap.Passes }

    static member initial =
        { Generated = false
          EditPlane = YPos
          LayersVisible = 12
          Cursor = Cursor.initial
          Selection = Selection.initial
          Palette = Palette.initial
          PaletteSelection = 0
          PaintHeight = 1
          Passes = Map.empty
          Config = Config.initial
          Scale = v3Dup 1.0f
          Chunk = Chunk.initial }

[<RequireQualifiedAccess>]
module ProcessFns =

    let Id _ _ _ block =
        Some ((fun _ _ -> ()), block)

    let ProcessFns<'p, 'w> : Map<string, ProcessFn<'p, 'w>> =
        [(nameof Id, Id)]
        |> Map.ofList