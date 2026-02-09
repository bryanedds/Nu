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

    static member getBlockOpt positionI chunk =
        Map.tryFind positionI chunk.Blocks

    static member setBlockOpt (positionI : Vector3i) blockOpt chunk =
        if chunk.BoundsI.ContainsExclusive positionI <> ContainmentType.Disjoint then
            match blockOpt with
            | Some block -> { chunk with Blocks = Map.add positionI block chunk.Blocks }
            | None -> { chunk with Blocks = Map.remove positionI chunk.Blocks }
        else chunk

    static member mapBlockOpt mapper positionI chunk =
        let blockOpt = Chunk.getBlockOpt positionI chunk
        let blockOpt = mapper blockOpt
        Chunk.setBlockOpt positionI blockOpt chunk

    static member getBlock positionI chunk =
        match Chunk.getBlockOpt positionI chunk with
        | Some block -> block
        | None -> failwith ("No block located at positionI " + scstring positionI + ".")

    static member setBlock positionI block chunk =
        Chunk.setBlockOpt positionI (Some block) chunk

    static member mapBlock mapper positionI chunk =
        let block = Chunk.getBlock positionI chunk
        let block = mapper block
        Chunk.setBlock positionI block chunk

    static member granulate : Granulator -> Chunk -> Chunk =
        failwithnie ()
    
    static member combine : Combiner -> Chunk -> Chunk =
        failwithnie ()

    static member make boundsI blocks =
        { BoundsI = boundsI; Blocks = blocks }

    static member initial =
        Chunk.make (box3i v3iZero (v3iDup 24)) Map.empty

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
      Cursor : Vector3i
      Selection : Selection
      Palette : Palette
      PaletteSelection : int
      PaintHeight : int
      Passes : Map<string, Pass>
      Config : Config
      Scale : Vector3
      Chunk : Chunk }

    (* Properties *)

    member this.Size =
        this.Chunk.BoundsI.Size.V3 * this.Scale

    (* Low-Level API *)

    static member setEditPlane plane blockMap =
        { blockMap with EditPlane = plane }

    static member mapEditPlane mapper blockMap =
        { blockMap with EditPlane = mapper blockMap.EditPlane }

    static member setLayersVisible layersVisible blockMap =
        { blockMap with LayersVisible = layersVisible }

    static member mapLayersVisible mapper blockMap =
        { blockMap with LayersVisible = mapper blockMap.LayersVisible }

    static member setCursor (cursor : Vector3i) blockMap =
        if blockMap.Chunk.BoundsI.ContainsExclusive cursor = ContainmentType.Disjoint then
            failwith "Block cursor position must be within the block map chunk bounds."
        { blockMap with Cursor = cursor }

    static member mapCursor mapper blockMap =
        { blockMap with Cursor = mapper blockMap.Cursor }

    static member setSelection selection blockMap =
        // TODO: check selection for appropriate boundedness.
        { blockMap with Selection = selection }

    static member mapSelection mapper blockMap =
        { blockMap with Selection = mapper blockMap.Selection }

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

    static member mapPalette mapper blockMap =
        let palette = mapper blockMap.Palette
        BlockMap.setPalette palette blockMap

    static member setPaletteSelection paletteSelection blockMap =
        if paletteSelection < 0 || paletteSelection >= Array.length blockMap.Palette.Styles then
            failwith "Block palette selection must be within the range of the block palette styles."
        { blockMap with PaletteSelection = paletteSelection }

    static member mapPaletteSelection mapper blockMap =
        let paletteSelection = mapper blockMap.PaletteSelection
        BlockMap.setPaletteSelection paletteSelection blockMap

    static member setPaintHeight paintHeight blockMap =
        { blockMap with PaintHeight = max 1 paintHeight }

    static member mapPaintHeight mapper blockMap =
        let paintHeight = mapper blockMap.PaintHeight
        BlockMap.setPaintHeight paintHeight blockMap

    static member setPasses passes blockMap =
        { blockMap with Passes = passes }

    static member mapPasses mapper blockMap =
        let passes = mapper blockMap.Passes
        BlockMap.setPasses passes blockMap

    static member setConfig config blockMap =
        { blockMap with Config = config }

    static member mapConfig mapper blockMap =
        let config = mapper blockMap.Config
        BlockMap.setConfig config blockMap

    static member setScale scale blockMap =
        { blockMap with Scale = scale }

    static member mapScale mapper blockMap =
        let scale = mapper blockMap.Scale
        BlockMap.setScale scale blockMap

    static member setChunk chunk blockMap =
        if chunk.Blocks.Count = 0 then
            failwith "Block map chunk must contain a block chunk with at least one block."
        { blockMap with Chunk = chunk }

    static member mapChunk mapper blockMap =
        { blockMap with Chunk = mapper blockMap.Chunk }

    static member getBlockOpt positionI blockMap =
        Chunk.getBlockOpt positionI blockMap.Chunk

    static member setBlockOpt (positionI : Vector3i) blockOpt blockMap =
        { blockMap with Chunk = Chunk.setBlockOpt positionI blockOpt blockMap.Chunk }

    static member mapBlockOpt mapper positionI blockMap =
        { blockMap with Chunk = Chunk.mapBlockOpt mapper positionI blockMap.Chunk }

    static member getBlock positionI blockMap =
        Chunk.getBlock positionI blockMap.Chunk

    static member setBlock positionI block blockMap =
        { blockMap with Chunk = Chunk.setBlock positionI block blockMap.Chunk }

    static member mapBlock mapper positionI blockMap =
        { blockMap with Chunk = Chunk.mapBlock mapper positionI blockMap.Chunk }

    (* Derived API *)

    static member getBounds position (blockMap : BlockMap) =
        let size = blockMap.Size
        Box3 (position - size * 0.5f, size)

    static member tryGetStyle styleIndex blockMap =
        Palette.tryGetStyle styleIndex blockMap.Palette

    static member tryGetSelectedColor blockMap =
        match BlockMap.tryGetStyle blockMap.PaletteSelection blockMap with
        | Some style -> Some style.Color
        | None -> None

    static member addPass passName pass blockMap =
        { blockMap with Passes = Map.add passName pass blockMap.Passes }

    static member removePass passName blockMap =
        { blockMap with Passes = Map.remove passName blockMap.Passes }

    static member tryGetBlockColor block blockMap =
        Block.tryGetColor block blockMap.Palette

    (* High-Level API *)

    static member tryPickPositionI (ray : Ray3) blockMapPosition (blockMap : BlockMap) =
        let bounds = BlockMap.getBounds blockMapPosition blockMap
        match blockMap.EditPlane with
        | YNeg | YPos ->
            let gridY = single blockMap.Cursor.Y * single blockMap.Scale.Y - bounds.Size.Y * 0.5f
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
                            (blockMap.Cursor.Y + if blockMap.EditPlane.IsYNeg then -1 else 0)
                            (int (intersection.Z / blockMap.Scale.Z))
                    if blockMap.Chunk.BoundsI.ContainsExclusive positionI <> ContainmentType.Disjoint
                    then Some positionI
                    else None
                else None
            else None
        | XNeg | XPos ->
            let gridX = single blockMap.Cursor.X * single blockMap.Scale.X - bounds.Size.X * 0.5f
            let gridCenter = bounds.Center + v3 gridX 0.0f 0.0f
            let plane = Plane3 (gridCenter, v3Right)
            let intersectionTOpt = plane.Intersection ray
            if intersectionTOpt.HasValue then
                let intersectionT = intersectionTOpt.Value
                let intersection = intersectionT - bounds.Min
                if intersection.Y >= 0.0f && intersection.Z >= 0.0f then
                    let positionI =
                        v3i
                            (blockMap.Cursor.X + if blockMap.EditPlane.IsXNeg then -1 else 0)
                            (int (intersection.Y / blockMap.Scale.Y))
                            (int (intersection.Z / blockMap.Scale.Z))
                    if blockMap.Chunk.BoundsI.ContainsExclusive positionI <> ContainmentType.Disjoint
                    then Some positionI
                    else None
                else None
            else None
        | ZNeg | ZPos ->
            let gridZ = single blockMap.Cursor.Z * single blockMap.Scale.Z - bounds.Size.Z * 0.5f
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
                            (blockMap.Cursor.Z + if blockMap.EditPlane.IsZNeg then -1 else 0)
                    if blockMap.Chunk.BoundsI.ContainsExclusive positionI <> ContainmentType.Disjoint
                    then Some positionI
                    else None
                else None
            else None

    static member paint (positionI : Vector3i) blockMap =
        match BlockMap.tryGetStyle blockMap.PaletteSelection blockMap with
        | Some style ->
            List.fold (fun (blockMap : BlockMap) i ->
                let offsetI =
                    match blockMap.EditPlane with
                    | XPos -> v3i i 0 0
                    | XNeg -> v3i -i 0 0
                    | YPos -> v3i 0 i 0
                    | YNeg -> v3i 0 -i 0
                    | ZPos -> v3i 0 0 i
                    | ZNeg -> v3i 0 0 -i
                let block = Block.make blockMap.PaletteSelection 0 style.Properties
                BlockMap.setBlock (positionI + offsetI) block blockMap)
                blockMap [0 .. dec blockMap.PaintHeight]
        | None -> blockMap

    static member initial =
        let chunk = Chunk.initial
        { Generated = false
          EditPlane = YPos
          LayersVisible = chunk.BoundsI.Max.Y
          Cursor = chunk.BoundsI.Size / 2
          Selection = Selection.initial
          Palette = Palette.initial
          PaletteSelection = 0
          PaintHeight = 1
          Passes = Map.empty
          Config = Config.initial
          Scale = v3One
          Chunk = chunk }

[<RequireQualifiedAccess>]
module ProcessFns =

    let Id _ _ _ block =
        Some ((fun _ _ -> ()), block)

    let ProcessFns<'p, 'w> : Map<string, ProcessFn<'p, 'w>> =
        [(nameof Id, Id)]
        |> Map.ofList