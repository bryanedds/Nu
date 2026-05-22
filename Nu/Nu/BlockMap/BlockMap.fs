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
        then Some palette.Styles[index]
        else None

    static member addStyle style palette =
        { Styles = Array.add style palette.Styles }

    static member removeStyle index palette =
        { Styles = Array.removeAt index palette.Styles }

    static member val initial =
        let blockStyles =
            [|for (name, color) in List.zip Palette.BaseColorNames Palette.BaseColorValues do
                Style.make color name Map.empty|]
        { Styles = blockStyles }

type Consumer =
    { PassName : string
      ProcessorName : string }

    static member make passName processorName =
        { PassName = passName; ProcessorName = processorName }

type Block =
    { PositionI : Vector3i
      StyleIndex : int
      ColorShift : int // -3 .. +3. Each shift can signify an additional level of adornment, such as stacking a pen on a book on a desk.
      Properties : Map<string, TypeName * Symbol>
      Consumers : Consumer Set }

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

    static member registerConsumer consumer block =
        { block with Consumers = Set.add consumer block.Consumers }

    static member getAvailable consumerOpt block =
        match consumerOpt with
        | Some consumer -> not (Set.contains consumer block.Consumers)
        | None -> true

    static member make styleIndex colorShift properties =
        { PositionI = v3iZero
          StyleIndex = styleIndex
          ColorShift = colorShift
          Properties = properties
          Consumers = Set.empty }

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
            | Some block ->
                let block = if block.PositionI <> positionI then { block with PositionI = positionI } else block
                { chunk with Blocks = Map.add positionI block chunk.Blocks }
            | None -> { chunk with Blocks = Map.remove positionI chunk.Blocks }
        else chunk

    static member consumeBlockOpt blockOpt consumer chunk =
        Chunk.mapBlockOpt (Option.map (Block.registerConsumer consumer)) blockOpt chunk

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

    static member consumeBlock block consumer chunk =
        Chunk.mapBlock (Block.registerConsumer consumer) block.PositionI chunk

    static member mapBlock mapper positionI chunk =
        let block = Chunk.getBlock positionI chunk
        let block = mapper block
        Chunk.setBlock positionI block chunk

    static member consumeBlocks blocks consumer chunk =
        Seq.fold
            (fun chunk block -> Chunk.consumeBlock block consumer chunk)
            chunk blocks

    static member granulate : Granulator -> Chunk -> Chunk =
        failwithnie ()
    
    static member combine : Combiner -> Chunk -> Chunk =
        failwithnie ()

    static member make boundsI blocks =
        { BoundsI = boundsI; Blocks = blocks }

    static member val initial =
        Chunk.make (box3i v3iZero (v3iDup 24)) Map.empty

type Selection =
    | SelectionVolume of Vector3i * Vector3i
    | SelectionAdHoc of Vector3i Set

    static member val initial =
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
    Vector3i -> Affine -> Map<string, ProcessParam> -> Consumer -> Chunk -> (ProcessEffect<'p, 'w> * Chunk) option

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

    static member val initial =
        { Processors = [||] }

type EditPlane =
    | XNeg | XPos
    | YNeg | YPos
    | ZNeg | ZPos

type Config =
    { CastShadows : bool }

    static member val initial =
        { CastShadows = true }

[<RequireQualifiedAccess>]
module BlockMap =

    type [<SymbolicExpansion>] BlockMap =
        private
            { Scale_ : Vector3
              Generated_ : bool
              LayersVisible_ : int
              EditPlane_ : EditPlane // plane currently containing cursor
              PaintHeight_ : int
              Cursor_ : Vector3i
              Selection_ : Selection
              StyleIndex_ : int
              Config_ : Config
              Palette_ : Palette
              Passes_ : Map<string, Pass>
              Chunk_ : Chunk }

        (* Properties *)

        member this.Scale = this.Scale_
        member this.Generated = this.Generated_
        member this.LayersVisible = this.LayersVisible_
        member this.EditPlane = this.EditPlane_
        member this.PaintHeight = this.PaintHeight_
        member this.Cursor = this.Cursor_
        member this.Selection = this.Selection_
        member this.StyleIndex = this.StyleIndex_
        member this.Config = this.Config_
        member this.Palette = this.Palette_
        member this.Passes = this.Passes_
        member this.Chunk = this.Chunk_
        member this.Size = this.Chunk_.BoundsI.Size.V3 * this.Scale_

    (* Low-Level API *)

    let setScale scale blockMap =
        { blockMap with Scale_ = scale }

    let mapScale mapper blockMap =
        setScale (mapper blockMap.Scale_) blockMap

    let setGenerated generated blockMap =
        { blockMap with Generated_ = generated }

    let mapGenerated mapper blockMap =
        { blockMap with Generated_ = mapper blockMap.Generated_ }

    let setLayersVisible layersVisible blockMap =
        { blockMap with LayersVisible_ = layersVisible }

    let mapLayersVisible mapper blockMap =
        { blockMap with LayersVisible_ = mapper blockMap.LayersVisible_ }

    let setEditPlane plane blockMap =
        { blockMap with EditPlane_ = plane }

    let mapEditPlane mapper blockMap =
        { blockMap with EditPlane_ = mapper blockMap.EditPlane_ }

    let setPaintHeight paintHeight blockMap =
        { blockMap with PaintHeight_ = max 1 paintHeight }

    let mapPaintHeight mapper blockMap =
        setPaintHeight (mapper blockMap.PaintHeight_) blockMap

    let setCursor (cursor : Vector3i) blockMap =
        if blockMap.Chunk_.BoundsI.ContainsExclusive cursor = ContainmentType.Disjoint then
            failwith "Cursor must be within the chunk bounds."
        { blockMap with Cursor_ = cursor }

    let mapCursor mapper blockMap =
        { blockMap with Cursor_ = mapper blockMap.Cursor_ }

    let setSelection selection blockMap =
        // TODO: check selection for appropriate boundedness.
        { blockMap with Selection_ = selection }

    let mapSelection mapper blockMap =
        { blockMap with Selection_ = mapper blockMap.Selection_ }

    let setStyleIndex styleIndex blockMap =
        if styleIndex < 0 || styleIndex >= Array.length blockMap.Palette_.Styles then
            failwith "Style index must be within the range of the block palette styles."
        { blockMap with StyleIndex_ = styleIndex }

    let mapStyleIndex mapper blockMap =
        setStyleIndex (mapper blockMap.StyleIndex_) blockMap

    let setConfig config blockMap =
        { blockMap with Config_ = config }

    let mapConfig mapper blockMap =
        setConfig (mapper blockMap.Config_) blockMap

    let setPalette palette blockMap =
        if palette.Styles.Length = 0 then
            failwith "Palette must contain at least one block style."
        let styleIndex =
            if blockMap.StyleIndex_ < Array.length palette.Styles
            then blockMap.StyleIndex_
            else 0
        { blockMap with
            Palette_ = palette
            StyleIndex_ = styleIndex }

    let mapPalette mapper blockMap =
        setPalette (mapper blockMap.Palette_) blockMap

    let setPasses passes blockMap =
        { blockMap with Passes_ = passes }

    let mapPasses mapper blockMap =
        setPasses (mapper blockMap.Passes_) blockMap

    let setChunk chunk blockMap =
        { blockMap with Chunk_ = chunk }

    let mapChunk mapper blockMap =
        { blockMap with Chunk_ = mapper blockMap.Chunk_ }

    let getBlockOpt positionI blockMap =
        Chunk.getBlockOpt positionI blockMap.Chunk_

    let setBlockOpt (positionI : Vector3i) blockOpt blockMap =
        { blockMap with Chunk_ = Chunk.setBlockOpt positionI blockOpt blockMap.Chunk_ }

    let mapBlockOpt mapper positionI blockMap =
        { blockMap with Chunk_ = Chunk.mapBlockOpt mapper positionI blockMap.Chunk_ }

    let getBlock positionI blockMap =
        Chunk.getBlock positionI blockMap.Chunk_

    let setBlock positionI block blockMap =
        { blockMap with Chunk_ = Chunk.setBlock positionI block blockMap.Chunk_ }

    let mapBlock mapper positionI blockMap =
        { blockMap with Chunk_ = Chunk.mapBlock mapper positionI blockMap.Chunk_ }

    (* Derived API *)

    let getBounds position (blockMap : BlockMap) =
        let size = blockMap.Size
        Box3 (position - size * 0.5f, size)

    let tryGetStyle styleIndex blockMap =
        Palette.tryGetStyle styleIndex blockMap.Palette_

    let tryGetSelectedColor blockMap =
        match tryGetStyle blockMap.StyleIndex_ blockMap with
        | Some style -> Some style.Color
        | None -> None

    let addPass passName pass blockMap =
        { blockMap with Passes_ = Map.add passName pass blockMap.Passes_ }

    let removePass passName blockMap =
        { blockMap with Passes_ = Map.remove passName blockMap.Passes_ }

    let tryGetBlockColor block blockMap =
        Block.tryGetColor block blockMap.Palette_

    (* High-Level API *)

    let tryPickPositionI (ray : Ray3) blockMapPosition (blockMap : BlockMap) =
        let bounds = getBounds blockMapPosition blockMap
        match blockMap.EditPlane_ with
        | YNeg | YPos ->
            let gridY = single blockMap.Cursor_.Y * single blockMap.Scale_.Y - bounds.Size.Y * 0.5f
            let gridCenter = bounds.Center + v3 0.0f gridY 0.0f
            let plane = Plane3 (gridCenter, v3Up)
            let intersectionTOpt = plane.Intersection ray
            if intersectionTOpt.HasValue then
                let intersectionT = intersectionTOpt.Value
                let intersection = intersectionT - bounds.Min
                if intersection.X >= 0.0f && intersection.Z >= 0.0f then
                    let positionI =
                        v3i
                            (int (intersection.X / blockMap.Scale_.X))
                            (blockMap.Cursor_.Y + if blockMap.EditPlane_.IsYNeg then -1 else 0)
                            (int (intersection.Z / blockMap.Scale_.Z))
                    if blockMap.Chunk_.BoundsI.ContainsExclusive positionI <> ContainmentType.Disjoint
                    then Some positionI
                    else None
                else None
            else None
        | XNeg | XPos ->
            let gridX = single blockMap.Cursor_.X * single blockMap.Scale_.X - bounds.Size.X * 0.5f
            let gridCenter = bounds.Center + v3 gridX 0.0f 0.0f
            let plane = Plane3 (gridCenter, v3Right)
            let intersectionTOpt = plane.Intersection ray
            if intersectionTOpt.HasValue then
                let intersectionT = intersectionTOpt.Value
                let intersection = intersectionT - bounds.Min
                if intersection.Y >= 0.0f && intersection.Z >= 0.0f then
                    let positionI =
                        v3i
                            (blockMap.Cursor_.X + if blockMap.EditPlane_.IsXNeg then -1 else 0)
                            (int (intersection.Y / blockMap.Scale_.Y))
                            (int (intersection.Z / blockMap.Scale_.Z))
                    if blockMap.Chunk_.BoundsI.ContainsExclusive positionI <> ContainmentType.Disjoint
                    then Some positionI
                    else None
                else None
            else None
        | ZNeg | ZPos ->
            let gridZ = single blockMap.Cursor_.Z * single blockMap.Scale_.Z - bounds.Size.Z * 0.5f
            let gridCenter = bounds.Center + v3 0.0f 0.0f gridZ
            let plane = Plane3 (gridCenter, v3Forward)
            let intersectionTOpt = plane.Intersection ray
            if intersectionTOpt.HasValue then
                let intersectionT = intersectionTOpt.Value
                let intersection = intersectionT - bounds.Min
                if intersection.X >= 0.0f && intersection.Y >= 0.0f then
                    let positionI =
                        v3i
                            (int (intersection.X / blockMap.Scale_.X))
                            (int (intersection.Y / blockMap.Scale_.Y))
                            (blockMap.Cursor_.Z + if blockMap.EditPlane_.IsZNeg then -1 else 0)
                    if blockMap.Chunk_.BoundsI.ContainsExclusive positionI <> ContainmentType.Disjoint
                    then Some positionI
                    else None
                else None
            else None

    let paint (positionI : Vector3i) blockMap =
        match tryGetStyle blockMap.StyleIndex_ blockMap with
        | Some style ->
            List.fold (fun (blockMap : BlockMap) i ->
                let offsetI =
                    match blockMap.EditPlane_ with
                    | XPos -> v3i i 0 0
                    | XNeg -> v3i -i 0 0
                    | YPos -> v3i 0 i 0
                    | YNeg -> v3i 0 -i 0
                    | ZPos -> v3i 0 0 i
                    | ZNeg -> v3i 0 0 -i
                let block = Block.make blockMap.StyleIndex_ 0 style.Properties
                setBlock (positionI + offsetI) block blockMap)
                blockMap [0 .. dec blockMap.PaintHeight_]
        | None -> blockMap

    let initial =
        let chunk = Chunk.initial
        { Scale_ = v3One
          Generated_ = false
          LayersVisible_ = chunk.BoundsI.Max.Y
          EditPlane_ = YPos
          PaintHeight_ = 1
          Cursor_ = chunk.BoundsI.Size / 2
          Selection_ = Selection.initial
          StyleIndex_ = 0
          Config_ = Config.initial
          Palette_ = Palette.initial
          Passes_ = Map.empty
          Chunk_ = chunk }

type BlockMap = BlockMap.BlockMap

[<RequireQualifiedAccess>]
module ProcessFns =

    let Nop _ _ _ _ block =
        Some ((fun _ _ -> ()), block)

    let ProcessFns<'p, 'w> : Map<string, ProcessFn<'p, 'w>> =
        [(nameof Nop, Nop)]
        |> Map.ofList