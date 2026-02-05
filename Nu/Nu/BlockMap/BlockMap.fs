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
         nameof Color.Brown; nameof Color.Chocolate
         nameof Color.Orange; nameof Color.OrangeRed
         nameof Color.Coral; nameof Color.IndianRed; nameof Color.Red; nameof Color.Maroon
         nameof Color.Purple; nameof Color.Indigo; nameof Color.Magenta; nameof Color.Orchid]

    static member BaseColorValues =
        List.map (fun name -> (typeof<Color>.GetProperty name).GetValue null :?> Color) Palette.BaseColorNames

    static member tryGetStyle color (palette : Palette) =
        Array.tryFind (fun style -> style.Color = color) palette.Styles

    static member getStyle index palette =
        palette.Styles.[index]

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
    { Color : Color // also used to look up Style
      ColorShift : int // -3 .. +3. Each shift can signify an additional level of adornment, such as stacking a pen on a book on a desk.
      Properties : Map<string, TypeName * Symbol> }

    static member make color colorShift properties =
        { Color = color; ColorShift = colorShift; Properties = properties }

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

type BlockMap =
    { Scale : Vector3
      Chunk : Chunk }

    member this.Size =
        this.Chunk.BoundsI.Size.V3 * this.Scale

    member this.Bounds position =
        let size = this.Size
        Box3 (position - size * 0.5f, size)

    static member tryGetBlock positionI map =
        Chunk.tryGetBlock positionI map.Chunk

    static member trySetBlock (positionI : Vector3i) block map =
        match Chunk.trySetBlock positionI block map.Chunk with
        | Some chunk -> Some { map with Chunk = chunk }
        | None -> None

    static member initial =
        { Scale = Vector3.One
          Chunk = Chunk.make (box3i v3iZero (v3iDup 32)) Map.empty }

type Cursor =
    { PositionI : Vector3i }

    static member initial =
        { PositionI = v3iDup 16 }

type Selection =
    | SelectionVolume of Vector3i * Vector3i
    | SelectionAdHoc of Vector3i Set

    static member initial =
        SelectionAdHoc Set.empty

type OutputFnCall =
    { AffineLocal : Affine
      OutputFnParams : Symbol
      OutputFnName : string }

type OutputRigidModel =
    { AffineLocal : Affine
      StaticModel : StaticModel AssetTag
      BodyType : BodyType }

type OutputRigidModelSurface =
    { AffineLocal : Affine
      StaticModel : StaticModel AssetTag
      SurfaceIndex : int
      BodyType : BodyType }

type Output =
    | OutputFnCall of OutputFnCall
    | OutputRigidModel of OutputRigidModel
    | OutputRigidModelSurface of OutputRigidModelSurface
    | Outputs of Output list

type OutputFn<'w> =
    Symbol -> string -> Affine -> Symbol -> 'w -> unit // receives combined map affine and local affine

type Processor =
    { ProcessorName : string
      ProcessingVolume : Vector3i
      MatchFnName : string
      EvalFnName : string }

    static member make name volume matchFnName evalFnName =
        { ProcessorName = name
          ProcessingVolume = volume
          MatchFnName = matchFnName
          EvalFnName = evalFnName }

type MatchFn =
    Vector3i -> Chunk -> bool

type EvalFn =
    Vector3i -> Chunk -> Output * (*leftovers/replacements*) Chunk // replacements allow for additional passes, such as wall decoration and object stacking

type Pass =
    { Processors : Processor array }

    static member addProcessor processor pass =
        { pass with Processors = Array.add processor pass.Processors }

    static member tryGetProcessor name pass =
        Array.tryFind (fun processor -> processor.ProcessorName = name) pass.Processors

    static member initial =
        { Processors = [||] }

type EditPlane =
    | XNeg | XPos | YNeg | YPos | ZNeg | ZPos

type BlockEditor =
    { EditPlane : EditPlane // plane currently containing cursor
      LayersVisible : int
      Cursor : Cursor
      Selection : Selection
      Palette : Palette
      PaletteSelection : int
      Passes : Map<string, Pass>
      BlockMap : BlockMap }

    member this.Style =
        Palette.getStyle this.PaletteSelection this.Palette

    static member setBlockMap blockMap editor =
        if blockMap.Chunk.Blocks.Count = 0 then
            failwith "Block map must contain a block chunk with at least one block."
        { editor with BlockMap = blockMap }

    static member setEditPlane plane editor =
        { editor with EditPlane = plane }

    static member setLayersVisible layersVisible editor =
        { editor with LayersVisible = layersVisible }

    static member setCursor cursor editor =
        if editor.BlockMap.Chunk.BoundsI.ContainsExclusive cursor.PositionI = ContainmentType.Disjoint then
            failwith "Block cursor position must be within the block map bounds."
        { editor with Cursor = cursor }

    static member setSelection selection editor =
        // TODO: check selection for appropriate boundedness.
        { editor with Selection = selection }

    static member setPalette palette editor =
        if palette.Styles.Length = 0 then
            failwith "Block palette must contain at least one block style."
        let paletteSelection =
            if editor.PaletteSelection < Array.length palette.Styles
            then editor.PaletteSelection
            else 0
        { editor with
            Palette = palette
            PaletteSelection = paletteSelection }

    static member setPaletteSelection paletteSelection editor =
        if paletteSelection < 0 || paletteSelection >= Array.length editor.Palette.Styles then
            failwith "Block palette selection must be within the range of the block palette styles."
        { editor with PaletteSelection = paletteSelection }

    static member tryPickPositionI (ray : Ray3) blockMapPosition editor =
        let bounds = editor.BlockMap.Bounds blockMapPosition
        match editor.EditPlane with
        | YNeg | YPos ->
            let gridY = single editor.Cursor.PositionI.Y * single editor.BlockMap.Scale.Y - bounds.Size.Y * 0.5f
            let gridCenter = bounds.Center + v3 0.0f gridY 0.0f
            let plane = Plane3 (gridCenter, v3Up)
            let tOpt = plane.Intersection ray
            if tOpt.HasValue then
                let t = tOpt.Value
                let position = t - bounds.Min
                let positionI =
                    v3i
                        (int (position.X / editor.BlockMap.Scale.X))
                        editor.Cursor.PositionI.Y
                        (int (position.Z / editor.BlockMap.Scale.Z))
                if editor.BlockMap.Chunk.BoundsI.ContainsExclusive positionI <> ContainmentType.Disjoint
                then Some positionI
                else None
            else None
        | XNeg | XPos ->
            let gridX = single editor.Cursor.PositionI.X * single editor.BlockMap.Scale.X - bounds.Size.X * 0.5f
            let gridCenter = bounds.Center + v3 gridX 0.0f 0.0f
            let plane = Plane3 (gridCenter, v3Right)
            let tOpt = plane.Intersection ray
            if tOpt.HasValue then
                let t = tOpt.Value
                let position = t - bounds.Min
                let positionI =
                    v3i
                        editor.Cursor.PositionI.X
                        (int (position.Y / editor.BlockMap.Scale.Y))
                        (int (position.Z / editor.BlockMap.Scale.Z))
                if editor.BlockMap.Chunk.BoundsI.ContainsExclusive positionI <> ContainmentType.Disjoint
                then Some positionI
                else None
            else None
        | ZNeg | ZPos ->
            let gridZ = single editor.Cursor.PositionI.Z * single editor.BlockMap.Scale.Z - bounds.Size.Z * 0.5f
            let gridCenter = bounds.Center + v3 0.0f 0.0f gridZ
            let plane = Plane3 (gridCenter, v3Forward)
            let tOpt = plane.Intersection ray
            if tOpt.HasValue then
                let t = tOpt.Value
                let position = t - bounds.Min
                let positionI =
                    v3i
                        (int (position.X / editor.BlockMap.Scale.X))
                        (int (position.Y / editor.BlockMap.Scale.Y))
                        editor.Cursor.PositionI.Z
                if editor.BlockMap.Chunk.BoundsI.ContainsExclusive positionI <> ContainmentType.Disjoint
                then Some positionI
                else None
            else None

    static member tryPaintBlock positionI editor =
        let blockMap = editor.BlockMap
        let style = editor.Style
        let block = Block.make style.Color 0 style.Properties
        match BlockMap.trySetBlock positionI block blockMap with
        | Some blockMap -> Some (BlockEditor.setBlockMap blockMap editor)
        | None -> None

    static member addPass passName pass editor =
        { editor with Passes = Map.add passName pass editor.Passes }

    static member removePass passName editor =
        { editor with Passes = Map.remove passName editor.Passes }

    static member initial =
        { EditPlane = YPos
          LayersVisible = 32
          Cursor = Cursor.initial
          Selection = Selection.initial
          Palette = Palette.initial
          PaletteSelection = 0
          Passes = Map.empty
          BlockMap = BlockMap.initial }