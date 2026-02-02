// Nu Game Engine.
// Required Notice:
// Copyright (C) Bryan Edds.
// Nu Game Engine is licensed under the Nu Game Engine Noncommercial License.
// See https://github.com/bryanedds/Nu/blob/master/License.md.

namespace Nu
open System
open System.Numerics
open Prime
open ImGuiNET
open Nu

[<AutoOpen>]
module BlockMapDispatcherExtensions =
    type Entity with
        member this.GetBlockEditor world : BlockEditor = this.Get (nameof BlockEditor) world
        member this.SetBlockEditor (value : BlockEditor) world = this.Set (nameof BlockEditor) value world
        member this.BlockEditor = lens (nameof BlockEditor) this this.GetBlockEditor this.SetBlockEditor

type BlockMapDispatcher () =
    inherit Entity3dDispatcher (false, false, false)

    static member Properties =
        [define Entity.BlockEditor BlockEditor.initial]

    override this.Render (renderPass, entity, world) =
        match renderPass with
        | NormalPass ->
            let blockEditor = entity.GetBlockEditor world
            let blockMap = blockEditor.BlockMap
            let blockScale = blockMap.BlockScale
            let blockMapSize = blockMap.Size
            let bounds = entity.GetBounds world
            let material = Material.empty
            for struct (blockPositionI, block) in blockMap.BlockChunk.Blocks.Pairs' do
                let blockPosition = bounds.Center + blockPositionI.V3 * blockScale - blockMapSize * 0.5f + blockScale * 0.5f
                let blockTransform = Matrix4x4.CreateTranslation blockPosition
                let materialProperties = { MaterialProperties.empty with AlbedoOpt = ValueSome block.BlockColor }
                World.renderStaticModelSurfaceFast
                    (&blockTransform, false, Omnipresent, ValueNone, &materialProperties, &material,
                     Assets.Default.StaticModel, 0, LessThanTest, DeferredRenderType, renderPass, world)
        | _ -> ()

    override this.Edit (op, entity, world) =
        match op with
        | ViewportOverlay _ ->

            // compute grid line segments
            let segments =
                let blockEditor = entity.GetBlockEditor world
                let blockMap = blockEditor.BlockMap
                let blockBounds = blockMap.BlockChunk.BlockBounds
                let blockScale = blockMap.BlockScale
                let blockMapSize = blockMap.Size
                let bounds = entity.GetBounds world
                let blockMapBounds = Box3 (bounds.Center - blockMapSize * 0.5f, blockMapSize)
                match blockEditor.BlockPlane with
                | XPos | XNeg ->

                    [|// compute segments down z axis
                      for i in blockBounds.Min.Y .. blockBounds.Max.Y do
                        let y = single i * blockScale.Y - blockMapSize.Y * 0.5f
                        let x = single blockEditor.BlockCursor.BlockPosition.X * blockScale.X + blockMapBounds.Min.X
                        let a = Vector3 (x, y, blockMapBounds.Min.Z)
                        let b = Vector3 (x, y, blockMapBounds.Max.Z)
                        Segment3 (a, b)

                      // compute segments down y axis
                      for i in blockBounds.Min.Z .. blockBounds.Max.Z do
                        let z = single i * blockScale.Z - blockMapSize.Z * 0.5f
                        let x = single blockEditor.BlockCursor.BlockPosition.X * blockScale.X + blockMapBounds.Min.X
                        let a = Vector3 (x, blockMapBounds.Min.Y, z)
                        let b = Vector3 (x, blockMapBounds.Max.Y, z)
                        Segment3 (a, b)|]

                | YPos | YNeg ->

                    [|// compute segments down z axis
                      for i in blockBounds.Min.X .. blockBounds.Max.X do
                        let x = single i * blockScale.X - blockMapSize.X * 0.5f
                        let y = single blockEditor.BlockCursor.BlockPosition.Y * blockScale.Y + blockMapBounds.Min.Y
                        let a = Vector3 (x, y, blockMapBounds.Min.Z)
                        let b = Vector3 (x, y, blockMapBounds.Max.Z)
                        Segment3 (a, b)

                      // compute segments down x axis
                      for i in blockBounds.Min.Z .. blockBounds.Max.Z do
                        let z = single i * blockScale.Z - blockMapSize.Z * 0.5f
                        let y = single blockEditor.BlockCursor.BlockPosition.Y * blockScale.Y + blockMapBounds.Min.Y
                        let a = Vector3 (blockMapBounds.Min.X, y, z)
                        let b = Vector3 (blockMapBounds.Max.X, y, z)
                        Segment3 (a, b)|]

                | ZPos | ZNeg ->

                    [|// compute segments down y axis
                      for i in blockBounds.Min.X .. blockBounds.Max.X do
                        let x = single i * blockScale.X - blockMapSize.X * 0.5f
                        let z = single blockEditor.BlockCursor.BlockPosition.Z * blockScale.Z + blockMapBounds.Min.Z
                        let a = Vector3 (x, blockMapBounds.Min.Y, z)
                        let b = Vector3 (x, blockMapBounds.Max.Y, z)
                        Segment3 (a, b)

                      // compute segments down x axis
                      for i in blockBounds.Min.Y .. blockBounds.Max.Y do
                        let y = single i * blockScale.Y - blockMapSize.Y * 0.5f
                        let z = single blockEditor.BlockCursor.BlockPosition.Z * blockScale.Z + blockMapBounds.Min.Z
                        let a = Vector3 (blockMapBounds.Min.X, y, z)
                        let b = Vector3 (blockMapBounds.Max.X, y, z)
                        Segment3 (a, b)|]

            // draw grid line segments
            let gridColor = Color (64uy, 64uy, 64uy, 255uy) // TODO: make constant.
            World.imGuiSegments3d segments 1.0f gridColor world

            // edit block editor
            if ImGui.Begin "Block Editor" then
            
                // use a mutable reference for tracking block editor's transformations
                let mutable blockEditor = entity.GetBlockEditor world

                // edit palette selection
                ImGui.Text "Style"
                let palette = blockEditor.BlockPalette
                let styleIndex = blockEditor.BlockPaletteSelection
                let styles = palette.BlockStyles
                let style = styles.[styleIndex]
                let mutable color = style.BlockColor.V4
                if ImGui.ColorEdit4 ("Palette Selection", &color, ImGuiColorEditFlags.NoLabel ||| ImGuiColorEditFlags.NoInputs) then
                    let styles = Array.removeAt styleIndex styles
                    let styles = Array.insertAt styleIndex { style with BlockColor = Color color } styles
                    let palette = { palette with BlockStyles = styles }
                    blockEditor <- BlockEditor.setBlockPalette palette blockEditor
                ImGui.SameLine ()
                if styleIndex < 24 then
                    if ImGui.Button "Reset Color" then
                        () // TODO: reset to original color
                else
                    if ImGui.Button "Random Color" then
                        () // TODO: set to random color

                // select from palette
                ImGui.Text "Block Palette"
                let palette = blockEditor.BlockPalette
                let styles = palette.BlockStyles
                for i in 0 .. dec styles.Length do
                    let style = styles.[i]
                    if ImGui.ColorButton ("Style" + string i, style.BlockColor.V4) then
                        blockEditor <- BlockEditor.setBlockPaletteSelection i blockEditor
                    if  inc i % 12 <> 0 &&
                        inc i < styles.Length then
                        ImGui.SameLine ()
                    if  ImGui.IsItemHovered () &&
                        ImGui.IsMouseClicked ImGuiMouseButton.Right &&
                        i >= 24 then
                        let palette = BlockPalette.removeStyle i blockEditor.BlockPalette
                        blockEditor <- BlockEditor.setBlockPalette palette blockEditor

                // augment palette
                if ImGui.Button "Add Style" then
                    let style = BlockStyle.make (Color (Random.Shared.NextSingle (), Random.Shared.NextSingle (), Random.Shared.NextSingle (), 1.0f)) "" Map.empty
                    let palette = BlockPalette.addStyle style blockEditor.BlockPalette
                    blockEditor <- BlockEditor.setBlockPalette palette blockEditor
                    blockEditor <- BlockEditor.setBlockPaletteSelection (dec palette.BlockStyles.Length) blockEditor

                // edit plane
                let blockPlaneName = scstringMemo blockEditor.BlockPlane
                if ImGui.BeginCombo ("Block Plane", blockPlaneName) then
                    let blockPlaneNames = Seq.cast<string> (Reflection.getUnionCases typeof<BlockPlane>).Keys
                    for name in blockPlaneNames do
                        if ImGui.Selectable (name, (name = blockPlaneName)) then
                            blockEditor <- BlockEditor.setBlockPlane (scvalueMemo name) blockEditor
                    ImGui.EndCombo ()

                // edit visible layers
                let mutable layersVisible = blockEditor.BlockLayersVisible
                if ImGui.SliderInt ("Layers Visible", &layersVisible, 0, 64) then
                    blockEditor <- BlockEditor.setBlockLayersVisible layersVisible blockEditor

                // edit passes
                ImGui.Text "Passes"
                let passes = blockEditor.BlockPasses
                for (passName, pass) in passes.Pairs' do
                    ImGui.Text passName
                    for processor in pass.BlockProcessors do
                        ImGui.Indent ()
                        ImGui.Text processor.BlockProcessorName
                        let mutable matchFnName = processor.BlockMatchFnName
                        let mutable evalFnName = processor.BlockEvalFnName
                        ImGui.InputText ("Match Fn Name##" + processor.BlockProcessorName, &matchFnName, 4096u) |> ignore<bool>
                        ImGui.InputText ("Eval Fn Name##" + processor.BlockProcessorName, &evalFnName, 4096u) |> ignore<bool>
                        ImGui.Unindent ()
                    ImGui.Indent ()
                    if ImGui.Button ("Add Processor##" + passName) then
                        let processor = BlockProcessor.make Gen.name v3iOne "Tautology" "Id"
                        let pass = BlockPass.addProcessor processor pass
                        blockEditor <- BlockEditor.addPass passName pass blockEditor
                    ImGui.Unindent ()
                if ImGui.Button "Add Pass" then
                    blockEditor <- BlockEditor.addPass Gen.name BlockPass.initial blockEditor

                // fin
                entity.SetBlockEditor blockEditor world

            ImGui.End ()

        | _ -> ()

    override this.GetAttributesInferred (entity, world) =
        let blockEditor = entity.GetBlockEditor world
        let blockMap = blockEditor.BlockMap
        AttributesInferred.important blockMap.Size v3Zero