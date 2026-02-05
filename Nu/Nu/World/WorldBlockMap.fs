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

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module BlockEditor =

    let clear blockEditor (entity : Entity) world =
        for child in World.getChildren entity world do
            World.destroyImmediate child world
        { blockEditor with Generated = false }

    let generate blockEditor entity world =
        let mutable blockEditor = clear blockEditor entity world
        for pass in blockEditor.Passes.Values do
            for processor in pass.Processors do
                let affine = entity.GetAffineMatrixLocal world
                match World.tryProcessChunk affine processor blockEditor.BlockMap.Chunk entity world with
                | Some chunk -> blockEditor <- { blockEditor with BlockMap = { blockEditor.BlockMap with Chunk = chunk }}
                | None -> ()
        { blockEditor with Generated = true }

namespace Nu
open System
open System.Numerics
open Prime
open ImGuiNET
open ImGuizmoNET
open Nu
open Nu.BlockMap

[<AutoOpen>]
module BlockMapDispatcherExtensions =
    type Entity with
        member this.GetBlockEditor world : BlockEditor = this.Get (nameof this.BlockEditor) world
        member this.SetBlockEditor (value : BlockEditor) world = this.Set (nameof this.BlockEditor) value world
        member this.BlockEditor = lens (nameof BlockEditor) this this.GetBlockEditor this.SetBlockEditor

type BlockMapDispatcher () =
    inherit Entity3dDispatcher (false, false, false)

    static member Properties =
        [define Entity.BlockEditor BlockMap.BlockEditor.initial]

    override this.Render (renderPass, entity, world) =

        // render block editor when needed
        let blockEditor = entity.GetBlockEditor world
        if  renderPass.IsNormalPass ||
            blockEditor.Config.CastShadows && renderPass.IsShadowPass &&
            not blockEditor.Generated then

            // render blocks
            let blockMap = blockEditor.BlockMap
            let blockMapScale = blockMap.Scale
            let blockMapSize = blockMap.Size
            let bounds = entity.GetBounds world
            let material =
                { Material.empty with
                    AlbedoImageOpt = ValueSome Assets.Default.MaterialAlbedo
                    NormalImageOpt = ValueSome Assets.Default.MaterialNormal }
            for struct (positionI, block) in blockMap.Chunk.Blocks.Pairs' do
                let position = bounds.Center + positionI.V3 * blockMapScale - blockMapSize * 0.5f + blockMapScale * 0.5f
                let modelMatrix = Matrix4x4.CreateTranslation position
                let materialProperties = { MaterialProperties.empty with AlbedoOpt = ValueSome block.Color }
                World.renderStaticModelSurfaceFast
                    (&modelMatrix, blockEditor.Config.CastShadows, Omnipresent, ValueNone, &materialProperties, &material,
                     Assets.Default.StaticModel, 0, LessThanTest, DeferredRenderType, renderPass, world)

            // render cursor
            let io = ImGui.GetIO ()
            if not (io.WantCaptureMouseGlobal) then
                let position = entity.GetPosition world
                let ray = World.getMouseRay3dWorld world
                match BlockEditor.tryPickPositionI ray position blockEditor with
                | Some positionI ->
                    let position = bounds.Center + positionI.V3 * blockMapScale - blockMapSize * 0.5f + blockMapScale * 0.5f
                    let modelMatrix = Matrix4x4.CreateTranslation position
                    let style = blockEditor.Style
                    let color = if int world.DateTime.TimeOfDay.TotalMilliseconds % 666 < 333 then Color.CornflowerBlue else style.Color
                    let materialProperties = { MaterialProperties.empty with AlbedoOpt = ValueSome color }
                    World.renderStaticModelSurfaceFast
                        (&modelMatrix, false, Omnipresent, ValueNone, &materialProperties, &material,
                         Assets.Default.StaticModel, 0, LessThanTest, DeferredRenderType, renderPass, world)
                | None -> ()

    override this.Edit (op, entity, world) =
        match op with
        | ViewportOverlay viewportOverlay ->
            
            // use a mutable reference for tracking block editor's transformations
            let mutable blockEditor = entity.GetBlockEditor world

            // compute grid line segments
            let segments =
                let blockMap = blockEditor.BlockMap
                let scale = blockMap.Scale
                let boundsI = blockMap.Chunk.BoundsI
                let bounds = blockMap.Bounds (entity.GetPosition world)
                match blockEditor.EditPlane with
                | XPos | XNeg ->

                    [|// segments along Z (vertical lines in Y direction)
                      for i in boundsI.Min.Y .. boundsI.Max.Y do
                        let y = bounds.Min.Y + single i * scale.Y
                        let x = bounds.Min.X + single blockEditor.Cursor.PositionI.X * scale.X
                        let a = Vector3 (x, y, bounds.Min.Z)
                        let b = Vector3 (x, y, bounds.Max.Z)
                        Segment3 (a, b)

                      // segments along Y (horizontal lines in Z direction)
                      for i in boundsI.Min.Z .. boundsI.Max.Z do
                        let z = bounds.Min.Z + single i * scale.Z
                        let x = bounds.Min.X + single blockEditor.Cursor.PositionI.X * scale.X
                        let a = Vector3 (x, bounds.Min.Y, z)
                        let b = Vector3 (x, bounds.Max.Y, z)
                        Segment3 (a, b)|]
                        
                | YPos | YNeg ->

                    [|// segments along Z (vertical lines in X direction)
                      for i in boundsI.Min.X .. boundsI.Max.X do
                        let x = bounds.Min.X + single i * scale.X
                        let y = bounds.Min.Y + single blockEditor.Cursor.PositionI.Y * scale.Y
                        let a = Vector3 (x, y, bounds.Min.Z)
                        let b = Vector3 (x, y, bounds.Max.Z)
                        Segment3 (a, b)

                      // segments along X (horizontal lines in Z direction)
                      for i in boundsI.Min.Z .. boundsI.Max.Z do
                        let z = bounds.Min.Z + single i * scale.Z
                        let y = bounds.Min.Y + single blockEditor.Cursor.PositionI.Y * scale.Y
                        let a = Vector3 (bounds.Min.X, y, z)
                        let b = Vector3 (bounds.Max.X, y, z)
                        Segment3 (a, b)|]

                | ZPos | ZNeg ->

                    [|// segments along Y (vertical lines in X direction)
                      for i in boundsI.Min.X .. boundsI.Max.X do
                        let x = bounds.Min.X + single i * scale.X
                        let z = bounds.Min.Z + single blockEditor.Cursor.PositionI.Z * scale.Z
                        let a = Vector3 (x, bounds.Min.Y, z)
                        let b = Vector3 (x, bounds.Max.Y, z)
                        Segment3 (a, b)

                      // segments along X (horizontal lines in Y direction)
                      for i in boundsI.Min.Y .. boundsI.Max.Y do
                        let y = bounds.Min.Y + single i * scale.Y
                        let z = bounds.Min.Z + single blockEditor.Cursor.PositionI.Z * scale.Z
                        let a = Vector3 (bounds.Min.X, y, z)
                        let b = Vector3 (bounds.Max.X, y, z)
                        Segment3 (a, b)|]


            // draw grid line segments
            let gridColor = Color (64uy, 64uy, 64uy, 255uy) // TODO: make constant.
            World.imGuiSegments3d segments 1.0f gridColor world

            // edit block editor
            if ImGui.Begin "Block Editor" then

                // edit palette selection
                ImGui.Text "Style"
                let palette = blockEditor.Palette
                let styleIndex = blockEditor.PaletteSelection
                let styles = palette.Styles
                let style = styles.[styleIndex]
                let mutable color = style.Color.V4
                if ImGui.ColorEdit4 ("Palette Selection", &color, ImGuiColorEditFlags.NoLabel ||| ImGuiColorEditFlags.NoInputs) then
                    let styles = Array.removeAt styleIndex styles
                    let styles = Array.insertAt styleIndex { style with Color = Color color } styles
                    let palette = { palette with Styles = styles }
                    blockEditor <- BlockEditor.setPalette palette blockEditor
                ImGui.SameLine ()
                if styleIndex < 24 then
                    if ImGui.Button "Reset Color" then
                        () // TODO: reset to original color
                else
                    if ImGui.Button "Random Color" then
                        () // TODO: set to random color

                // select from palette
                ImGui.Text "Block Palette"
                let palette = blockEditor.Palette
                let styles = palette.Styles
                for i in 0 .. dec styles.Length do
                    let style = styles.[i]
                    if ImGui.ColorButton ("Style" + string i, style.Color.V4) then
                        blockEditor <- BlockEditor.setPaletteSelection i blockEditor
                    if  inc i % 12 <> 0 &&
                        inc i < styles.Length then
                        ImGui.SameLine ()
                    if  ImGui.IsItemHovered () &&
                        ImGui.IsMouseClicked ImGuiMouseButton.Right &&
                        i >= 24 then
                        let palette = Palette.removeStyle i blockEditor.Palette
                        blockEditor <- BlockEditor.setPalette palette blockEditor

                // augment palette
                if ImGui.Button "Add Style" then
                    let style = Style.make (Color (Random.Shared.NextSingle (), Random.Shared.NextSingle (), Random.Shared.NextSingle (), 1.0f)) "" Map.empty
                    let palette = Palette.addStyle style blockEditor.Palette
                    blockEditor <- BlockEditor.setPalette palette blockEditor
                    blockEditor <- BlockEditor.setPaletteSelection (dec palette.Styles.Length) blockEditor

                // edit plane
                let editPlaneName = scstringMemo blockEditor.EditPlane
                if ImGui.BeginCombo ("Block Plane", editPlaneName) then
                    let editPlaneNames = Seq.cast<string> (Reflection.getUnionCases typeof<EditPlane>).Keys
                    for name in editPlaneNames do
                        if ImGui.Selectable (name, (name = editPlaneName)) then
                            blockEditor <- BlockEditor.setEditPlane (scvalueMemo name) blockEditor
                    ImGui.EndCombo ()

                // edit visible layers
                let mutable layersVisible = blockEditor.LayersVisible
                if ImGui.SliderInt ("Layers Visible", &layersVisible, 0, 64) then
                    blockEditor <- BlockEditor.setLayersVisible layersVisible blockEditor

                // actions
                if not blockEditor.Generated then
                    if ImGui.Button "Generate" then
                        viewportOverlay.EditContext.Snapshot GenerateFromBlockMap world
                        blockEditor <- BlockEditor.generate blockEditor entity world
                else
                    if ImGui.Button "Clear" then
                        viewportOverlay.EditContext.Snapshot ClearBlocks world
                        blockEditor <- BlockEditor.clear blockEditor entity world

            ImGui.End ()

            // paint block when ungenerated
            if  not blockEditor.Generated &&
                World.isMouseButtonDown MouseLeft world then
                if World.isMouseButtonPressed MouseLeft world then
                    viewportOverlay.EditContext.Snapshot PaintBlocks world
                let position = entity.GetPosition world
                let ray = World.getMouseRay3dWorld world
                match BlockEditor.tryPickPositionI ray position blockEditor with
                | Some positionI ->
                    match BlockEditor.tryPaintBlock positionI blockEditor with
                    | Some blockEditor' -> blockEditor <- blockEditor'
                    | None -> ()
                | None -> ()

            // fin
            entity.SetBlockEditor blockEditor world

        | ReplaceProperty replaceProperty ->

            // replace when BlockEditor
            if  replaceProperty.PropertyDescriptor.PropertyName = "BlockEditor" &&
                replaceProperty.PropertyDescriptor.PropertyType = typeof<BlockEditor> then
                replaceProperty.IndicateReplaced ()

                // use a mutable reference for tracking block editor's transformations
                let mutable blockEditor = entity.GetBlockEditor world

                // edit passes
                ImGui.Text "Passes"
                let passes = blockEditor.Passes
                for (passName, pass) in passes.Pairs' do
                    ImGui.Text passName
                    for processor in pass.Processors do
                        ImGui.Indent ()
                        ImGui.Text processor.ProcessorName
                        let mutable processFnName = processor.ProcessFnName
                        if ImGui.InputText ("Process Fn Name##" + processor.ProcessorName, &processFnName, 4096u) then
                            let processor = { processor with ProcessFnName = processFnName }
                            let pass = Pass.replaceProcessor processor pass
                            blockEditor <- BlockEditor.addPass passName pass blockEditor
                        if ImGui.IsItemFocused () then replaceProperty.EditContext.FocusProperty ()
                        ImGui.Unindent ()
                    ImGui.Indent ()
                    if ImGui.Button ("Add Processor##" + passName) then
                        let processor = Processor.make Gen.name Map.empty (nameof ProcessFns.Id)
                        let pass = Pass.addProcessor processor pass
                        blockEditor <- BlockEditor.addPass passName pass blockEditor
                    if ImGui.IsItemFocused () then replaceProperty.EditContext.FocusProperty ()
                    ImGui.Unindent ()
                if ImGui.Button "Add Pass" then blockEditor <- BlockEditor.addPass Gen.name Pass.initial blockEditor
                if ImGui.IsItemFocused () then replaceProperty.EditContext.FocusProperty ()

                // fin
                entity.SetBlockEditor blockEditor world

        | _ -> ()

    override this.GetAttributesInferred (entity, world) =
        let blockEditor = entity.GetBlockEditor world
        let blockMap = blockEditor.BlockMap
        AttributesInferred.important blockMap.Size v3Zero