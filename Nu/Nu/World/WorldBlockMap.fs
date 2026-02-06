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

    /// World extensions.
    type World with

        /// Attempt to apply the given block granulator function.
        static member tryGranulateChunk chunk granulatorFnName (world : World) =
            match world.WorldExtension.Plugin.GranulatorFns.TryGetValue granulatorFnName with
            | (true, (volume, fn)) -> Some (fn volume chunk)
            | (false, _) -> None

        /// Attempt to apply the given block combiner function.
        static member tryCombineChunk chunk combinerFnName (world : World) =
            match world.WorldExtension.Plugin.CombinerFns.TryGetValue combinerFnName with
            | (true, (volume, fn)) -> Some (fn volume chunk)
            | (false, _) -> None

        /// Attempt to find the given block process function.
        static member tryProcessChunk
            (affine : Affine)
            (processor : BlockMap.Processor)
            (chunk : BlockMap.Chunk)
            (blockMap : BlockMap.BlockMap)
            (parent : Entity)
            (world : World) =
            match world.WorldExtension.Plugin.ProcessFns.TryGetValue processor.ProcessFnName with
            | (true, (volume, fn)) ->
                let mutable chunk = chunk
                for i in inc -volume.X .. dec (chunk.BoundsI.Size.X + dec volume.X) do
                    for j in inc -volume.Y .. dec (chunk.BoundsI.Size.Y + dec volume.Y) do
                        for k in inc -volume.Z .. dec (chunk.BoundsI.Size.Z + dec volume.Z) do
                            let positionI = v3i i j k
                            let chunkBounds = box3i positionI volume
                            let blocks =
                                [|for x in 0 .. dec volume.X do
                                    for y in 0 .. dec volume.Y do
                                        for z in 0 .. dec volume.Z do
                                            let positionI = v3i x y z
                                            let positionI' = chunkBounds.Min + positionI
                                            match BlockMap.Chunk.tryGetBlock positionI' chunk with
                                            | Some block -> (positionI, block)
                                            | None -> ()|]
                                |> Map.ofArray
                            let affine =
                                let parentPosition = parent.GetPosition world
                                let blockMapBounds = blockMap.Bounds parentPosition
                                let translation = parentPosition + affine.Translation + positionI.V3 * blockMap.Scale - blockMapBounds.Size * 0.5f + blockMap.Scale * 1.5f
                                Affine.makeTranslation translation
                            let subchunk =
                                BlockMap.Chunk.make chunkBounds blocks
                            let subchunk' =
                                match fn volume affine processor.ProcessParams subchunk with
                                | Some (effect, subchunk') -> effect parent world; subchunk'
                                | None -> subchunk
                            for struct (positionI, block) in subchunk'.Blocks.Pairs' do
                                match BlockMap.Chunk.trySetBlock (chunkBounds.Min + positionI) block chunk with
                                | Some chunk' -> chunk <- chunk'
                                | None -> ()
                Some chunk
            | (false, _) -> None

    let clear blockEditor (entity : Entity) world =
        for child in World.getChildren entity world do
            World.destroyImmediate child world
        { blockEditor with Generated = false }

    let generate blockEditor entity world =
        let mutable blockEditor = clear blockEditor entity world
        for pass in blockEditor.Passes.Values do
            for processor in pass.Processors do
                let affine = Affine.make (entity.GetPosition world) (entity.GetRotation world) (entity.GetScale world)
                match World.tryProcessChunk affine processor blockEditor.BlockMap.Chunk blockEditor.BlockMap entity world with
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
        if  (renderPass.IsNormalPass || blockEditor.Config.CastShadows && renderPass.IsShadowPass) &&
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
                match BlockEditor.tryGetBlockColor block blockEditor with
                | Some color ->

                    // render full block
                    if block.StyleIndex < 12 || block.StyleIndex >= 18 then // TODO: make this a style property instead.
                        let position = bounds.Center + positionI.V3 * blockMapScale - blockMapSize * 0.5f + blockMapScale * 0.5f
                        let modelMatrix = Matrix4x4.CreateTranslation position
                        let materialProperties = { MaterialProperties.empty with AlbedoOpt = ValueSome color }
                        World.renderStaticModelSurfaceFast
                            (&modelMatrix, blockEditor.Config.CastShadows, Omnipresent, ValueNone, &materialProperties, &material,
                             Assets.Default.StaticModel, 0, LessThanTest, DeferredRenderType, renderPass, world)

                    // render quarter block, such as for floors/ceilings
                    else
                        let position = bounds.Center + positionI.V3 * blockMapScale - blockMapSize * 0.5f + blockMapScale * 0.5f + blockMapScale * v3 0.0f 0.5f 0.0f
                        let scale = Vector3 (1.0f, 0.25f, 1.0f)
                        let modelMatrix = Matrix4x4.CreateAffine (position, quatIdentity, scale) 
                        let materialProperties = { MaterialProperties.empty with AlbedoOpt = ValueSome color }
                        World.renderStaticModelSurfaceFast
                            (&modelMatrix, blockEditor.Config.CastShadows, Omnipresent, ValueNone, &materialProperties, &material,
                             Assets.Default.StaticModel, 0, LessThanTest, DeferredRenderType, renderPass, world)

                | None -> ()

            // render cursor
            let io = ImGui.GetIO ()
            if not (io.WantCaptureMouseGlobal) then
                let position = entity.GetPosition world
                let ray = World.getMouseRay3dWorld world
                match BlockEditor.tryPickPositionI ray position blockEditor with
                | Some positionI ->
                    match BlockEditor.tryGetSelectedColor blockEditor with
                    | Some color ->

                        // render full block
                        let colorBlinking = if int world.DateTime.TimeOfDay.TotalMilliseconds % 666 < 333 then Color.CornflowerBlue else color
                        let materialProperties = { MaterialProperties.empty with AlbedoOpt = ValueSome colorBlinking }
                        if blockEditor.PaletteSelection < 12 || blockEditor.PaletteSelection >= 18 then // TODO: make this a style property instead.
                            let position = bounds.Center + positionI.V3 * blockMapScale - blockMapSize * 0.5f + blockMapScale * 0.5f
                            let modelMatrix = Matrix4x4.CreateTranslation position
                            World.renderStaticModelSurfaceFast
                                (&modelMatrix, blockEditor.Config.CastShadows, Omnipresent, ValueNone, &materialProperties, &material,
                                 Assets.Default.StaticModel, 0, LessThanTest, DeferredRenderType, renderPass, world)

                        // render quarter block, such as for floors/ceilings
                        else
                            let position = bounds.Center + positionI.V3 * blockMapScale - blockMapSize * 0.5f + blockMapScale * 0.5f + blockMapScale * v3 0.0f 0.5f 0.0f
                            let scale = Vector3 (1.0f, 0.25f, 1.0f)
                            let modelMatrix = Matrix4x4.CreateAffine (position, quatIdentity, scale)
                            let materialProperties = { MaterialProperties.empty with AlbedoOpt = ValueSome colorBlinking }
                            World.renderStaticModelSurfaceFast
                                (&modelMatrix, blockEditor.Config.CastShadows, Omnipresent, ValueNone, &materialProperties, &material,
                                 Assets.Default.StaticModel, 0, LessThanTest, DeferredRenderType, renderPass, world)

                    | None -> ()
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

            // finish block editor window
            ImGui.End ()

            // paint block when ungenerated
            if  not blockEditor.Generated &&
                World.isMouseButtonDown MouseLeft world then
                if World.isMouseButtonPressed MouseLeft world then viewportOverlay.EditContext.Snapshot PaintBlocks world
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