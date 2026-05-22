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

[<RequireQualifiedAccess>]
module WorldBlockMap =

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

        /// Process a block map with the given processor.
        static member processBlockMap
            (affine : Affine)
            (processor : BlockMap.Processor)
            (consumer : BlockMap.Consumer)
            (blockMap : BlockMap.BlockMap)
            (parent : Entity)
            (world : World) =
            match world.WorldExtension.Plugin.ProcessFns.TryGetValue processor.ProcessFnName with
            | (true, (volume, fn)) ->
                BlockMap.BlockMap.mapChunk (fun chunk ->
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
                                                match Chunk.getBlockOpt positionI' chunk with
                                                | Some block -> (positionI, block)
                                                | None -> ()|]
                                    |> Map.ofArray
                                let affine =
                                    let parentPosition = parent.GetPosition world
                                    let blockMapBounds = BlockMap.getBounds parentPosition blockMap
                                    let translation = parentPosition + affine.Translation + positionI.V3 * blockMap.Scale - blockMapBounds.Size * 0.5f + blockMap.Scale * 0.5f
                                    Affine.makeTranslation translation
                                let subchunk =
                                    BlockMap.Chunk.make chunkBounds blocks
                                let subchunk' =
                                    match fn volume affine processor.ProcessParams consumer subchunk with
                                    | Some (effect, subchunk') -> effect parent world; subchunk'
                                    | None -> subchunk
                                for struct (positionI, block) in subchunk'.Blocks.Pairs' do
                                    let positionI = positionI + chunkBounds.Min
                                    chunk <- BlockMap.Chunk.setBlock positionI block chunk
                    chunk)
                    blockMap
            | (false, _) -> blockMap

    let clear blockMap (entity : Entity) world =
        for child in World.getChildren entity world do
            World.destroyImmediate child world
        BlockMap.setGenerated false blockMap

    let generate blockMap entity world =
        let mutable blockMap = clear blockMap entity world
        for (passName, pass) in blockMap.Passes.Pairs' do
            for processor in pass.Processors do
                let consumer = Consumer.make passName processor.ProcessorName
                let affine = Affine.make (entity.GetPosition world) (entity.GetRotation world) (entity.GetScale world)
                blockMap <- World.processBlockMap affine processor consumer blockMap entity world
        BlockMap.setGenerated true blockMap

namespace Nu
open System
open System.Numerics
open Prime
open ImGuiNET
open Nu
open Nu.BlockMap

[<AutoOpen>]
module BlockMapDispatcherExtensions =
    type Entity with
        member this.GetBlockMap world : BlockMap = this.Get (nameof this.BlockMap) world
        member this.SetBlockMap (value : BlockMap) world = this.Set (nameof this.BlockMap) value world
        member this.BlockMap = lens (nameof BlockMap) this this.GetBlockMap this.SetBlockMap

type BlockMapDispatcher () =
    inherit Entity3dDispatcher (false, false, false)

    static member Properties =
        [define Entity.Presence Omnipresent
         define Entity.BlockMap BlockMap.initial]

    override this.Render (renderPass, entity, world) =

        // render block editor when needed
        let blockMap = entity.GetBlockMap world
        if  (renderPass.IsNormalPass || blockMap.Config.CastShadows && renderPass.IsShadowPass) &&
            not blockMap.Generated then

            // render blocks
            let chunk = blockMap.Chunk
            let blockMapScale = blockMap.Scale
            let blockMapSize = blockMap.Size
            let bounds = entity.GetBounds world
            let material =
                { Material.empty with
                    AlbedoImageOpt = ValueSome Assets.Default.MaterialAlbedo
                    NormalImageOpt = ValueSome Assets.Default.MaterialNormal }
            for struct (positionI, block) in chunk.Blocks.Pairs' do
                match BlockMap.tryGetBlockColor block blockMap with
                | Some color ->
                    let position = bounds.Center + positionI.V3 * blockMapScale - blockMapSize * 0.5f + blockMapScale * 0.5f
                    let modelMatrix = Matrix4x4.CreateTranslation position
                    let materialProperties = { MaterialProperties.empty with AlbedoOpt = ValueSome color }
                    World.renderStaticModelSurfaceFast
                        (&modelMatrix, blockMap.Config.CastShadows, Omnipresent, ValueNone, &materialProperties, &material,
                         Assets.Default.StaticModel, 0, LessThanTest, DeferredRenderType, renderPass, world)
                | None -> ()

            // render cursor when needed
            let io = ImGui.GetIO ()
            match world.EditContextOpt with
            | Some editContext when editContext.SelectedEntityOpt = Some entity && not io.WantCaptureMouseGlobal ->
                let position = entity.GetPosition world
                let ray = World.getMouseRay3dWorld world
                match BlockMap.tryPickPositionI ray position blockMap with
                | Some positionI ->
                    match BlockMap.tryGetSelectedColor blockMap with
                    | Some color ->
                        let colorBlinking = if int world.DateTime.TimeOfDay.TotalMilliseconds % 666 < 333 then Color.CornflowerBlue else color
                        let materialProperties = { MaterialProperties.empty with AlbedoOpt = ValueSome colorBlinking }
                        for i in 0 .. dec blockMap.PaintHeight do
                            let position = bounds.Center + positionI.V3 * blockMapScale - blockMapSize * 0.5f + blockMapScale * 0.5f
                            let offsetI =
                                match blockMap.EditPlane with
                                | XPos -> v3i i 0 0
                                | XNeg -> v3i -i 0 0
                                | YPos -> v3i 0 i 0
                                | YNeg -> v3i 0 -i 0
                                | ZPos -> v3i 0 0 i
                                | ZNeg -> v3i 0 0 -i
                            let offset = offsetI.V3 * blockMapScale
                            let modelMatrix = Matrix4x4.CreateTranslation (position + offset)
                            World.renderStaticModelSurfaceFast
                                (&modelMatrix, blockMap.Config.CastShadows, Omnipresent, ValueNone, &materialProperties, &material,
                                 Assets.Default.StaticModel, 0, LessThanTest, DeferredRenderType, renderPass, world)
                    | None -> ()
                | None -> ()
            | Some _ | None -> ()

    override this.Edit (op, entity, world) =
        match op with
        | ViewportOverlay viewportOverlay ->
            
            // use a mutable reference for tracking block editor's transformations
            let mutable blockMap = entity.GetBlockMap world

            // compute grid line segments
            let segments =
                let scale = blockMap.Scale
                let boundsI = blockMap.Chunk.BoundsI
                let bounds = BlockMap.getBounds (entity.GetPosition world) blockMap
                match blockMap.EditPlane with
                | XPos | XNeg ->

                    [|// segments along Z (vertical lines in Y direction)
                      for i in boundsI.Min.Y .. boundsI.Max.Y do
                        let y = bounds.Min.Y + single i * scale.Y
                        let x = bounds.Min.X + single blockMap.Cursor.X * scale.X
                        let a = Vector3 (x, y, bounds.Min.Z)
                        let b = Vector3 (x, y, bounds.Max.Z)
                        Segment3 (a, b)

                      // segments along Y (horizontal lines in Z direction)
                      for i in boundsI.Min.Z .. boundsI.Max.Z do
                        let z = bounds.Min.Z + single i * scale.Z
                        let x = bounds.Min.X + single blockMap.Cursor.X * scale.X
                        let a = Vector3 (x, bounds.Min.Y, z)
                        let b = Vector3 (x, bounds.Max.Y, z)
                        Segment3 (a, b)|]
                        
                | YPos | YNeg ->

                    [|// segments along Z (vertical lines in X direction)
                      for i in boundsI.Min.X .. boundsI.Max.X do
                        let x = bounds.Min.X + single i * scale.X
                        let y = bounds.Min.Y + single blockMap.Cursor.Y * scale.Y
                        let a = Vector3 (x, y, bounds.Min.Z)
                        let b = Vector3 (x, y, bounds.Max.Z)
                        Segment3 (a, b)

                      // segments along X (horizontal lines in Z direction)
                      for i in boundsI.Min.Z .. boundsI.Max.Z do
                        let z = bounds.Min.Z + single i * scale.Z
                        let y = bounds.Min.Y + single blockMap.Cursor.Y * scale.Y
                        let a = Vector3 (bounds.Min.X, y, z)
                        let b = Vector3 (bounds.Max.X, y, z)
                        Segment3 (a, b)|]

                | ZPos | ZNeg ->

                    [|// segments along Y (vertical lines in X direction)
                      for i in boundsI.Min.X .. boundsI.Max.X do
                        let x = bounds.Min.X + single i * scale.X
                        let z = bounds.Min.Z + single blockMap.Cursor.Z * scale.Z
                        let a = Vector3 (x, bounds.Min.Y, z)
                        let b = Vector3 (x, bounds.Max.Y, z)
                        Segment3 (a, b)

                      // segments along X (horizontal lines in Y direction)
                      for i in boundsI.Min.Y .. boundsI.Max.Y do
                        let y = bounds.Min.Y + single i * scale.Y
                        let z = bounds.Min.Z + single blockMap.Cursor.Z * scale.Z
                        let a = Vector3 (bounds.Min.X, y, z)
                        let b = Vector3 (bounds.Max.X, y, z)
                        Segment3 (a, b)|]

            // draw grid line segments
            let gridColor = Color (64uy, 64uy, 64uy, 255uy) // TODO: make constant.
            World.imGuiSegments3d segments 1.0f gridColor world

            // edit block editor
            if ImGui.Begin ("Block Editor", ImGuiWindowFlags.NoNav) then

                // edit palette selection
                ImGui.Text "Style"
                let palette = blockMap.Palette
                let styleIndex = blockMap.StyleIndex
                let styles = palette.Styles
                let style = styles[styleIndex]
                let mutable color = style.Color.V4
                if ImGui.ColorEdit4 ("Palette Selection", &color, ImGuiColorEditFlags.NoLabel ||| ImGuiColorEditFlags.NoInputs) then
                    let styles = Array.removeAt styleIndex styles
                    let styles = Array.insertAt styleIndex { style with Color = Color color } styles
                    let palette = { palette with Styles = styles }
                    blockMap <- BlockMap.setPalette palette blockMap
                ImGui.SameLine ()
                if styleIndex < 24 then
                    if ImGui.Button "Reset Color" then
                        () // TODO: reset to original color
                else
                    if ImGui.Button "Random Color" then
                        () // TODO: set to random color

                // select from palette
                ImGui.Text "Block Palette"
                let palette = blockMap.Palette
                let styles = palette.Styles
                for i in 0 .. dec styles.Length do
                    let style = styles[i]
                    if ImGui.ColorButton ("Style" + string i, style.Color.V4) then
                        blockMap <- BlockMap.setStyleIndex i blockMap
                    if  inc i % 12 <> 0 &&
                        inc i < styles.Length then
                        ImGui.SameLine ()
                    if  ImGui.IsItemHovered () &&
                        ImGui.IsMouseClicked ImGuiMouseButton.Right &&
                        i >= 24 then
                        let palette = Palette.removeStyle i blockMap.Palette
                        blockMap <- BlockMap.setPalette palette blockMap

                // augment palette
                if ImGui.Button "Add Style" then
                    let style = Style.make (Color (Random.Shared.NextSingle (), Random.Shared.NextSingle (), Random.Shared.NextSingle (), 1.0f)) "" Map.empty
                    let palette = Palette.addStyle style blockMap.Palette
                    blockMap <- BlockMap.setPalette palette blockMap
                    blockMap <- BlockMap.setStyleIndex (dec palette.Styles.Length) blockMap

                // edit plane
                let editPlaneName = scstringMemo blockMap.EditPlane
                if ImGui.BeginCombo ("Block Plane", editPlaneName) then
                    let editPlaneNames = Seq.cast<string> (Reflection.getUnionCases typeof<EditPlane>).Keys
                    for name in editPlaneNames do
                        if ImGui.Selectable (name, (name = editPlaneName)) then
                            blockMap <- BlockMap.setEditPlane (scvalueMemo name) blockMap
                    ImGui.EndCombo ()

                // edit visible layers
                let mutable layersVisible = blockMap.LayersVisible
                if ImGui.SliderInt ("Layers Visible", &layersVisible, 0, 64) then
                    blockMap <- BlockMap.setLayersVisible layersVisible blockMap

                // actions
                if not blockMap.Generated then
                    if ImGui.Button "Generate" then
                        viewportOverlay.EditContext.Snapshot GenerateFromBlockMap world
                        blockMap <- WorldBlockMap.generate blockMap entity world
                else
                    if ImGui.Button "Clear" then
                        viewportOverlay.EditContext.Snapshot ClearBlocks world
                        blockMap <- WorldBlockMap.clear blockMap entity world

            // finish block editor window
            ImGui.End ()

            // inspect modifiers for later use
            let allModsUp = World.isKeyboardCtrlUp world && World.isKeyboardAltUp world && World.isKeyboardShiftUp world
            let onlyCtrlDown = World.isKeyboardCtrlDown world && World.isKeyboardAltUp world && World.isKeyboardShiftUp world
            let onlyCtrlShiftDown = World.isKeyboardCtrlDown world && World.isKeyboardAltUp world && World.isKeyboardShiftDown world

            // handle paint height adjustment via Ctrl+Scroll
            if onlyCtrlDown && World.isMouseScrolledDown world then
                blockMap <- BlockMap.setPaintHeight (inc blockMap.PaintHeight) blockMap
            if onlyCtrlDown && World.isMouseScrolledUp world then
                blockMap <- BlockMap.setPaintHeight (dec blockMap.PaintHeight) blockMap

            // handle edit plane selection
            if allModsUp && World.isKeyboardKeyPressed KeyboardKey.X world then
                match blockMap.EditPlane with
                | XPos | YPos | ZPos -> blockMap <- BlockMap.setEditPlane XPos blockMap
                | XNeg | YNeg | ZNeg -> blockMap <- BlockMap.setEditPlane XNeg blockMap
            if allModsUp && World.isKeyboardKeyPressed KeyboardKey.Y world then
                match blockMap.EditPlane with
                | XPos | YPos | ZPos -> blockMap <- BlockMap.setEditPlane YPos blockMap
                | XNeg | YNeg | ZNeg -> blockMap <- BlockMap.setEditPlane YNeg blockMap
            if allModsUp && World.isKeyboardKeyPressed KeyboardKey.Z world then
                match blockMap.EditPlane with
                | XPos | YPos | ZPos -> blockMap <- BlockMap.setEditPlane ZPos blockMap
                | XNeg | YNeg | ZNeg -> blockMap <- BlockMap.setEditPlane ZNeg blockMap

            // handle edit plane dimension cycling
            if allModsUp && World.isKeyboardKeyPressed KeyboardKey.Tab world then
                match blockMap.EditPlane with
                | XPos -> blockMap <- BlockMap.setEditPlane YPos blockMap
                | YPos -> blockMap <- BlockMap.setEditPlane ZPos blockMap
                | ZPos -> blockMap <- BlockMap.setEditPlane XPos blockMap
                | XNeg -> blockMap <- BlockMap.setEditPlane YNeg blockMap
                | YNeg -> blockMap <- BlockMap.setEditPlane ZNeg blockMap
                | ZNeg -> blockMap <- BlockMap.setEditPlane XNeg blockMap

            // handle edit plane orientation cycling
            if allModsUp && World.isKeyboardKeyPressed KeyboardKey.Space world then
                match blockMap.EditPlane with
                | XPos -> blockMap <- BlockMap.setEditPlane XNeg blockMap
                | YPos -> blockMap <- BlockMap.setEditPlane YNeg blockMap
                | ZPos -> blockMap <- BlockMap.setEditPlane ZNeg blockMap
                | XNeg -> blockMap <- BlockMap.setEditPlane XPos blockMap
                | YNeg -> blockMap <- BlockMap.setEditPlane YPos blockMap
                | ZNeg -> blockMap <- BlockMap.setEditPlane ZPos blockMap

            // handle scrolling up
            if allModsUp && World.isMouseScrolledUp world then
                match blockMap.EditPlane with
                | XPos | XNeg -> blockMap <- BlockMap.mapCursor (fun cursor -> cursor.MapX (fun x -> max (dec x) 0)) blockMap
                | YPos | YNeg -> blockMap <- BlockMap.mapCursor (fun cursor -> cursor.MapY (fun y -> max (dec y) 0)) blockMap
                | ZPos | ZNeg -> blockMap <- BlockMap.mapCursor (fun cursor -> cursor.MapZ (fun z -> max (dec z) 0)) blockMap

            // handle scrolling down
            if allModsUp && World.isMouseScrolledDown world then
                match blockMap.EditPlane with
                | XPos | XNeg -> blockMap <- BlockMap.mapCursor (fun cursor -> cursor.MapX (fun x -> min (inc x) blockMap.Chunk.BoundsI.Size.X)) blockMap
                | YPos | YNeg -> blockMap <- BlockMap.mapCursor (fun cursor -> cursor.MapY (fun y -> min (inc y) blockMap.Chunk.BoundsI.Size.Y)) blockMap
                | ZPos | ZNeg -> blockMap <- BlockMap.mapCursor (fun cursor -> cursor.MapZ (fun z -> min (inc z) blockMap.Chunk.BoundsI.Size.Z)) blockMap

            // paint block when ungenerated
            if allModsUp && World.isMouseButtonDown MouseLeft world && not blockMap.Generated then
                if World.isMouseButtonPressed MouseLeft world then viewportOverlay.EditContext.Snapshot PaintBlocks world
                let position = entity.GetPosition world
                let ray = World.getMouseRay3dWorld world
                match BlockMap.tryPickPositionI ray position blockMap with
                | Some positionI -> blockMap <- BlockMap.paint positionI blockMap
                | None -> ()

            // actions
            if not blockMap.Generated then
                if onlyCtrlDown && World.isKeyboardKeyPressed KeyboardKey.G world then
                    viewportOverlay.EditContext.Snapshot GenerateFromBlockMap world
                    blockMap <- WorldBlockMap.generate blockMap entity world
            else
                if onlyCtrlShiftDown && World.isKeyboardKeyPressed KeyboardKey.G world then
                    viewportOverlay.EditContext.Snapshot ClearBlocks world
                    blockMap <- WorldBlockMap.clear blockMap entity world

            // fin
            entity.SetBlockMap blockMap world

        | ReplaceProperty replaceProperty ->

            // replace when BlockMap
            if  replaceProperty.PropertyDescriptor.PropertyName = nameof BlockMap &&
                replaceProperty.PropertyDescriptor.PropertyType = typeof<BlockMap> then
                replaceProperty.IndicateReplaced ()

                // use a mutable reference for tracking block editor's transformations
                let mutable blockMap = entity.GetBlockMap world

                // edit passes
                ImGui.Text "Passes"
                let passes = blockMap.Passes
                for (passName, pass) in passes.Pairs' do
                    ImGui.Text passName
                    for processor in pass.Processors do
                        ImGui.Indent ()
                        ImGui.Text processor.ProcessorName
                        let mutable processFnName = processor.ProcessFnName
                        if ImGui.InputText ("Process Fn Name##" + processor.ProcessorName, &processFnName, 4096u) then
                            let processor = { processor with ProcessFnName = processFnName }
                            let pass = Pass.replaceProcessor processor pass
                            blockMap <- BlockMap.addPass passName pass blockMap
                        if ImGui.IsItemFocused () then replaceProperty.EditContext.FocusProperty ()
                        ImGui.Unindent ()
                    ImGui.Indent ()
                    if ImGui.Button ("Add Processor##" + passName) then
                        let processor = Processor.make Gen.name Map.empty (nameof ProcessFns.Nop)
                        let pass = Pass.addProcessor processor pass
                        blockMap <- BlockMap.addPass passName pass blockMap
                    if ImGui.IsItemFocused () then replaceProperty.EditContext.FocusProperty ()
                    ImGui.Unindent ()
                if ImGui.Button "Add Pass" then blockMap <- BlockMap.addPass Gen.name Pass.initial blockMap
                if ImGui.IsItemFocused () then replaceProperty.EditContext.FocusProperty ()

                // fin
                entity.SetBlockMap blockMap world

        | _ -> ()

    override this.GetAttributesInferred (entity, world) =
        let blockMap = entity.GetBlockMap world
        AttributesInferred.important blockMap.Size v3Zero