namespace NuEditLogic
open NuEditDesign
open SDL2
open OpenTK
open TiledSharp
open System
open System.IO
open System.Collections.Generic
open System.Reflection
open System.Windows.Forms
open System.ComponentModel
open System.Xml
open System.Xml.Serialization
open Microsoft.FSharp.Reflection
open Nu
open Nu.Core
open Nu.Math
open Nu.Metadata
open Nu.Physics
open Nu.Sdl
open Nu.Entities
open Nu.Groups
open Nu.Screens
open Nu.Games
open Nu.Sim
open NuEditLogic.Entity

type WorldChanger = World -> World

type WorldChangers = WorldChanger List

type DragState =
    | DragNone
    | DragPosition of Vector2 * Vector2 * Address
    | DragRotation of Vector2 * Vector2 * Address

type EditorState =
    { DragState : DragState
      PastWorlds : World list
      FutureWorlds : World list
      Clipboard : (EntityModel option) ref }

module Program =

    let DefaultPositionSnap = 8
    let DefaultRotationSnap = 5
    let DefaultCreationDepth = 0.0f
    let DefaultSize = 64.0f
    let DefaultRotation = 0.0f
    let (ScreenWidth, ScreenHeight) = (900, 600)

    let pushPastWorld pastWorld world =
        let editorState_ = world.ExtData :?> EditorState
        let editorState_ = { editorState_ with PastWorlds = pastWorld :: editorState_.PastWorlds; FutureWorlds = [] }
        { world with ExtData = editorState_ }

    let clearPastWorlds world =
        let editorState_ = world.ExtData :?> EditorState
        let editorState_ = { editorState_ with PastWorlds = [] }
        { world with ExtData = editorState_ }

    type [<TypeDescriptionProvider (typeof<EntityModelTypeDescriptorProvider>)>] EntityModelTypeDescriptorSource =
        { Address : Address
          Form : NuEditForm
          WorldChangers : WorldChangers
          RefWorld : World ref }

    and EntityModelPropertyDescriptor (property : PropertyInfo) =
        inherit PropertyDescriptor (property.Name, Array.empty)
        override this.ComponentType with get () = property.DeclaringType
        override this.PropertyType with get () = property.PropertyType
        override this.CanResetValue source = false
        override this.ResetValue source = ()
        override this.ShouldSerializeValue source = true

        override this.IsReadOnly
            // NOTE: we make entity ids read-only
            with get () = property.Name.Contains "Id"

        override this.GetValue source =
            let entityModelTds = source :?> EntityModelTypeDescriptorSource
            let entityModelLens = worldEntityModelLens entityModelTds.Address
            let entityModel = get !entityModelTds.RefWorld entityModelLens
            getEntityModelPropertyValue property entityModel

        override this.SetValue (source, value) =
            let entityModelTds = source :?> EntityModelTypeDescriptorSource
            let changer = (fun world_ ->
                let pastWorld = world_
                let world_ =
                    // handle special case for an entity's Name field change
                    if property.Name = "Name" then
                        let valueStr = str value
                        if Int64.TryParse (valueStr, ref 0L) then
                            trace <| "Invalid entity model name '" + valueStr + "' (must not be a number)."
                            world_
                        else
                            // TODO: factor out a renameEntityModel function
                            let entityModel_ = get world_ <| worldEntityModelLens entityModelTds.Address
                            let world_ = removeEntityModel entityModelTds.Address world_
                            let entity_ = get entityModel_ entityLens
                            let entity_ = { entity_ with Name = valueStr }
                            let entityModel_ = set entity_ entityModel_ entityLens
                            let entityModelAddress = Test.GroupModelAddress @ [Lun.make <| valueStr]
                            let world_ = addEntityModel entityModelAddress entityModel_ world_
                            entityModelTds.RefWorld := world_ // must be set for property grid
                            entityModelTds.Form.propertyGrid.SelectedObject <- { entityModelTds with Address = entityModelAddress }
                            world_
                    else
                        let world_ = setEntityModelPropertyValue entityModelTds.Address property value world_
                        let entityModel_ = get world_ <| worldEntityModelLens entityModelTds.Address
                        let entityModel_ =
                            // handle special case for TileMap's TileMapAsset field change
                            if property.Name = "TileMapAsset" then
                                match entityModel_ with
                                | TileMap tileMap_ ->
                                    let tileMapAsset = tileMap_.TileMapAsset
                                    let optTileMapMetadata = tryGetTileMapMetadata tileMapAsset.TileMapAssetName tileMapAsset.PackageName world_.AssetMetadataMap
                                    match optTileMapMetadata with
                                    | None -> entityModel_
                                    | Some (tileMapFileName, tileMapSprites) ->
                                        let tileMap_ = { tileMap_ with TmxMap = new TmxMap (tileMapFileName) }
                                        let tileMap_ = { tileMap_ with TileMapSprites = tileMapSprites }
                                        TileMap tileMap_
                                | _ -> entityModel_
                            else entityModel_
                        let world_ = set entityModel_ world_ <| worldEntityModelLens entityModelTds.Address
                        propagateEntityModelPhysics entityModelTds.Address entityModel_ world_
                pushPastWorld pastWorld world_)
            entityModelTds.RefWorld := changer !entityModelTds.RefWorld
            entityModelTds.WorldChangers.Add changer

        // NOTE: This has to be a static member in order to see the relevant types in the recursive definitions.
        static member GetPropertyDescriptors (aType : Type) =
            let properties = aType.GetProperties (BindingFlags.Instance ||| BindingFlags.Public)
            let propertyDescriptors = Seq.map (fun property -> new EntityModelPropertyDescriptor (property) :> PropertyDescriptor) properties
            List.ofSeq propertyDescriptors

    and EntityModelTypeDescriptor (optSource : obj) =
        inherit CustomTypeDescriptor ()
        override this.GetProperties _ =
            let propertyDescriptors =
                match optSource with
                | :? EntityModelTypeDescriptorSource as source ->
                    let entityModelLens = worldEntityModelLens source.Address
                    let entityModel = get !source.RefWorld entityModelLens
                    let entityModelTypes = getEntityModelTypes entityModel
                    // NOTE: this line could be simplified by a List.concatBy function.
                    List.fold (fun propertyDescriptors aType -> EntityModelPropertyDescriptor.GetPropertyDescriptors aType @ propertyDescriptors) [] entityModelTypes
                | _ -> EntityModelPropertyDescriptor.GetPropertyDescriptors typeof<EntityModel>
            PropertyDescriptorCollection (Array.ofList propertyDescriptors)

    and EntityModelTypeDescriptorProvider () =
        inherit TypeDescriptionProvider ()
        override this.GetTypeDescriptor (_, optSource) =
            EntityModelTypeDescriptor optSource :> ICustomTypeDescriptor

    let getSnaps (form : NuEditForm) =
        let positionSnap = ref 0
        ignore <| Int32.TryParse (form.positionSnapTextBox.Text, positionSnap)
        let rotationSnap = ref 0
        ignore <| Int32.TryParse (form.rotationSnapTextBox.Text, rotationSnap)
        (!positionSnap, !rotationSnap)
    
    let getCreationDepth (form : NuEditForm) =
        let creationDepth = ref 0.0f
        ignore <| Single.TryParse (form.creationDepthTextBox.Text, creationDepth)
        !creationDepth

    let beginDrag (form : NuEditForm) worldChangers refWorld _ _ message world_ =
        match message.Data with
        | MouseButtonData (position, _) ->
            if form.interactButton.Checked then (message, world_)
            else
                let groupModelAddress = Test.GroupModelAddress
                let groupModel = get world_ (worldGroupModelLens groupModelAddress)
                let entityModels = Map.toValueList <| (get groupModel groupLens).EntityModels
                let optPicked = tryPick position entityModels world_
                match optPicked with
                | None -> (handle message, world_)
                | Some entityModel ->
                    let pastWorld = world_
                    let entity = get entityModel entityLens
                    let entityModelAddress = groupModelAddress @ [Lun.make entity.Name]
                    let entityModelTransform = getEntityModelTransform (Some world_.Camera) entityModel
                    let dragState = DragPosition (entityModelTransform.Position + world_.MouseState.MousePosition, world_.MouseState.MousePosition, entityModelAddress)
                    let editorState_ = world_.ExtData :?> EditorState
                    let editorState_ = { editorState_ with DragState = dragState }
                    let world_ = { world_ with ExtData = editorState_ }
                    let world_ = pushPastWorld pastWorld world_
                    refWorld := world_ // must be set for property grid
                    form.propertyGrid.SelectedObject <- { Address = entityModelAddress; Form = form; WorldChangers = worldChangers; RefWorld = refWorld }
                    (handle message, world_)
        | _ -> failwith <| "Expected MouseButtonData in message '" + str message + "'."

    let endDrag (form : NuEditForm) _ _ message world =
        match message.Data with
        | MouseButtonData (position, _) ->
            if form.interactButton.Checked then (message, world)
            else
                let editorState_ = world.ExtData :?> EditorState
                match editorState_.DragState with
                | DragNone -> (handle message, world)
                | DragPosition _
                | DragRotation _ ->
                    let editorState_ = { editorState_ with DragState = DragNone }
                    form.propertyGrid.Refresh ()
                    (handle message, { world with ExtData = editorState_ })
        | _ -> failwith <| "Expected MouseButtonData in message '" + str message + "'."

    let updateDrag (form : NuEditForm) world_ =
        let editorState_ = world_.ExtData :?> EditorState
        match editorState_.DragState with
        | DragNone -> world_
        | DragPosition (pickOffset, origMousePosition, address) ->
            let entityModel_ = get world_ <| worldEntityModelLens address
            let transform_ = getEntityModelTransform (Some world_.Camera) entityModel_
            let transform_ = { transform_ with Position = (pickOffset - origMousePosition) + (world_.MouseState.MousePosition - origMousePosition) }
            let (positionSnap, rotationSnap) = getSnaps form
            let entityModel_ = setEntityModelTransform (Some world_.Camera) positionSnap rotationSnap transform_ entityModel_
            let world_ = set entityModel_ world_ <| worldEntityModelLens address
            let editorState_ = { editorState_ with DragState = DragPosition (pickOffset, origMousePosition, address) }
            let world_ = { world_ with ExtData = editorState_ }
            let world_ = propagateEntityModelPhysics address entityModel_ world_
            form.propertyGrid.Refresh ()
            world_
        | DragRotation (pickOffset, origPosition, address) -> world_

    /// Needed for physics system side-effects...
    let physicsHack world_ =
        let world_ = { world_ with PhysicsMessages = ResetHackMessage :: world_.PhysicsMessages }
        reregisterPhysicsHack Test.GroupModelAddress world_

    let handleExit (form : NuEditForm) _ =
        form.Close ()

    let handleCreate (form : NuEditForm) (worldChangers : WorldChanger List) refWorld atMouse _ =
        let changer = (fun world_ ->
            let pastWorld = world_
            let entityModelPosition = if atMouse then world_.MouseState.MousePosition else world_.Camera.EyeSize * 0.5f
            let entityModelTransform = { Transform.Position = entityModelPosition; Depth = getCreationDepth form; Size = Vector2 DefaultSize; Rotation = DefaultRotation }
            let entityModelTypeName = typeof<EntityModel>.FullName + "+" + form.createEntityComboBox.Text
            let entityModel_ = makeDefaultEntityModel entityModelTypeName
            let (positionSnap, rotationSnap) = getSnaps form
            let entityModel_ = setEntityModelTransform (Some world_.Camera) positionSnap rotationSnap entityModelTransform entityModel_
            let entity = get entityModel_ entityLens
            let entityModelAddress = Test.GroupModelAddress @ [Lun.make entity.Name]
            let world_ = addEntityModel entityModelAddress entityModel_ world_
            let world_ = pushPastWorld pastWorld world_
            refWorld := world_ // must be set for property grid
            form.propertyGrid.SelectedObject <- { Address = entityModelAddress; Form = form; WorldChangers = worldChangers; RefWorld = refWorld }
            world_)
        refWorld := changer !refWorld
        worldChangers.Add changer

    let handleDelete (form : NuEditForm) (worldChangers : WorldChanger List) refWorld _ =
        let selectedObject = form.propertyGrid.SelectedObject
        let changer = (fun world_ ->
            match selectedObject with
            | :? EntityModelTypeDescriptorSource as entityModelTds ->
                let pastWorld = world_
                let world_ = removeEntityModel entityModelTds.Address world_
                let world_ = pushPastWorld pastWorld world_
                form.propertyGrid.SelectedObject <- null
                world_
            | _ -> world_)
        refWorld := changer !refWorld
        worldChangers.Add changer

    let handleSave (form : NuEditForm) (worldChangers : WorldChanger List) refWorld _ =
        let saveFileResult = form.saveFileDialog.ShowDialog form
        match saveFileResult with
        | DialogResult.OK -> writeFile form.saveFileDialog.FileName !refWorld
        | _ -> ()

    let handleOpen (form : NuEditForm) (worldChangers : WorldChanger List) refWorld _ =
        let openFileResult = form.openFileDialog.ShowDialog form
        match openFileResult with
        | DialogResult.OK ->
            let changer = (fun world_ ->
                let world_ = readFile form.openFileDialog.FileName world_
                let world_ = clearPastWorlds world_
                form.interactButton.Checked <- false
                world_)
            refWorld := changer !refWorld
            worldChangers.Add changer
        | _ -> ()

    let handleUndo (form : NuEditForm) (worldChangers : WorldChanger List) refWorld _ =
        let changer = (fun world_ ->
            let futureWorld = world_
            let editorState_ = world_.ExtData :?> EditorState
            match editorState_.PastWorlds with
            | [] -> world_
            | pastWorld :: pastWorlds ->
                let world_ = pastWorld
                let world_ = physicsHack world_
                let editorState_ = { editorState_ with PastWorlds = pastWorlds; FutureWorlds = futureWorld :: editorState_.FutureWorlds }
                let world_ = { world_ with ExtData = editorState_ }
                if form.interactButton.Checked then form.interactButton.Checked <- false
                form.propertyGrid.SelectedObject <- null
                world_)
        refWorld := changer !refWorld
        worldChangers.Add changer

    let handleRedo (form : NuEditForm) (worldChangers : WorldChanger List) refWorld _ =
        let changer = (fun world_ ->
            let pastWorld = world_
            let editorState_ = world_.ExtData :?> EditorState
            match editorState_.FutureWorlds with
            | [] -> world_
            | futureWorld :: futureWorlds ->
                let world_ = futureWorld
                let world_ = physicsHack world_
                let editorState_ = { editorState_ with PastWorlds = pastWorld :: editorState_.PastWorlds; FutureWorlds = futureWorlds }
                let world_ = { world_ with ExtData = editorState_ }
                if form.interactButton.Checked then form.interactButton.Checked <- false
                form.propertyGrid.SelectedObject <- null
                world_)
        refWorld := changer !refWorld
        worldChangers.Add changer

    let handleInteractChanged (form : NuEditForm) (worldChangers : WorldChanger List) refWorld _ =
        if form.interactButton.Checked then
            let changer = (fun world_ -> pushPastWorld world_ world_)
            refWorld := changer !refWorld
            worldChangers.Add changer

    let handleCut (form : NuEditForm) (worldChangers : WorldChanger List) refWorld _ =
        let optEntityModelTds = form.propertyGrid.SelectedObject
        match optEntityModelTds with
        | null -> ()
        | :? EntityModelTypeDescriptorSource as entityModelTds ->
            let changer = (fun world_ ->
                let pastWorld = world_
                let editorState = world_.ExtData :?> EditorState
                let entityModel = get world_ <| worldEntityModelLens entityModelTds.Address
                let world_ = removeEntityModel entityModelTds.Address world_
                editorState.Clipboard := Some entityModel
                form.propertyGrid.SelectedObject <- null
                world_)
            refWorld := changer !refWorld
            worldChangers.Add changer
        | _ -> trace <| "Invalid cut operation (likely a code issue in NuEditLogic)."
        
    let handleCopy (form : NuEditForm) (worldChangers : WorldChanger List) refWorld _ =
        let optEntityModelTds = form.propertyGrid.SelectedObject
        match optEntityModelTds with
        | null -> ()
        | :? EntityModelTypeDescriptorSource as entityModelTds ->
            let entityModel = get !refWorld <| worldEntityModelLens entityModelTds.Address
            let editorState = (!refWorld).ExtData :?> EditorState
            editorState.Clipboard := Some entityModel
        | _ -> trace <| "Invalid copy operation (likely a code issue in NuEditLogic)."

    let handlePaste (form : NuEditForm) (worldChangers : WorldChanger List) refWorld atMouse _ =
        let editorState = (!refWorld).ExtData :?> EditorState
        match !editorState.Clipboard with
        | None -> ()
        | Some entityModel_ ->
            let changer = (fun world_ ->
                let entity_ = get entityModel_ entityLens
                let id = getNuId ()
                let entity_ = { entity_ with Id = id; Name = str id }
                let entityModel_ = set entity_ entityModel_ entityLens
                let entityModelPosition = if atMouse then world_.MouseState.MousePosition else world_.Camera.EyeSize * 0.5f
                let entityModelTransform_ = getEntityModelTransform (Some world_.Camera) entityModel_
                let entityModelTransform_ = { entityModelTransform_ with Position = entityModelPosition; Depth = getCreationDepth form }
                let (positionSnap, rotationSnap) = getSnaps form
                let entityModel_ = setEntityModelTransform (Some world_.Camera) positionSnap rotationSnap entityModelTransform_ entityModel_
                let address = Test.GroupModelAddress @ [Lun.make entity_.Name]
                let pastWorld = world_
                let world_ = pushPastWorld pastWorld world_
                addEntityModel address entityModel_ world_)
            refWorld := changer !refWorld
            worldChangers.Add changer

    let handleQuickSize (form : NuEditForm) (worldChangers : WorldChanger List) refWorld _ =
        let optEntityModelTds = form.propertyGrid.SelectedObject
        match optEntityModelTds with
        | null -> ()
        | :? EntityModelTypeDescriptorSource as entityModelTds ->
            let changer = (fun world_ ->
                let entityModel_ = get world_ <| worldEntityModelLens entityModelTds.Address
                let entityModelQuickSize = getEntityModelQuickSize world_.AssetMetadataMap entityModel_
                let entityModelTransform = { getEntityModelTransform None entityModel_ with Size = entityModelQuickSize }
                let entityModel_ = setEntityModelTransform None 0 0 entityModelTransform entityModel_
                let world_ = set entityModel_ world_ <| worldEntityModelLens entityModelTds.Address
                refWorld := world_ // must be set for property grid
                form.propertyGrid.Refresh ()
                world_)
            refWorld := changer !refWorld
            worldChangers.Add changer
        | _ -> trace <| "Invalid quick size operation (likely a code issue in NuEditLogic)."

    let createNuEditForm worldChangers refWorld =
        let form = new NuEditForm ()
        form.displayPanel.MaximumSize <- Drawing.Size (ScreenWidth, ScreenHeight)
        form.positionSnapTextBox.Text <- str DefaultPositionSnap
        form.rotationSnapTextBox.Text <- str DefaultRotationSnap
        form.creationDepthTextBox.Text <- str DefaultCreationDepth
        for unionCase in FSharpType.GetUnionCases typeof<EntityModel> do
            ignore <| form.createEntityComboBox.Items.Add unionCase.Name
        form.createEntityComboBox.SelectedIndex <- 0
        form.exitToolStripMenuItem.Click.Add (handleExit form)
        form.createEntityButton.Click.Add (handleCreate form worldChangers refWorld false)
        form.createToolStripMenuItem.Click.Add (handleCreate form worldChangers refWorld false)
        form.createContextMenuItem.Click.Add (handleCreate form worldChangers refWorld true)
        form.deleteEntityButton.Click.Add (handleDelete form worldChangers refWorld)
        form.deleteToolStripMenuItem.Click.Add (handleDelete form worldChangers refWorld)
        form.deleteContextMenuItem.Click.Add (handleDelete form worldChangers refWorld)
        form.saveToolStripMenuItem.Click.Add (handleSave form worldChangers refWorld)
        form.openToolStripMenuItem.Click.Add (handleOpen form worldChangers refWorld)
        form.undoButton.Click.Add (handleUndo form worldChangers refWorld)
        form.undoToolStripMenuItem.Click.Add (handleUndo form worldChangers refWorld)
        form.redoButton.Click.Add (handleRedo form worldChangers refWorld)
        form.redoToolStripMenuItem.Click.Add (handleRedo form worldChangers refWorld)
        form.interactButton.CheckedChanged.Add (handleInteractChanged form worldChangers refWorld)
        form.cutToolStripMenuItem.Click.Add (handleCut form worldChangers refWorld)
        form.cutContextMenuItem.Click.Add (handleCut form worldChangers refWorld)
        form.copyToolStripMenuItem.Click.Add (handleCopy form worldChangers refWorld)
        form.copyContextMenuItem.Click.Add (handleCopy form worldChangers refWorld)
        form.pasteToolStripMenuItem.Click.Add (handlePaste form worldChangers refWorld false)
        form.pasteContextMenuItem.Click.Add (handlePaste form worldChangers refWorld true)
        form.quickSizeToolStripButton.Click.Add (handleQuickSize form worldChangers refWorld)
        form.Show ()
        form

    let tryCreateEditorWorld form worldChangers refWorld sdlDeps =
        let screen = { Id = getNuId (); GroupModels = Map.empty }
        let group = { Id = getNuId (); EntityModels = Map.empty }
        let editorState = { DragState = DragNone; PastWorlds = []; FutureWorlds = []; Clipboard = ref None }
        let optWorld = tryCreateEmptyWorld sdlDeps editorState
        match optWorld with
        | Left errorMsg -> Left errorMsg
        | Right world ->
            refWorld := world
            refWorld := addScreen Test.ScreenModelAddress screen !refWorld
            refWorld := set (Some Test.ScreenModelAddress) !refWorld worldOptSelectedScreenModelAddressLens
            refWorld := addGroup Test.GroupModelAddress group !refWorld
            refWorld := subscribe DownMouseLeftAddress [] (beginDrag form worldChangers refWorld) !refWorld
            refWorld := subscribe UpMouseLeftAddress [] (endDrag form) !refWorld
            Right !refWorld

    // TODO: remove code duplication with below
    let updateUndo (form : NuEditForm) world =
        let editorState = world.ExtData :?> EditorState
        if form.undoToolStripMenuItem.Enabled then
            if List.isEmpty editorState.PastWorlds then
                form.undoButton.Enabled <- false
                form.undoToolStripMenuItem.Enabled <- false
        elif not <| List.isEmpty editorState.PastWorlds then
            form.undoButton.Enabled <- true
            form.undoToolStripMenuItem.Enabled <- true

    let updateRedo (form : NuEditForm) world =
        let editorState = world.ExtData :?> EditorState
        if form.redoToolStripMenuItem.Enabled then
            if List.isEmpty editorState.FutureWorlds then
                form.redoButton.Enabled <- false
                form.redoToolStripMenuItem.Enabled <- false
        elif not <| List.isEmpty editorState.FutureWorlds then
            form.redoButton.Enabled <- true
            form.redoToolStripMenuItem.Enabled <- true

    let updateEditorWorld form (worldChangers : WorldChangers) refWorld world_ =
        refWorld := updateDrag form world_
        refWorld := Seq.fold (fun world_ changer -> changer world_) !refWorld worldChangers
        worldChangers.Clear ()
        let editorState = (!refWorld).ExtData :?> EditorState
        updateUndo form !refWorld
        updateRedo form !refWorld
        (not form.IsDisposed, !refWorld)

    let [<EntryPoint; STAThread>] main _ =
        initTypeConverters ()
        let worldChangers = WorldChangers ()
        let refWorld = ref Unchecked.defaultof<World>
        use form = createNuEditForm worldChangers refWorld
        let sdlViewConfig = ExistingWindow form.displayPanel.Handle
        let sdlRenderFlags = enum<SDL.SDL_RendererFlags> (int SDL.SDL_RendererFlags.SDL_RENDERER_ACCELERATED ||| int SDL.SDL_RendererFlags.SDL_RENDERER_PRESENTVSYNC)
        let sdlConfig = makeSdlConfig sdlViewConfig form.displayPanel.MaximumSize.Width form.displayPanel.MaximumSize.Height sdlRenderFlags 1024
        run4
            (tryCreateEditorWorld form worldChangers refWorld)
            (updateEditorWorld form worldChangers refWorld)
            (fun world -> form.displayPanel.Invalidate (); world)
            sdlConfig