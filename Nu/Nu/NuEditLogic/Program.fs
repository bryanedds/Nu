module NuEditLogic.Program
open NuEditDesign
open SDL2
open OpenTK
open System
open System.IO
open System.Collections.Generic
open System.Reflection
open System.Windows.Forms
open System.ComponentModel
open System.Xml
open System.Xml.Serialization
open Microsoft.FSharp.Reflection
open Nu.Core
open Nu.Math
open Nu.AssetMetadata
open Nu.Sdl
open Nu.Entity
open Nu.Group
open Nu.Screen
open Nu.Game
open Nu.Simulation
open NuEditLogic.Entity

let DefaultPositionSnap = 8
let DefaultRotationSnap = 5
let DefaultCreationDepth = 0.0f
let DefaultSize = 64.0f
let DefaultRotation = 0.0f
let (ScreenWidth, ScreenHeight) = (900, 600)

type WorldChanger = World -> World
type WorldChangers = WorldChanger List

type DragState =
    | DragNone
    | DragPosition of Vector2 * Vector2 * Address
    | DragRotation of Vector2 * Vector2 * Address

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
        let entityModelLens = worldEntityModel entityModelTds.Address
        let entityModel = get !entityModelTds.RefWorld entityModelLens
        getEntityModelPropertyValue property entityModel

    override this.SetValue (source, value) =
        let entityModelTds = source :?> EntityModelTypeDescriptorSource
        let changer = (fun world_ ->
            if property.Name = "Name" then
                let valueStr = str value
                if Int64.TryParse (valueStr, ref 0L) then
                    trace <| "Invalid entity model name '" + valueStr + "' (must not be a number)."
                    world_
                else
                    let entityModelAddress_ = entityModelTds.Address
                    let entityModel_ = get world_ <| worldEntityModel entityModelAddress_
                    let world_ = removeEntityModel entityModelTds.Address world_
                    let entity_ = get entityModel_ entityModelEntity
                    let entity_ = { entity_ with Name = valueStr }
                    let entityModel_ = set entity_ entityModel_ entityModelEntity
                    let entityModelAddress_ = Test.GroupModelAddress @ [Lun.make <| valueStr]
                    let world_ = addEntityModel entityModelAddress_ entityModel_ world_
                    entityModelTds.RefWorld := world_ // must be set for property grid
                    entityModelTds.Form.propertyGrid.SelectedObject <- { entityModelTds with Address = entityModelAddress_ }
                    world_
            else
                let entityModelAddress = entityModelTds.Address
                let world_ = setEntityModelPropertyValue entityModelAddress property value world_
                let entityModel = get world_ <| worldEntityModel entityModelAddress
                propagateEntityModelProperties entityModel world_)
        // NOTE: even though this ref world will eventually get blown away, it must still be
        // updated here so that the change is reflected immediately by the property grid.
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
                let entityModelLens = worldEntityModel source.Address
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
        if form.InteractButton.Checked then (message, world_)
        else
            let groupModelAddress = Test.GroupModelAddress
            let groupModel = get world_ (worldGroupModel groupModelAddress)
            let entityModels = Map.toValueList <| (get groupModel groupModelGroup).EntityModels
            let optPicked = tryPick position entityModels world_
            match optPicked with
            | None -> (handle message, world_)
            | Some entityModel ->
                let entity = get entityModel entityModelEntity
                let entityModelAddress = groupModelAddress @ [Lun.make entity.Name]
                refWorld := world_ // must be set for property grid
                form.propertyGrid.SelectedObject <- { Address = entityModelAddress; Form = form; WorldChangers = worldChangers; RefWorld = refWorld }
                let entityModelTransform = getEntityModelTransform true world_.Camera entityModel
                let dragState = DragPosition (entityModelTransform.Position + world_.MouseState.MousePosition, world_.MouseState.MousePosition, entityModelAddress)
                let world_ = { world_ with ExtData = dragState }
                (handle message, world_)
    | _ -> failwith <| "Expected MouseButtonData in message '" + str message + "'."

let endDrag (form : NuEditForm) _ _ message world =
    match message.Data with
    | MouseButtonData (position, _) ->
        if form.InteractButton.Checked then (message, world)
        else
            match world.ExtData :?> DragState with
            | DragNone -> (handle message, world)
            | DragPosition _
            | DragRotation _ ->
                form.propertyGrid.Refresh ()
                (handle message, { world with ExtData = DragNone })
    | _ -> failwith <| "Expected MouseButtonData in message '" + str message + "'."

let updateDrag (form : NuEditForm) world_ =
    let dragState = world_.ExtData :?> DragState
    match dragState with
    | DragNone -> world_
    | DragPosition (pickOffset, origMousePosition, address) ->
        let entityModel_ = get world_ <| worldEntityModel address
        let transform_ = getEntityModelTransform true world_.Camera entityModel_
        let transform_ = { transform_ with Position = (pickOffset - origMousePosition) + (world_.MouseState.MousePosition - origMousePosition) }
        let (positionSnap, rotationSnap) = getSnaps form
        let entityModel_ = setEntityModelTransform true world_.Camera positionSnap rotationSnap transform_ entityModel_
        let world_ = set entityModel_ world_ <| worldEntityModel address
        let world_ = { world_ with ExtData = DragPosition (pickOffset, origMousePosition, address) }
        let world_ = propagateEntityModelTransform entityModel_ world_
        form.propertyGrid.Refresh ()
        world_
    | DragRotation (pickOffset, origPosition, address) -> world_

let createNuEditForm worldChangers refWorld =
    
    let form = new NuEditForm ()
    form.displayPanel.MaximumSize <- Drawing.Size (ScreenWidth, ScreenHeight)
    form.positionSnapTextBox.Text <- str DefaultPositionSnap
    form.rotationSnapTextBox.Text <- str DefaultRotationSnap
    form.creationDepthTextBox.Text <- str DefaultCreationDepth

    for unionCase in FSharpType.GetUnionCases (typeof<EntityModel>) do
        ignore <| form.createEntityComboBox.Items.Add unionCase.Name
    form.createEntityComboBox.SelectedIndex <- 0

    form.exitToolStripMenuItem.Click.Add (fun _ ->
        form.Close ())

    form.createEntityButton.Click.Add (fun _ ->
        let changer = (fun world ->
            let entityModelTransform = { Transform.Position = world.Camera.EyeSize * 0.5f; Depth = getCreationDepth form; Size = Vector2 DefaultSize; Rotation = DefaultRotation }
            let entityModelTypeName = typeof<EntityModel>.FullName + "+" + form.createEntityComboBox.Text
            let entityModel_ = makeDefaultEntityModel entityModelTypeName
            let entityModel_ = setEntityModelTransform true world.Camera 0 0 entityModelTransform entityModel_
            let entity = get entityModel_ entityModelEntity
            let entityModelAddress = Test.GroupModelAddress @ [Lun.make entity.Name]
            refWorld := addEntityModel entityModelAddress entityModel_ world // must be set for property grid
            form.propertyGrid.SelectedObject <- { Address = entityModelAddress; Form = form; WorldChangers = worldChangers; RefWorld = refWorld }
            !refWorld)
        refWorld := changer !refWorld
        worldChangers.Add changer)

    form.deleteEntityButton.Click.Add (fun _ ->
        let selectedObject = form.propertyGrid.SelectedObject
        form.propertyGrid.SelectedObject <- null
        let changer = (fun world ->
            match selectedObject with
            | :? EntityModelTypeDescriptorSource as entityModelTds -> removeEntityModel entityModelTds.Address world
            | _ -> world)
        refWorld := changer !refWorld
        worldChangers.Add changer)

    form.saveToolStripMenuItem.Click.Add (fun _ ->
        let saveFileResult = form.saveFileDialog.ShowDialog form
        match saveFileResult with
        | DialogResult.OK -> writeFile form.saveFileDialog.FileName !refWorld
        | _ -> ())

    form.openToolStripMenuItem.Click.Add (fun _ ->
        let openFileResult = form.openFileDialog.ShowDialog form
        match openFileResult with
        | DialogResult.OK ->
            let changer = readFile form.openFileDialog.FileName
            refWorld := changer !refWorld
            worldChangers.Add changer
        | _ -> ())

    form

let tryCreateEditorWorld form worldChangers refWorld sdlDeps =
    let screen = { Id = getNuId (); GroupModels = Map.empty }
    let group = { Id = getNuId (); EntityModels = Map.empty }
    refWorld := createEmptyWorld sdlDeps DragNone
    refWorld := addScreen Test.ScreenModelAddress screen !refWorld
    refWorld := set (Some Test.ScreenModelAddress) !refWorld worldOptSelectedScreenModelAddress
    refWorld := addGroup Test.GroupModelAddress group !refWorld
    refWorld := subscribe DownMouseLeftAddress [] (beginDrag form worldChangers refWorld) !refWorld
    refWorld := subscribe UpMouseLeftAddress [] (endDrag form) !refWorld
    Right !refWorld

let updateEditorWorld form (worldChangers : WorldChangers) refWorld world =
    refWorld := updateDrag form world
    refWorld := Seq.fold (fun world changer -> changer world) !refWorld worldChangers
    worldChangers.Clear ()
    (not form.IsDisposed, !refWorld)

let [<EntryPoint; STAThread>] main _ =
    initTypeConverters ()
    let worldChangers = WorldChangers ()
    let refWorld = ref Unchecked.defaultof<World> // uglaaayyyyyy!
    use form = createNuEditForm worldChangers refWorld
    form.Show ()
    let sdlViewConfig = ExistingWindow form.displayPanel.Handle
    let sdlRenderFlags = enum<SDL.SDL_RendererFlags> (int SDL.SDL_RendererFlags.SDL_RENDERER_ACCELERATED)
    let sdlConfig = makeSdlConfig sdlViewConfig form.displayPanel.MaximumSize.Width form.displayPanel.MaximumSize.Height sdlRenderFlags 1024
    run4
        (tryCreateEditorWorld form worldChangers refWorld)
        (updateEditorWorld form worldChangers refWorld)
        (fun world -> form.displayPanel.Invalidate (); world)
        sdlConfig