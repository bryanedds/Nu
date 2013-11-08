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

// NOTE: I believe .NET's lack of parameterization in one aspect is forcing me to use this global-
// style variable (but perhaps I've merely overlooked how to parameterize this?)
let private gWorldChangers = List<World -> World> ()

type DragState =
    | DragNone
    | DragPosition of Vector2 * Vector2 * Address
    | DragRotation of Vector2 * Vector2 * Address

type [<TypeDescriptionProvider (typeof<EntityModelTypeDescriptorProvider>)>] EntityModelTypeDescriptorSource =
    { Address : Address
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
        let changer = (fun world -> setEntityModelPropertyValue entityModelTds.Address property value world)
        // NOTE: even though this ref world will eventually get blown away, it must still be
        // updated here so that the change is reflected immediately by the property grid.
        entityModelTds.RefWorld := changer !entityModelTds.RefWorld
        gWorldChangers.Add changer

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

let getSnaps (form : NuEditForm)=
    let positionSnap = ref 0
    let positionSnap_ = if Int32.TryParse (form.positionSnapTextBox.Text, positionSnap) then !positionSnap else 0
    let rotationSnap = ref 0
    let rotationSnap_ = if Int32.TryParse (form.rotationSnapTextBox.Text, rotationSnap) then !rotationSnap else 0
    (positionSnap_, rotationSnap_)

let beginDrag (form : NuEditForm) refWorld _ _ message world =
    refWorld := world
    match message.Data with
    | MouseButtonData (position, _) ->
        if form.InteractButton.Checked then (message, !refWorld)
        else
            let groupModelAddress = Test.GroupModelAddress
            let groupModel = get !refWorld (worldGroupModel groupModelAddress)
            let entityModels = Map.toValueList <| (get groupModel groupModelGroup).EntityModels
            let optPicked = tryPick position entityModels !refWorld
            match optPicked with
            | None -> (handle message, !refWorld)
            | Some entityModel ->
                let entity = get entityModel entityModelEntity
                let entityModelAddress = groupModelAddress @ [Lun.make entity.Name]
                form.propertyGrid.SelectedObject <- { Address = entityModelAddress; RefWorld = refWorld }
                let entityModelTransform = getEntityModelTransform true (!refWorld).Camera entityModel
                let newDragState = DragPosition (entityModelTransform.Position + (!refWorld).MouseState.MousePosition, (!refWorld).MouseState.MousePosition, entityModelAddress)
                refWorld := { !refWorld with ExtData = newDragState }
                (handle message, !refWorld)
    | _ -> failwith <| "Expected MouseButtonData in message '" + str message + "'."

let endDrag (form : NuEditForm) _ _ message world =
    match message.Data with
    | MouseButtonData (position, _) ->
        if form.InteractButton.Checked then (message, world)
        else
            match world.ExtData :?> DragState with
            | DragNone -> (handle message, world)
            | DragPosition _ -> (handle message, { world with ExtData = DragNone })
            | DragRotation _ -> (handle message, { world with ExtData = DragNone })
    | _ -> failwith <| "Expected MouseButtonData in message '" + str message + "'."

let updateDrag (form : NuEditForm) world =
    let dragState = world.ExtData :?> DragState
    match dragState with
    | DragNone -> world
    | DragPosition (pickOffset, origMousePosition, address) ->
        let entityModel = get world <| worldEntityModel address
        let transform = getEntityModelTransform true world.Camera entityModel
        let transform_ = { transform with Position = (pickOffset - origMousePosition) + (world.MouseState.MousePosition - origMousePosition) }
        let (positionSnap, rotationSnap) = getSnaps form
        let entityModel_ = setEntityModelTransform true world.Camera positionSnap rotationSnap transform_ entityModel
        let world_ = set entityModel_ world <| worldEntityModel address
        let world_ = trySetEntityModelTransformToPhysics entityModel_ world_
        { world_ with ExtData = DragPosition (pickOffset, origMousePosition, address) }
    | DragRotation (pickOffset, origPosition, address) -> world

let [<EntryPoint; STAThread>] main _ =

    initTypeConverters ()
    use form = new NuEditForm ()
    form.displayPanel.MaximumSize <- Drawing.Size (900, 600)
    form.positionSnapTextBox.Text <- str DefaultPositionSnap
    form.rotationSnapTextBox.Text <- str DefaultRotationSnap
    let sdlViewConfig = ExistingWindow form.displayPanel.Handle
    let sdlRenderFlags = enum<SDL.SDL_RendererFlags> (int SDL.SDL_RendererFlags.SDL_RENDERER_ACCELERATED ||| int SDL.SDL_RendererFlags.SDL_RENDERER_PRESENTVSYNC)
    let sdlConfig = makeSdlConfig sdlViewConfig form.displayPanel.MaximumSize.Width form.displayPanel.MaximumSize.Height sdlRenderFlags 1024
    let refWorld = ref Unchecked.defaultof<World>
    run4

        (fun sdlDeps ->

            let screen = { Id = getNuId (); GroupModels = Map.empty }
            let group = { Id = getNuId (); EntityModels = Map.empty }
            refWorld := createEmptyWorld sdlDeps DragNone
            refWorld := addScreen Test.ScreenModelAddress screen !refWorld
            refWorld := addGroup Test.GroupModelAddress group !refWorld
            refWorld := subscribe DownMouseLeftAddress [] (beginDrag form refWorld) !refWorld
            refWorld := subscribe UpMouseLeftAddress [] (endDrag form) !refWorld

            form.exitToolStripMenuItem.Click.Add (fun _ ->
                form.Close ())

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
                    gWorldChangers.Add changer
                | _ -> ())

            form.Show ()
            Right !refWorld)

        (fun world ->
            refWorld := updateDrag form world
            refWorld := Seq.fold (fun world changer -> changer world) !refWorld gWorldChangers
            gWorldChangers.Clear ()
            (not form.IsDisposed, !refWorld))

        (fun world ->
            form.displayPanel.Invalidate ()
            world)

        sdlConfig