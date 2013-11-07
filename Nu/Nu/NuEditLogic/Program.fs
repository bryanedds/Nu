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
open Nu.AssetMetadata
open Nu.Sdl
open Nu.Entity
open Nu.Group
open Nu.Screen
open Nu.Game
open Nu.Simulation
open NuEditLogic.Entity

// NOTE: I believe .NET's lack of parameterization in one aspect is forcing me to use this global-
// style variable (but perhaps I've merely overlooked how to parameterize this?)
let private gWorldChangers = List<World -> World> ()

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
        let entityModelLens = World.entityModel entityModelTds.Address
        let entityModel = get entityModelTds.RefWorld.Value entityModelLens
        getEntityModelPropertyValue property entityModel

    override this.SetValue (source, value) =
        let entityModelTds = source :?> EntityModelTypeDescriptorSource
        let changer = (fun world -> setEntityModelPropertyValue entityModelTds.Address property value world)
        // NOTE: even though this ref world will eventually get blown away, it must still be
        // updated here so that the change is reflected immediately by the property grid.
        entityModelTds.RefWorld := changer entityModelTds.RefWorld.Value
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
                let entityModelLens = World.entityModel source.Address
                let entityModel = get source.RefWorld.Value entityModelLens
                let entityModelTypes = getEntityModelTypes entityModel
                // NOTE: this line could be simplified by a List.concatBy function.
                List.fold (fun propertyDescriptors aType -> EntityModelPropertyDescriptor.GetPropertyDescriptors aType @ propertyDescriptors) [] entityModelTypes
            | _ -> EntityModelPropertyDescriptor.GetPropertyDescriptors typeof<EntityModel>
        PropertyDescriptorCollection (Array.ofList propertyDescriptors)

and EntityModelTypeDescriptorProvider () =
    inherit TypeDescriptionProvider ()
    override this.GetTypeDescriptor (_, optSource) =
        EntityModelTypeDescriptor optSource :> ICustomTypeDescriptor

let [<EntryPoint; STAThread>] main _ =

    initTypeConverters ()
    use form = new NuEditForm ()
    let sdlViewConfig = ExistingWindow form.displayPanel.Handle
    let sdlRenderFlags = enum<SDL.SDL_RendererFlags> (int SDL.SDL_RendererFlags.SDL_RENDERER_ACCELERATED ||| int SDL.SDL_RendererFlags.SDL_RENDERER_PRESENTVSYNC)
    let sdlConfig = makeSdlConfig sdlViewConfig 900 600 sdlRenderFlags 1024
    let refWorld = ref Unchecked.defaultof<World>
    run4

        (fun sdlDeps ->
            refWorld := createEmptyWorld sdlDeps
            let screen = { Id = getNuId (); GroupModels = Map.empty }
            refWorld := addScreen Test.ScreenModelAddress screen refWorld.Value
            let group = { Id = getNuId (); EntityModels = Map.empty }
            refWorld := addGroup Test.GroupModelAddress group refWorld.Value

            refWorld := subscribe
                DownMouseLeftAddress
                []
                (fun _ _ message world ->
                    match message.Data with
                    | MouseButtonData (position, _) ->
                        if form.InteractButton.Checked then (message, world)
                        else
                            let groupModelAddress = Test.GroupModelAddress
                            let groupModel = get world (World.groupModel groupModelAddress)
                            let entityModels = Map.toValueList <| (get groupModel GroupModel.group).EntityModels
                            let optPicked = tryPick position entityModels world
                            match optPicked with
                            | None -> (handle message, world)
                            | Some picked ->
                                let entity = get picked EntityModel.entity
                                let entityModelAddress = groupModelAddress @ [Lun.make entity.Name]
                                form.propertyGrid.SelectedObject <- { Address = entityModelAddress; RefWorld = refWorld }
                                (handle message, world)
                    | _ -> failwith <| "Expected MouseButtonData in message '" + str message + "'.")
                refWorld.Value

            let testTypeSource = { Address = Test.ButtonAddress; RefWorld = refWorld }
            form.propertyGrid.SelectedObject <- testTypeSource

            form.exitToolStripMenuItem.Click.Add (fun _ -> form.Close ())

            form.saveToolStripMenuItem.Click.Add (fun _ ->
                let saveFileResult = form.saveFileDialog.ShowDialog form
                match saveFileResult with
                | DialogResult.OK -> writeFile form.saveFileDialog.FileName refWorld.Value
                | _ -> ())

            form.openToolStripMenuItem.Click.Add (fun _ ->
                let openFileResult = form.openFileDialog.ShowDialog form
                match openFileResult with
                | DialogResult.OK ->
                    let changer = readFile form.openFileDialog.FileName
                    refWorld := changer refWorld.Value
                    gWorldChangers.Add changer
                | _ -> ())

            form.Show ()
            Right refWorld.Value)

        (fun world ->
            refWorld := Seq.fold (fun world changer -> changer world) world gWorldChangers
            gWorldChangers.Clear ()
            (not form.IsDisposed, refWorld.Value))

        (fun world ->
            form.displayPanel.Invalidate ()
            world)

        sdlConfig