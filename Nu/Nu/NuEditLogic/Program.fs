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
open Nu.TestGame
open NuEditLogic.Entity

// NOTE: I believe .NET's lack of parameterization in one aspect is forcing me to use this global-
// style variable (but perhaps I've merely overlooked how to parameterize this?)
let private gEntityModelChanges = List<EntityModelPropertyChange> ()

type [<TypeDescriptionProvider (typeof<EntityModelTypeDescriptorProvider>)>] EntityModelTypeDescriptorSource =
    { Address : Address
      RefWorld : World ref }

and EntityModelPropertyDescriptor (property : PropertyInfo) =
    inherit PropertyDescriptor (property.Name, Array.empty)
    override this.ComponentType with get () = null // TODO: figure out what this definition should be!
    override this.PropertyType with get () = property.PropertyType
    override this.CanResetValue source = false
    override this.ResetValue source = ()
    override this.ShouldSerializeValue source = true

    override this.IsReadOnly
        // NOTE: we make entity id read-only
        with get () = property.Name = "Id"

    override this.GetValue source =
        let entityMtds = source :?> EntityModelTypeDescriptorSource
        let entityModelLens = World.entityModel entityMtds.Address
        let entityModel = get entityMtds.RefWorld.Value entityModelLens
        getEntityModelPropertyValue property entityModel

    override this.SetValue (source, value) =
        let entityMtds = source :?> EntityModelTypeDescriptorSource
        let entityModelChange = { Address = entityMtds.Address; PropertyInfo = property; Value = value }
        // NOTE: even though this ref world will eventually get blown away, it must still be
        // updated here so that the change is reflected immediately by the property grid.
        entityMtds.RefWorld := setEntityModelPropertyValue entityMtds.RefWorld.Value entityModelChange
        gEntityModelChanges.Add entityModelChange

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
            let optTestWorld = tryCreateTestWorld sdlDeps
            match optTestWorld with
            | Left errorMsg -> Left errorMsg
            | Right testWorld ->
                refWorld := testWorld
                let testTypeSource = { Address = TestButtonAddress; RefWorld = refWorld }
                form.propertyGrid.SelectedObject <- testTypeSource
                form.exitToolStripMenuItem.Click.Add (fun _ -> form.Close ())
                form.saveToolStripMenuItem.Click.Add (fun _ ->
                    use file = File.Open ("temp.xml", FileMode.Create)
                    let writerSettings = XmlWriterSettings ()
                    writerSettings.Indent <- true
                    use writer = XmlWriter.Create (file, writerSettings)
                    writer.WriteStartDocument ()
                    let testEntity = get refWorld.Value (World.entity TestButtonAddress)
                    //writeEntityToXml writer testEntity
                    writer.WriteEndDocument ())
                form.openToolStripMenuItem.Click.Add (fun _ ->
                    use file = File.Open ("temp.xml", FileMode.Open)
                    use reader = XmlReader.Create file
                    //let testEntity = readEntityFromXml reader
                    ())
                form.Show ()
                Right refWorld.Value)
        (fun world ->
            let world_ = setEntityModelPropertyValues gEntityModelChanges world
            gEntityModelChanges.Clear ()
            let (keepRunning, world_) = testHandleUpdate world_
            refWorld := world_ // the old refWorld is blown away here
            (keepRunning && not form.IsDisposed, world_))
        (fun world ->
            form.displayPanel.Invalidate ()
            world)
        sdlConfig