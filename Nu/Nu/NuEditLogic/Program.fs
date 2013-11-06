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

GC.RemoveMemoryPressure 100000000L

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
        // NOTE: we make entity id read-only
        with get () = property.Name = "Id"

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
            refWorld := addScreen Test.ScreenAddress screen refWorld.Value
            let group = { Id = getNuId (); EntityModels = Map.empty }
            refWorld := addGroup Test.GroupAddress group refWorld.Value
            let testTypeSource = { Address = Test.ButtonAddress; RefWorld = refWorld }
            form.propertyGrid.SelectedObject <- testTypeSource
            form.exitToolStripMenuItem.Click.Add (fun _ -> form.Close ())
            form.saveToolStripMenuItem.Click.Add (fun _ ->
                let saveFileResult = form.saveFileDialog.ShowDialog form
                match saveFileResult with
                | DialogResult.OK ->
                    use file = File.Open (form.saveFileDialog.FileName, FileMode.Create)
                    let writerSettings = XmlWriterSettings ()
                    writerSettings.Indent <- true
                    use writer = XmlWriter.Create (file, writerSettings)
                    writer.WriteStartDocument ()
                    writer.WriteStartElement "Root"
                    let testGroupModel = get refWorld.Value <| World.groupModel Test.GroupAddress
                    writeGroupModelToXml writer testGroupModel
                    writer.WriteEndElement ()
                    writer.WriteEndDocument ()
                | _ -> ())
            form.openToolStripMenuItem.Click.Add (fun _ ->
                let openFileResult = form.openFileDialog.ShowDialog form
                match openFileResult with
                | DialogResult.OK ->
                    let changer = (fun world ->
                        let document = XmlDocument ()
                        document.Load form.openFileDialog.FileName
                        let rootNode = document.Item "Root"
                        let testGroupModelNode = rootNode.FirstChild
                        let (testGroupModel, testEntityModels) = loadGroupModelFromXml testGroupModelNode
                        let w_ = world
                        let w_ = removeGroupModel Test.GroupAddress w_
                        let w_ = addGroupModel Test.GroupAddress testGroupModel w_
                        addEntityModelsToGroup testEntityModels Test.GroupAddress w_)
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