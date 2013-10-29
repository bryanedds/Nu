module NuEditLogic.Program
open NuEditDesign
open SDL2
open OpenTK
open System
open System.Collections.Generic
open System.Reflection
open System.Windows.Forms
open System.ComponentModel
open Nu.Sdl
open Nu.Core
open Nu.AssetMetadataMap
open Nu.Entity
open Nu.Group
open Nu.Screen
open Nu.Game
open Nu.Simulation
open Nu.TestGame

type EntityChange =
    { Address : Address
      FieldInfo : FieldInfo
      Value : obj }

let applyEntityChange world (change : EntityChange) =
    let entityLens = World.entity change.Address
    let entity = get world entityLens
    let fieldInfo = change.FieldInfo
    let value = change.Value
    let entity_ =
        let entity_ = { entity with Id = entity.Id } // NOTE: this is just a hacky way to copy an entity in lieu of reflection
        if entity_.GetType().GetField (fieldInfo.Name, BindingFlags.Instance ||| BindingFlags.NonPublic) <> null
        then let _ = fieldInfo.SetValue (entity_, value) in entity_
        else
            match entity.EntitySemantic with
            | Gui gui ->
                let gui_ = { gui with Position = gui.Position } // NOTE: hacky copy
                if gui_.GetType().GetField (fieldInfo.Name, BindingFlags.Instance ||| BindingFlags.NonPublic) <> null
                then let _ = fieldInfo.SetValue (gui_, value) in { entity with EntitySemantic = Gui gui_ }
                else
                    match gui.GuiSemantic with
                    | Button button ->
                        let button_ = { button with IsDown = button.IsDown } // NOTE: hacky copy
                        fieldInfo.SetValue (button_, value)
                        { entity with EntitySemantic = Gui { gui with GuiSemantic = Button button_ }}
                    | Label label ->
                        let label_ = { label with LabelSprite = label.LabelSprite } // NOTE: hacky copy
                        fieldInfo.SetValue (label_, value)
                        { entity with EntitySemantic = Gui { gui with GuiSemantic = Label label_ }}
                    | TextBox textBox ->
                        let textBox_ = { textBox with BoxSprite = textBox.BoxSprite } // NOTE: hacky copy
                        fieldInfo.SetValue (textBox_, value)
                        { entity with EntitySemantic = Gui { gui with GuiSemantic = TextBox textBox_ }}
                    | Toggle toggle ->
                        let toggle_ = { toggle with IsPressed = toggle.IsPressed } // NOTE: hacky copy
                        fieldInfo.SetValue (toggle_, value)
                        { entity with EntitySemantic = Gui { gui with GuiSemantic = Toggle toggle_ }}
                    | Feeler feeler ->
                        let feeler_ = { feeler with IsTouched = feeler.IsTouched } // NOTE: hacky copy
                        fieldInfo.SetValue (feeler_, value)
                        { entity with EntitySemantic = Gui { gui with GuiSemantic = Feeler feeler_ }}
            | Actor actor ->
                let actor_ = { actor with Position = actor.Position } // NOTE: hacky copy
                if actor_.GetType().GetField (fieldInfo.Name, BindingFlags.Instance ||| BindingFlags.NonPublic) <> null
                then let _ = fieldInfo.SetValue (actor_, value) in { entity with EntitySemantic = Actor actor_ }
                else
                    match actor.ActorSemantic with
                    | Block block ->
                        let block_ = { block with PhysicsId = block.PhysicsId } // NOTE: hacky copy
                        fieldInfo.SetValue (block_, value)
                        { entity with EntitySemantic = Actor { actor with ActorSemantic = Block block_ }}
                    | Avatar avatar ->
                        let avatar_ = { avatar with PhysicsId = avatar.PhysicsId } // NOTE: hacky copy
                        fieldInfo.SetValue (avatar_, value)
                        { entity with EntitySemantic = Actor { actor with ActorSemantic = Avatar avatar_ }}
                    | TileMap tileMap ->
                        let tileMap_ = { tileMap with PhysicsIds = tileMap.PhysicsIds } // NOTE: hacky copy
                        fieldInfo.SetValue (tileMap_, value)
                        { entity with EntitySemantic = Actor { actor with ActorSemantic = TileMap tileMap_ }}
    set entity_ world entityLens

let applyEntityChanges changes world =
    Seq.fold applyEntityChange world changes

// NOTE: I believe .NET's lack of parameterization in one aspect is forcing me to use this global-
// style variable (but perhaps I've merely overlooked how to parameterize this?)
let private gEntityChanges = List<EntityChange> ()

type [<TypeDescriptionProvider (typeof<EntityTypeDescriptorProvider>)>] EntityTypeDescriptorSource =
    { Address : Address
      RefWorld : World ref }

and EntityPropertyDescriptor (fieldInfo : FieldInfo) =
    inherit PropertyDescriptor (fieldInfo.Name, Array.empty)
    override this.ComponentType with get () = typeof<obj>
    override this.PropertyType with get () = fieldInfo.FieldType
    override this.CanResetValue source = false
    override this.ResetValue source = ()
    override this.ShouldSerializeValue source = true
    override this.IsReadOnly
        // NOTE: we make entity id read-only
        // TODO: use attribute from the corresponding property here instead
        with get () = fieldInfo.Name = "Id@"
    override this.GetValue source =
        let entityTds = source :?> EntityTypeDescriptorSource
        let entityLens = World.entity entityTds.Address
        let entity = get entityTds.RefWorld.Value entityLens
        if entity.GetType().GetField (fieldInfo.Name, BindingFlags.Instance ||| BindingFlags.NonPublic) <> null
        then fieldInfo.GetValue entity
        else
            match entity.EntitySemantic with
            | Gui gui ->
                if gui.GetType().GetField (fieldInfo.Name, BindingFlags.Instance ||| BindingFlags.NonPublic) <> null
                then fieldInfo.GetValue gui
                else
                    match gui.GuiSemantic with
                    | Button button -> fieldInfo.GetValue button
                    | Label label -> fieldInfo.GetValue label
                    | TextBox textBox -> fieldInfo.GetValue textBox
                    | Toggle toggle -> fieldInfo.GetValue toggle
                    | Feeler feeler -> fieldInfo.GetValue feeler
            | Actor actor ->
                if actor.GetType().GetField (fieldInfo.Name, BindingFlags.Instance ||| BindingFlags.NonPublic) <> null
                then fieldInfo.GetValue actor
                else
                    match actor.ActorSemantic with
                    | Block block -> fieldInfo.GetValue block
                    | Avatar avatar -> fieldInfo.GetValue avatar
                    | TileMap tileMap -> fieldInfo.GetValue tileMap
    override this.SetValue (source, value) =
        let entityTds = source :?> EntityTypeDescriptorSource
        let entityChange = { Address = entityTds.Address; FieldInfo = fieldInfo; Value = value }
        // NOTE: even though this ref world will eventually get blown away, it must still be
        // updated here so that the change is reflected immediately by the property grid.
        entityTds.RefWorld := applyEntityChange entityTds.RefWorld.Value entityChange
        gEntityChanges.Add entityChange
    static member GetPropertyDescriptors (aType : Type) =
        let fields = aType.GetFields (BindingFlags.Instance ||| BindingFlags.NonPublic)
        let propertyDescriptors = Seq.map (fun field -> new EntityPropertyDescriptor (field) :> PropertyDescriptor) fields
        // TODO: use attribute from the corresponding property here instead
        let propertyDescriptors2 = Seq.filter (fun (propertyDescriptor : PropertyDescriptor) -> not <| propertyDescriptor.Name.EndsWith "Semantic@") propertyDescriptors
        List.ofSeq propertyDescriptors2

and EntityTypeDescriptor (optSource : obj) =
    inherit CustomTypeDescriptor ()
    override this.GetProperties _ =
        let propertyDescriptors =
            match optSource with
            | :? EntityTypeDescriptorSource as source ->
                let entityLens = World.entity source.Address
                let entity = get source.RefWorld.Value entityLens
                let types =
                    match entity.EntitySemantic with
                    | Gui gui ->
                        let guiSemType =
                            match gui.GuiSemantic with
                            | Button _ -> typeof<Button>
                            | Label _ -> typeof<Label>
                            | TextBox _ -> typeof<TextBox>
                            | Toggle _ -> typeof<Toggle>
                            | Feeler _ -> typeof<Feeler>
                        [typeof<Entity>; typeof<Gui>; guiSemType]
                     | Actor actor ->
                        let actorSemType =
                            match actor.ActorSemantic with
                            | Block _ -> typeof<Block>
                            | Avatar _ -> typeof<Avatar>
                            | TileMap _ -> typeof<TileMap>
                        [typeof<Entity>; typeof<Actor>; actorSemType]
                // NOTE: this line could be simplified by a List.concatBy function.
                List.fold (fun propertyDescriptors aType -> EntityPropertyDescriptor.GetPropertyDescriptors aType @ propertyDescriptors) [] types
            | _ -> EntityPropertyDescriptor.GetPropertyDescriptors typeof<Entity>
        PropertyDescriptorCollection (Array.ofList propertyDescriptors)

and EntityTypeDescriptorProvider () =
    inherit TypeDescriptionProvider ()
    override this.GetTypeDescriptor (_, optSource) = EntityTypeDescriptor optSource :> ICustomTypeDescriptor

let [<EntryPoint; STAThread>] main _ =
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
                form.Show ()
                Right refWorld.Value)
        (fun world ->
            // NOTE: the old refWorld will be blown away here!
            refWorld := applyEntityChanges gEntityChanges world
            gEntityChanges.Clear ()
            let (keepRunning, world2) = testHandleUpdate refWorld.Value
            refWorld := world2
            (keepRunning && not form.IsDisposed, refWorld.Value))
        (fun world ->
            form.displayPanel.Invalidate ()
            world)
        sdlConfig