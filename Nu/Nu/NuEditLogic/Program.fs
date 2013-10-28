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

type EntityPropertyChange =
    { Address : Address
      FieldInfo : FieldInfo
      Value : obj }

// NOTE: I believe .NET's lack of parameterization in one aspect is forcing me to use this global.
let gEntityChanges = List<EntityPropertyChange> ()

let applyEntityChange world (change : EntityPropertyChange) =
    let entityLens = World.entity change.Address
    let entity = get world entityLens
    let entity2 = { entity with Id = entity.Id } // NOTE: this is just a hacky way to copy an entity
    change.FieldInfo.SetValue (entity2, change.Value)
    set entity2 world entityLens

let applyEntityChanges entityChanges world =
    Seq.fold applyEntityChange world entityChanges

type [<TypeDescriptionProvider (typeof<EntityTypeDescriptorProvider>)>] EntityTypeDescriptorSource =
    { Address : Address
      RefRefWorld : World ref ref }

and EntityPropertyDescriptor (fieldInfo : FieldInfo) =
    inherit PropertyDescriptor (fieldInfo.Name.Substring (0, fieldInfo.Name.Length - 1), Array.empty)
    override this.ComponentType with get () = typeof<obj>
    override this.PropertyType with get () = fieldInfo.FieldType
    override this.CanResetValue source = false
    override this.ResetValue source = ()
    override this.ShouldSerializeValue source = true
    override this.IsReadOnly with get () = false
    override this.GetValue source =
        let entityTds = source :?> EntityTypeDescriptorSource
        let entityLens = World.entity entityTds.Address
        let entity = get entityTds.RefRefWorld.Value.Value entityLens
        fieldInfo.GetValue entity
    override this.SetValue (source, value) =
        let entityTds = source :?> EntityTypeDescriptorSource
        let entityChange = { Address = entityTds.Address; FieldInfo = fieldInfo; Value = value }
        entityTds.RefRefWorld.Value := applyEntityChange entityTds.RefRefWorld.Value.Value entityChange
        gEntityChanges.Add entityChange

and EntityTypeDescriptor () =
    inherit CustomTypeDescriptor ()
    let Pdc =
        let ty = typeof<Entity>
        let fields = ty.GetFields (BindingFlags.Instance ||| BindingFlags.NonPublic)
        let propertyDescriptors = Array.map (fun field -> new EntityPropertyDescriptor (field) :> PropertyDescriptor) fields
        PropertyDescriptorCollection propertyDescriptors
    override this.GetProperties optAttributes = Pdc

and EntityTypeDescriptorProvider () =
    inherit TypeDescriptionProvider ()
    let Etd = EntityTypeDescriptor () :> ICustomTypeDescriptor
    override this.GetTypeDescriptor (_, _) = Etd

[<EntryPoint; STAThread>]
let main _ =
    use form = new NuEditForm ()
    let sdlViewConfig = ExistingWindow form.displayPanel.Handle
    let sdlRenderFlags = enum<SDL.SDL_RendererFlags> (int SDL.SDL_RendererFlags.SDL_RENDERER_ACCELERATED ||| int SDL.SDL_RendererFlags.SDL_RENDERER_PRESENTVSYNC)
    let sdlConfig = makeSdlConfig sdlViewConfig 900 600 sdlRenderFlags 1024
    let refRefWorld = ref <| ref Unchecked.defaultof<World> // ugh
    run4
        (fun sdlDeps ->

            refRefWorld.Value := createEmptyWorld sdlDeps

            match tryGenerateAssetMetadataMap "AssetGraph.xml" with
            | Left errorMsg -> Left errorMsg
            | Right assetMetadataMap ->

                let testScreenAddress = [Lun.make "testScreen"]
                let testGroupAddress = testScreenAddress @ [Lun.make "testGroup"]
                let testFeelerAddress = testGroupAddress @ [Lun.make "testFeeler"]
                let testTextBoxAddress = testGroupAddress @ [Lun.make "testTextBox"]

                let testScreen =
                    { Id = getNuId ()
                      Groups = Map.empty }

                let testGroup =
                    { Id = getNuId ()
                      Entities = Map.empty }
          
                let testTextBox =
                    { BoxSprite = { SpriteAssetName = Lun.make "Image3"; PackageName = Lun.make "Misc"; PackageFileName = "AssetGraph.xml" }
                      Text = "Hi!"
                      TextFont = { FontAssetName = Lun.make "Font"; PackageName = Lun.make "Misc"; PackageFileName = "AssetGraph.xml" }
                      TextOffset = Vector2 4.0f
                      TextColor = Vector4.One }

                let testTextBoxGui =
                    { Position = Vector2 (120.0f, 50.0f)
                      Depth = 0.1f
                      Size = getTextureSizeAsVector2 (Lun.make "Image3") (Lun.make "Misc") assetMetadataMap
                      GuiSemantic = TextBox testTextBox }

                let testTextBoxGuiEntity =
                    { Id = getNuId ()
                      IsEnabled = true
                      IsVisible = true
                      EntitySemantic = Gui testTextBoxGui }
        
                refRefWorld.Value := addScreen testScreen testScreenAddress refRefWorld.Value.Value
                refRefWorld.Value := setP (Some testScreenAddress) World.optActiveScreenAddress refRefWorld.Value.Value
                refRefWorld.Value := addGroup testGroup testGroupAddress refRefWorld.Value.Value
                refRefWorld.Value := addEntityGuiTextBox (testTextBoxGuiEntity, testTextBoxGui, testTextBox) testTextBoxAddress refRefWorld.Value.Value

                let testTypeSource = { Address = testTextBoxAddress; RefRefWorld = refRefWorld }
                form.propertyGrid.SelectedObject <- testTypeSource

                form.exitToolStripMenuItem.Click.Add (fun _ -> form.Close ())
                form.Show ()
                Right refRefWorld.Value.Value)
        (fun world ->
            refRefWorld.Value := applyEntityChanges gEntityChanges world
            (not form.IsDisposed, refRefWorld.Value.Value))
        (fun world ->
            form.displayPanel.Invalidate ()
            world)
        sdlConfig