namespace MyGame
open System
open OpenTK
open Prime
open Nu
open Nu.Declarative

type MyEntityDispatcher () =
    inherit EntityDispatcher ()

    // makes user-defined properties faster by using static data
    static member Properties =
        [define Entity.StaticData
            { DesignerType = typeof<Image AssetTag>
              DesignerValue = AssetTag.make<Image> Assets.DefaultPackage "Image4" }]

    override dispatcher.Update (entity, world) =
        entity.SetRotation (entity.GetRotation world + 0.03f) world

    override dispatcher.Actualize (entity, world) =
        if entity.GetVisibleLayered world && entity.GetInView world then
            let position = entity.GetPosition world
            let image = (entity.GetStaticData world).DesignerValue :?> Image AssetTag
            World.enqueueRenderMessage
                (RenderDescriptorsMessage
                    [|LayerableDescriptor
                        { Depth = entity.GetDepthLayered world
                          AssetTag = image
                          PositionY = position.Y
                          LayeredDescriptor =
                          SpriteDescriptor
                            { Position = position
                              Size = entity.GetSize world
                              Rotation = entity.GetRotation world
                              Offset = Vector2.Zero
                              ViewType = entity.GetViewType world
                              InsetOpt = None
                              Image = image
                              Color = Vector4.One }}|])
                world
        else world

type MyGameDispatcher () =
    inherit GameDispatcher ()
    
    override dispatcher.Register (_, world) =
        let world = World.createScreen (Some Default.Screen.ScreenName) world |> snd
        let world = World.createLayer (Some Default.Layer.LayerName) Default.Screen world |> snd
        let indices = // approximately 3000 entities
            seq {
                for i in 0 .. 70 do
                    for j in 0 .. 43 do
                    yield v2 (single i * 12.0f) (single j * 12.0f) }
        let world =
            Seq.fold (fun world position ->
                let (entity, world) = World.createEntity<MyEntityDispatcher> None DefaultOverlay Default.Layer world
                let world = entity.SetPosition (position + v2 -420.0f -265.0f) world
                let world = entity.SetSize (v2One * 8.0f) world
                let world = entity.SetImperative true world // makes updates faster by using mutation
                let world = entity.SetIgnoreLayer true world // makes actualization faster by not touching the containing layer
                let world = entity.SetOmnipresent true world // makes updates faster by not touching the entity tree
                world)
                world
                indices
        World.selectScreen Default.Screen world

type MyGamePlugin () =
    inherit NuPlugin ()
    override this.MakeEntityDispatchers () = [MyEntityDispatcher () :> EntityDispatcher]
    override this.MakeGameDispatchers () = [MyGameDispatcher () :> GameDispatcher]
    override this.GetStandAloneGameDispatcherName () = typeof<MyGameDispatcher>.Name
    
module Program =

    // this program exists to take metrics on Nu's performance
    let [<EntryPoint; STAThread>] main _ =
        Nu.init false
        let sdlWindowConfig = { SdlWindowConfig.defaultConfig with WindowTitle = "MyGame" }
        let sdlConfig = { SdlConfig.defaultConfig with ViewConfig = NewWindow sdlWindowConfig }
        let tryMakeWorld sdlDeps =
            let plugin = MyGamePlugin ()
            World.tryMake true 1L () plugin sdlDeps
        World.run tryMakeWorld sdlConfig