namespace MyGame
open System
open Prime
open Nu
open Nu.Declarative

type MyEntityDispatcher () =
    inherit EntityDispatcher ()

    static member FacetNames =
        [typeof<StaticSpriteFacet>.Name]

    static member Properties =
        [define Entity.Rotation 0.0f]

    override dispatcher.Update (entity, world) =
        entity.SetRotation (entity.GetRotation world + 0.02f) world

type MyGameDispatcher () =
    inherit GameDispatcher ()
    
    override dispatcher.Register (_, world) =
        let world = World.createScreen (Some Default.Screen.ScreenName) world |> snd
        let world = World.createLayer (Some Default.Layer.LayerName) Default.Screen world |> snd
        let indices =
            seq {
                for i in 0 .. 50 do
                    for j in 0 .. 30 do
                    yield v2 (single i * 16.0f) (single j * 16.0f) }
        let world =
            Seq.fold (fun world position ->
                let (entity, world) = World.createEntity<MyEntityDispatcher> None DefaultOverlay Default.Layer world
                let world = entity.SetPosition (position + v2 -400.0f -240.0f) world
                let world = entity.SetSize (v2One * 10.0f) world
                let world = entity.SetImperative true world // makes updates faster by using mutation
                let world = entity.SetIgnoreLayer true world // makes actualization faster by not touching the containing layer
                let world = entity.SetOmnipresent true world // makes updates faster by not touching the entity tree
                world)
                world
                indices
        let world = World.selectScreen Default.Screen world
        world

type MyGamePlugin () =
    inherit NuPlugin ()

    override this.MakeEntityDispatchers () =
        [MyEntityDispatcher () :> EntityDispatcher]

    override this.MakeGameDispatchers () =
        [MyGameDispatcher () :> GameDispatcher]

    override this.GetStandAloneGameDispatcherName () =
        typeof<MyGameDispatcher>.Name
    
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