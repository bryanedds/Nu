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
        entity.Rotation.Update ((+) 0.01f) world

// this is the game dispatcher that is customized for our game. In here, we create screens and wire
// them up with subsciptions and transitions.
type MyGameDispatcher () =
    inherit GameDispatcher ()
    
    override dispatcher.Register (_, world) =
        let world = World.createScreen (Some Default.Screen.ScreenName) world |> snd
        let world = World.createLayer (Some Default.Layer.LayerName) Default.Screen world |> snd
        let world =
            Array.fold2 (fun world i j ->
                let (entity, world) = World.createEntity<MyEntityDispatcher> None DefaultOverlay Default.Layer world
                let world = entity.SetPosition (v2 ((single i) * 5.0f) ((single j) * 5.0f)) world
                world)
                world
                [|0 .. 22|]
                [|0 .. 22|]
        world

// this is a plugin for the Nu game engine by which user-defined dispatchers, facets, and other
// sorts of types can be obtained by both your application and Gaia.
type MyGamePlugin () =
    inherit NuPlugin ()

    // make our game-specific game dispatcher...
    override this.MakeGameDispatchers () =
        [MyGameDispatcher () :> GameDispatcher]

    // specify the above game dispatcher to use
    override this.GetStandAloneGameDispatcherName () =
        typeof<MyGameDispatcher>.Name
    
// this is the main module for our program.
module Program =

    // this the entry point for your Nu application
    let [<EntryPoint; STAThread>] main _ =

        // initialize Nu
        Nu.init false

        // this specifies the window configuration used to display the game.
        let sdlWindowConfig = { SdlWindowConfig.defaultConfig with WindowTitle = "MyGame" }
        
        // this specifies the configuration of the game engine's use of SDL.
        let sdlConfig = { SdlConfig.defaultConfig with ViewConfig = NewWindow sdlWindowConfig }

        // this is a callback that attempts to make 'the world' in a functional programming
        // sense. In a Nu game, the world is represented as an abstract data type named World.
        let tryMakeWorld sdlDeps =

            // an instance of the above plugin
            let plugin = MyGamePlugin ()

            // here is an attempt to make the world with the various initial states, the engine
            // plugin, and SDL dependencies.
            World.tryMake true 1L () plugin sdlDeps

        // after some configuration it is time to run the game. We're off and running!
        World.run tryMakeWorld sdlConfig