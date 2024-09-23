namespace MyGame
open System
open System.Numerics
open Prime
open Nu

type GameplayResult =
    | KeepPlaying
    | StartQuitting

// this is our MMCC model type representing gameplay.
// this model representation uses update time, that is, time based on number of engine updates.
type Gameplay =
    { GameplayTime : int64 }

    // this represents the gameplay model in an unutilized state, such as when the gameplay screen is not selected.
    static member empty =
        { GameplayTime = 0L }

    // this represents the gameplay model in its initial state, such as when gameplay starts.
    static member initial =
        Gameplay.empty

// this extends the Screen API to expose the Gameplay model as well as the Quit event.
[<AutoOpen>]
module GameplayExtensions =
    type Screen with
        member this.GetGameplay world = this.GetModelGeneric<Gameplay> world
        member this.SetGameplay value world = this.SetModelGeneric<Gameplay> value world
        member this.Gameplay = this.ModelGeneric<Gameplay> ()

// this is the dispatcher that defines the behavior of the screen where gameplay takes place.
type GameplayDispatcher () =
    inherit ScreenDispatcher<Gameplay> (Gameplay.empty)

    // here we define the behavior of our gameplay
    override this.Run (gameplay, screen, world) =

        // scope to screen
        let world = World.scopeScreen screen world []

        // declare scene group when selected
        let world =
            if screen.GetSelected world then
                let world = World.beginGroupFromFile Simulants.GameplayScene.Name "Assets/Gameplay/Scene.nugroup" world []
                let world =
                    World.doStaticModel "StaticModel" world 
                        [Entity.Position .= v3 0.0f 0.0f -2.0f
                         Entity.Rotation @= Quaternion.CreateFromAxisAngle ((v3 1.0f 0.75f 0.5f).Normalized, gameplay.GameplayTime % 360L |> single |> Math.DegreesToRadians)]
                World.endGroup world
            else world

        // declare gui group
        let world = World.beginGroup Simulants.GameplayGui.Name world []
        let (_, world) = World.doButton Simulants.GameplayQuit.Name world [Entity.Position .= v3 232.0f -144.0f 0.0f; Entity.Text .= "Text"]
        let world = World.endGroup world

        // terminate scope
        let world = World.scopeWorld world

        // advance gameplay time
        let gameDelta = world.GameDelta
        let gameplay = { gameplay with GameplayTime = gameplay.GameplayTime + gameDelta.Updates }
        (gameplay, world)

[<AutoOpen>]
module GameplayExtensions2 =
    type World with
        static member beginScreenGameplay name select behavior world args =
            let init mapResult gameplay world = World.monitor (fun _ world -> (Cascade, mapResult (fun _ -> StartQuitting) world)) Simulants.GameplayQuit.ClickEvent gameplay world
            World.beginScreenPlus<GameplayDispatcher, GameplayResult> KeepPlaying init name select behavior world args