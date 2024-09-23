namespace MyGame
open System
open System.Numerics
open Prime
open Nu

type GameplayState =
    | Playing
    | Quitting

// this is our MMCC model type representing gameplay.
// this model representation uses update time, that is, time based on number of engine updates.
type Gameplay =
    { GameplayTime : int64
      GameplayState : GameplayState }

    // this represents the gameplay model in an unutilized state, such as when the gameplay screen is not selected.
    static member empty =
        { GameplayTime = 0L
          GameplayState = Quitting }

    // this represents the gameplay model in its initial state, such as when gameplay starts.
    static member initial =
        { Gameplay.empty with
            GameplayState = Playing }

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

    // here we define the screen's fallback model depending on whether screen is selected
    override this.GetFallbackModel (_, screen, world) =
        if screen.GetSelected world
        then Gameplay.initial
        else Gameplay.empty

    // here we define the behavior of our gameplay
    override this.Run (gameplay, screen, world) =

        // declare scene group when selected
        let world =
            if screen.GetSelected world then
                let world = World.beginGroupFromFile "Scene" "Assets/Gameplay/Scene.nugroup" [] world
                let rotation = Quaternion.CreateFromAxisAngle ((v3 1.0f 0.75f 0.5f).Normalized, gameplay.GameplayTime % 360L |> single |> Math.DegreesToRadians)
                let world = World.doStaticModel "StaticModel" [Entity.Position .= v3 0.0f 0.0f -2.0f; Entity.Rotation @= rotation] world
                World.endGroup world
            else world

        // declare gui group
        let world = World.beginGroup "Gui" [] world
        let (gameplay, world) =
            match World.doButton "Quit" [Entity.Text .= "Quit"; Entity.Position .= v3 232.0f -144.0f 0.0f] world with
            | (true, world) -> ({ gameplay with GameplayState = Quitting }, world)
            | (false, world) -> (gameplay, world)
        let world = World.endGroup world

        // advance gameplay time
        let gameDelta = world.GameDelta
        let gameplay = { gameplay with GameplayTime = gameplay.GameplayTime + gameDelta.Updates }
        (gameplay, world)