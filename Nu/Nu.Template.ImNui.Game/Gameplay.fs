namespace MyGame
open System
open System.Numerics
open Prime
open Nu
open MyGame

type GameplayState =
    | Playing
    | Quit

// this extends the Screen API to expose the Gameplay model as well as the Quit event.
[<AutoOpen>]
module GameplayExtensions =
    type Screen with
        member this.GetGameplayState world : GameplayState = this.Get (nameof Screen.GameplayState) world
        member this.SetGameplayState (value : GameplayState) world = this.Set (nameof Screen.GameplayState) value world
        member this.GameplayState = lens (nameof Screen.GameplayState) this this.GetGameplayState this.SetGameplayState

// this is the dispatcher that defines the behavior of the screen where gameplay takes place.
type GameplayDispatcher () =
    inherit ScreenDispatcherImNui ()

    // here we define default property values
    static member Properties =
        [define Screen.GameplayState Quit]

    // here we define the behavior of our gameplay
    override this.Process (_, gameplay, world) =

        // declare scene group
        let world = World.beginGroupFromFile "Scene" "Assets/Gameplay/Scene.nugroup" [] world
        let rotation = Quaternion.CreateFromAxisAngle ((v3 1.0f 0.75f 0.5f).Normalized, world.UpdateTime % 360L |> single |> Math.DegreesToRadians)
        let world = World.doStaticModel "StaticModel" [Entity.Scale .= v3Dup 0.5f; Entity.Rotation @= rotation] world
        let world = World.endGroup world

        // declare gui group
        let world = World.beginGroup "Gui" [] world
        let (clicked, world) = World.doButton "Quit" [Entity.Position .= v3 232.0f -144.0f 0.0f; Entity.Text .= "Quit"] world
        let world = if clicked then gameplay.SetGameplayState Quit world else world
        let world = World.endGroup world
        world

    // this is a semantic fix-up that allows the editor to avoid creating an unused group. This is specific to the
    // ImNui API that is needed to patch a little semantic hole inherent in the immediate-mode programming idiom.
    override this.CreateDefaultGroup (screen, world) = World.createGroup (Some "Gui") screen world