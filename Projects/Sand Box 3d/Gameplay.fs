namespace SandBox3d
open System
open System.Numerics
open Prime
open Nu
open SandBox3d

// this represents the state of gameplay simulation.
type GameplayState =
    | Playing
    | Quit

// this extends the Screen API to expose the GameplayState property.
[<AutoOpen>]
module GameplayExtensions =
    type Screen with
        member this.GetGameplayState world : GameplayState = this.Get (nameof Screen.GameplayState) world
        member this.SetGameplayState (value : GameplayState) world = this.Set (nameof Screen.GameplayState) value world
        member this.GameplayState = lens (nameof Screen.GameplayState) this this.GetGameplayState this.SetGameplayState

// this is the dispatcher that defines the behavior of the screen where gameplay takes place.
type GameplayDispatcher () =
    inherit ScreenDispatcherImSim ()

    // here we define default property values
    static member Properties =
        [define Screen.GameplayState Quit]

    // here we define the behavior of our gameplay
    override this.Process (_, screen, world) =

        // only process when selected
        if screen.GetSelected world then

            // begin scene declaration
            // NOTE: unlike MMCC, ImSim works more smoothly by placing gui entities in the default Scene group instead
            // of in their own separate Gui group. This is because, by default, ImSim ties simulant declaration
            // order together with simulant processing order. While common scene content in ImSim is still saved in /
            // loaded from a .nugroup file, you might instead consider saving / loading additive content in .nuentity
            // files. Lastly, note that you can save multiple entities to a single .nuentity file by parenting them
            // under a single root entity.
            World.beginGroupFromFile "Scene" "Assets/Gameplay/Scene.nugroup" [] world

            // declare static model
            let degrees = world.ScaledTime.Seconds * 60.0 % 360.0 // 60 degrees a second
            let rotation = Quaternion.CreateFromAxisAngle ((v3 1.0f 0.75f 0.5f).Normalized, degrees |> single |> degToRadF)
            World.doStaticModel "StaticModel" [Entity.Scale .= v3Dup 0.5f; Entity.Rotation @= rotation] world

            // declare quit button
            if World.doButton "Quit" [Entity.Position .= v3 232.0f -144.0f 0.0f; Entity.Text .= "Quit"] world then
                screen.SetGameplayState Quit world

            // ensure game is unpaused when quitting
            if screen.GetGameplayState world = Quit then
                World.setTimeAdvancing true world

            // end scene declaration
            World.endGroup world