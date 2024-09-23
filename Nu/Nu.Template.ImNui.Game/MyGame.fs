namespace MyGame
open System
open System.Numerics
open Prime
open Nu

// this is our top-level MMCC model type. It determines what state the game is in. To learn about MMCC in Nu, see -
// https://github.com/bryanedds/Nu/wiki/Model-View-Update-for-Games-via-MMCC
type MyGame =
    | Splash
    | Title
    | Credits
    | Gameplay

// this extends the Game API to expose the above MMCC model as a property.
[<AutoOpen>]
module MyGameExtensions =
    type Game with
        member this.GetMyGame world = this.GetModelGeneric<MyGame> world
        member this.SetMyGame value world = this.SetModelGeneric<MyGame> value world
        member this.MyGame = this.ModelGeneric<MyGame> ()

// this is the dispatcher that customizes the top-level behavior of our game.
type MyGameDispatcher () =
    inherit GameDispatcher<MyGame> (Splash)

    override this.Run (myGame, _, world) : MyGame * World =

        // declare splash screen
        let (result, world) = World.beginScreen Simulants.Splash.Name (myGame = Splash) (Slide (Constants.Dissolve.Default, Constants.Slide.Default, None, Simulants.Title)) world []
        let myGame =
            match result |> Seq.filter (function Deselecting -> true | _ -> false) |> Seq.tryHead with
            | Some _ -> Title
            | None -> myGame
        let world = World.endScreen world

        // declare title screen
        let (_, world) = World.beginScreenWithGroupFromFile Simulants.Title.Name (myGame = Title) (Dissolve (Constants.Dissolve.Default, None)) "Assets/Gui/Title.nugroup" world []
        let world = World.beginGroup "Gui" world []
        let (myGame, world) =
            match World.doButton "Play" world [] with
            | (true, world) -> (Gameplay, world)
            | (false, world) -> (myGame, world)
        let (myGame, world) =
            match World.doButton "Credits" world [] with
            | (true, world) -> (Credits, world)
            | (false, world) -> (myGame, world)
        let world =
            match World.doButton "Exit" world [] with
            | (true, world) -> World.exit world
            | (false, world) -> world
        let world = World.endGroup world
        let world = World.endScreen world

        // declare credits screen
        let (_, world) = World.beginScreenWithGroupFromFile Simulants.Credits.Name (myGame = Credits) (Dissolve (Constants.Dissolve.Default, None)) "Assets/Gui/Credits.nugroup" world []
        let world = World.beginGroup "Gui" world []
        let (myGame, world) =
            match World.doButton "Back" world [] with
            | (true, world) -> (Title, world)
            | (false, world) -> (myGame, world)
        let world = World.endGroup world
        let world = World.endScreen world

        // declare gameplay screen
        let (result, world) = World.beginScreen<GameplayDispatcher> Simulants.Gameplay.Name (myGame = Gameplay) (Dissolve (Constants.Dissolve.Default, None)) world []
        let gameplayScreen = world.ContextScreen
        let world =
            if Seq.contains Select result // TODO: P0: change to FStack.contains.
            then gameplayScreen.SetGameplay { gameplayScreen.GetGameplay world with GameplayState = Playing } world
            else world
        let myGame =
            if gameplayScreen.GetSelected world && (gameplayScreen.GetGameplay world).GameplayState = Quitting
            then Title
            else myGame
        let world = World.endScreen world

        // handle Alt+F4
        let world =
            if World.isKeyboardAltDown world && World.isKeyboardKeyDown KeyboardKey.F4 world
            then World.exit world
            else world
        (myGame, world)