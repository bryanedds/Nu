namespace MyGame
open System
open System.Numerics
open Prime
open Nu

// this module provides global handles to the game's key simulants.
// having a Simulants module for your game is optional, but can be nice to avoid duplicating string literals across
// the code base.
[<RequireQualifiedAccess>]
module Simulants =

    // title screen
    let Title = Game / "Title"
    let TitleGui = Title / "Gui"
    let TitleExit = TitleGui / "Exit"
    let TitleScene = Title / "Scene"

// this is our top-level MMCC model type. It determines what state the game is in. To learn about MMCC in Nu, see -
// https://github.com/bryanedds/Nu/wiki/Model-View-Update-for-Games-via-MMCC
type MyGame =
    | Title

// this is our top-level MMCC message type. The Nil message is just a placeholder message that doesn't do anything.
type MyGameMessage =
    | Nil
    interface Message
        
// this is our top-level MMCC command type. Commands are used instead of messages when the world is to be transformed.
type MyGameCommand =
    | Exit
    interface Command

// this extends the Game API to expose the above MMCC model as a property.
[<AutoOpen>]
module MyGameExtensions =
    type Game with
        member this.GetMyGame world = this.GetModelGeneric<MyGame> world
        member this.SetMyGame value world = this.SetModelGeneric<MyGame> value world
        member this.MyGame = this.ModelGeneric<MyGame> ()

// this is the dispatcher customizes the top-level behavior of our game. In here, we create screens as content and bind
// them up with events and properties.
type MyGameDispatcher () =
    inherit GameDispatcher<MyGame, MyGameMessage, MyGameCommand> (Title)

    // here we define the game's properties and event handling
    override this.Definitions (myGame, _) =
        [Game.DesiredScreen :=
            match myGame with
            | Title -> Desire Simulants.Title
            Simulants.TitleExit.ClickEvent => Exit]

    // here we handle the above messages
    override this.Message (myGame, message, _, _) =
        match message with
        | Nil -> just myGame

    // here we handle the above commands
    override this.Command (_, command, _, world) =
        match command with
        | Exit ->
            if world.Unaccompanied
            then just (World.exit world)
            else just world

    // here we describe the content of the game.
    override this.Content (_, _) =
        [Content.screen Simulants.Title.Name Vanilla []
            [Content.group Simulants.TitleGui.Name []
                [Content.button Simulants.TitleExit.Name
                    [Entity.Text == "Exit"]
                 (* insert more gui content here... *)]
             Content.group Simulants.TitleScene.Name []
                [Content.block2d "SomeOldBlock"
                    [Entity.Position == v3 0.0f 64.0f 0.0f]
                 (* insert more gui content here... *)]]]