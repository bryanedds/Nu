namespace MyGame
open System
open System.Numerics
open Prime
open Nu

// this is our top-level MMCC model type. It determines what state the game is in. To learn about MMCC in Nu, see -
// https://github.com/bryanedds/Nu/wiki/Model-View-Update-for-Games-via-MMCC
type MyGame =
    { MyGameTime : int64 }
    static member initial = { MyGameTime = 0L }

// this is our top-level MMCC message type.
type MyGameMessage =
    | Update
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

// this is the dispatcher that customizes the top-level behavior of our game. In here, we create screens as content and
// bind them up with events and properties.
type MyGameDispatcher () =
    inherit GameDispatcher<MyGame, MyGameMessage, MyGameCommand> (MyGame.initial)

    // here we define the game's properties and event handling
    override this.Definitions (_, _) =
        [Game.UpdateEvent => Update]

    // here we handle the above messages
    override this.Message (myGame, message, _, _) =
        match message with
        | Update ->
            let myGame = { myGame with MyGameTime = inc myGame.MyGameTime }
            just myGame

    // here we handle the above commands
    override this.Command (_, command, _, world) =
        match command with
        | Exit ->
            if world.Unaccompanied
            then just (World.exit world)
            else just world

    // here we describe the content of the game, including a screen, a group, and a couple example entities.
    override this.Content (myGame, _) =
        [Content.screen "Screen" Vanilla []
            [Content.group "Group" []
                [Content.button "Exit"
                    [Entity.Position == v3 232.0f -144.0f 0.0f
                     Entity.Elevation == 10.0f
                     Entity.Text == "Exit"
                     Entity.ClickEvent => Exit]
                 Content.staticModel "StaticModel"
                    [Entity.Position == v3 0.0f 1.0f 0.0f
                     Entity.Rotation := Quaternion.CreateFromAxisAngle ((v3 1.0f 0.75f 0.5f).Normalized, myGame.MyGameTime % 360L |> single |> Math.DegreesToRadians)]]]]