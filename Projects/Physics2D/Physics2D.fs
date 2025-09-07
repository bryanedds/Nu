namespace Physics2D
open System
open System.Numerics
open Prime
open Nu

// this determines what state the game is in. To learn about ImSim in Nu, see -
// https://github.com/bryanedds/Nu/wiki/Immediate-Mode-for-Games-via-ImSim
type GameState =
    | Enclosure

// this extends the Game API to expose GameState as a property.
[<AutoOpen>]
module Physics2DExtensions =
    type Game with
        member this.GetGameState world : GameState = this.Get (nameof Game.GameState) world
        member this.SetGameState (value : GameState) world = this.Set (nameof Game.GameState) value world
        member this.GameState = lens (nameof Game.GameState) this this.GetGameState this.SetGameState

// this is the dispatcher that customizes the top-level behavior of our game.
type Physics2DDispatcher () =
    inherit GameDispatcherImSim ()

    // here we define default property values
    static member Properties =
        [define Game.GameState Enclosure]

    // here we define game initialization
    override _.Register (_, world) = 
        World.setEye2dCenter (v2 60f 0f) world

    // here we define the game's top-level behavior
    override this.Process (game, world) =
        
        // declare PlaygroundDispatcher screen
        let behavior = Dissolve (Constants.Dissolve.Default, None)
        let _ = World.beginScreen<EnclosureDispatcher> Simulants.Enclosure.Name (game.GetGameState world = Enclosure) behavior [] world
        World.beginGroup Simulants.SceneGroup [] world
        if World.doButton Simulants.BackEntity [] world && world.Unaccompanied then World.exit world
        World.endGroup world
        World.endScreen world
        
        World.cam
        // Camera control
        if World.isKeyboardKeyDown KeyboardKey.Left world then
            World.setEye2dCenter (World.getEye2dCenter world - v2 1f 0f) world
        if World.isKeyboardKeyDown KeyboardKey.Right world then
            World.setEye2dCenter (World.getEye2dCenter world + v2 1f 0f) world
        if World.isKeyboardKeyDown KeyboardKey.Up world then
            World.setEye2dCenter (World.getEye2dCenter world + v2 0f 1f) world
        if World.isKeyboardKeyDown KeyboardKey.Down world then
            World.setEye2dCenter (World.getEye2dCenter world - v2 0f 1f) world
        if World.isKeyboardKeyDown KeyboardKey.PageUp world then
            World.setEye2dSize (World.getEye2dSize world * 1.01f) world
        if World.isKeyboardKeyDown KeyboardKey.PageDown world then
            World.setEye2dSize (World.getEye2dSize world * 0.99f) world
        if World.isKeyboardKeyDown KeyboardKey.Home world then
            World.setEye2dCenter (v2 60f 0f) world
            World.setEye2dSize Constants.Render.DisplayVirtualResolution.V2 world

        // handle Alt+F4 when not in editor
        if  World.isKeyboardAltDown world &&
            World.isKeyboardKeyDown KeyboardKey.F4 world &&
            world.Unaccompanied then
            World.exit world