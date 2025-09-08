namespace Physics2D
open System
open Nu
open Physics2D

// this is a plugin for the Nu game engine that directs the execution of your application and editor
type Physics2DPlugin () =
    inherit NuPlugin ()

    // this exposes different editing modes in the editor
    override this.EditModes =
        Enum.GetValues<GameState> ()
        |> Array.map (fun v -> string v, Game.SetGameState v)
        |> Map.ofArray