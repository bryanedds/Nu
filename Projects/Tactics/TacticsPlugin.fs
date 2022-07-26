namespace Tactics
open Nu
open Tactics

// this is a plugin for the Nu game engine that directs the execution of your application and editor
type TacticsPlugin () =
    inherit NuPlugin ()
    
    // this specifies the game dispatcher to use in your application
    override this.StandAloneConfig =
        typeof<TacticsDispatcher>