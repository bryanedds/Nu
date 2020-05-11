namespace Elmario
open Nu
open Elmario

// this is a plugin for the Nu game engine that directs the execution of your application and editor
type ElmarioPlugin () =
    inherit NuPlugin ()
    
    // this specifies the game dispatcher to use in your application
    override this.GetGameDispatcher () =
        typeof<ElmarioDispatcher>