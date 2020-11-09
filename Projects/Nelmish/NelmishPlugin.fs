namespace Nelmish
open Nu
open Nelmish

// this is a plugin for the Nu game engine that directs the execution of your application and editor
type NelmishPlugin () =
    inherit NuPlugin ()

    // this specifies the game dispatcher to use in your application
    override this.GetGameDispatcher () =
        typeof<NelmishDispatcher>