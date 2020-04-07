namespace Nelmish
open Nu
open Nelmish

// this is a plugin for the Nu game engine that directs the execution of your application and editor
type NelmishPlugin () =
    inherit NuPlugin ()

    // this specifies the game dispatcher to use in your application
    override this.GetStandAloneGameDispatcher () =
        typeof<NelmishDispatcher>

    // this specifies the game dispatcher to use in the editor
    override this.GetEditorGameDispatcher () =
        typeof<NelmishDispatcher>

    // this specifies no screen dispatcher is created by the editor since one is already created by NelmishDispatcher
    override this.GetEditorScreenDispatcherOpt () =
        None