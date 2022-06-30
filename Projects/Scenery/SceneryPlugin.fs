namespace Scenery
open Nu
open Scenery

// this is a plugin for the Nu game engine that directs the execution of your application and editor
type SceneryPlugin () =
    inherit NuPlugin ()
    
    // this specifies the game dispatcher to use in your application
    override this.StandAloneConfig =
        typeof<SceneryDispatcher>