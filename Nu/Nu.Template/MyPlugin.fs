namespace MyGame
open Nu

// this is a plugin for the Nu game engine that directs the execution of your application and editor
type MyPlugin () =
    inherit NuPlugin ()

    // this specifies the game dispatcher to use in your application
    override this.GetStandAloneGameDispatcher () =
        typeof<MyGameDispatcher>

    // this specifies the game dispatcher to use in the editor
    override this.GetEditorGameDispatcher () =
        typeof<GameDispatcher>

    // this specifies the screen dispatcher to optionally use in the editor
    override this.GetEditorScreenDispatcherOpt () =
        Some typeof<MyGameplayDispatcher>