namespace BlazeVector
open Nu
open BlazeVector

// this is a plugin for the Nu game engine that directs the execution of your application and editor
type BlazePlugin () =
    inherit NuPlugin ()

    // specify the game dispatcher to use at run-time
    override this.GetGameDispatcher () =
        typeof<BlazeDispatcher>

    // specify the screen dispatcher to use in the editor
    override this.GetEditorScreenDispatcher () =
        typeof<GameplayDispatcher>