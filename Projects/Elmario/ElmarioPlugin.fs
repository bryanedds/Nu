namespace Elmario
open Nu
open Elmario

// this is a plugin for the Nu game engine that directs the execution of your application and editor
type ElmarioPlugin () =
    inherit NuPlugin ()
    
    // this specifies the game dispatcher to use in your application
    override this.GetStandAloneGameDispatcher () =
        typeof<ElmarioDispatcher>
        
    // this specifies the game dispatcher to use in the editor
    override this.GetEditorGameDispatcher () =
        typeof<ElmarioDispatcher>
            
    // this specifies no screen dispatcher is to be created by the editor
    override this.GetEditorScreenDispatcherOpt () =
        None