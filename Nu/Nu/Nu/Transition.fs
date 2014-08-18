namespace Nu
open System
open Prime
open Nu
open Nu.NuConstants

[<AutoOpen>]
module TransitionModule =

    type Transition with
    
        static member makeDefault transitionType =
            { TransitionLifetime = 0L
              TransitionTicks = 0L
              TransitionType = transitionType
              OptDissolveImage = None }