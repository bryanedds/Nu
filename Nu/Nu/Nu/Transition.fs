namespace Nu
open System
open FSharpx
open FSharpx.Lens.Operators
open Prime
open Nu
open Nu.NuConstants

[<AutoOpen>]
module TransitionModule =

    type Transition with
        end

    type TransitionDispatcher () =
        class end

[<RequireQualifiedAccess>]
module Transition =

    let makeDefault transitionType =
        { TransitionLifetime = 0L
          TransitionTicks = 0L
          TransitionType = transitionType
          OptDissolveImage = None }
