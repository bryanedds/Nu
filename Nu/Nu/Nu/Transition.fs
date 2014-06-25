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

[<RequireQualifiedAccess>]
module Transition =

    let makeDefault dispatcherName transitionType =
        { Id = NuCore.getId ()
          TransitionLifetime = 0L
          TransitionTicks = 0L
          TransitionType = transitionType
          OptDissolveSprite = None
          Xtension = { XFields = Map.empty; OptXDispatcherName = Some dispatcherName; CanDefault = true; Sealed = false }}