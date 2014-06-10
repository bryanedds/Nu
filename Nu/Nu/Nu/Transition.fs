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

    let makeDefault defaultDispatcherName transitionType =
        { Id = NuCore.getId ()
          Lifetime = 0
          Ticks = 0
          Type = transitionType
          OptDissolveSprite = None
          FacetNamesNs = []
          Xtension = { XFields = Map.empty; OptXDispatcherName = Some defaultDispatcherName; CanDefault = true; Sealed = false }}