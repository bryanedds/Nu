namespace BlazeVector
open System
open Prime
open Nu

// this module specifies new events types for BlazeVector.
[<RequireQualifiedAccess>]
module Events =

    // this event is called when the player or an enemy dies
    let DieEvent = stoa<unit> "Die/Event"