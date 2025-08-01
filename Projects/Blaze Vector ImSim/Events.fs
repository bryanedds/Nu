namespace BlazeVector
open System
open Nu

// this module specifies user-defined event types for Blaze Vector.
[<RequireQualifiedAccess>]
module Events =

    // this event is called when an enemy dies
    let DeathEvent = stoa<Entity> "Death/Event"