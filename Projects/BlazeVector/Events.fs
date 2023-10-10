namespace BlazeVector
open System
open Prime
open Nu

// this extend the available vocabulary for expressing events in Nu for BlazeVector's specific purposes.
[<RequireQualifiedAccess>]
module Events =

    // this event is called when the player or an enemy is dying
    let Dying = stoa<unit> "Dying/Event"