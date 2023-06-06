namespace BlazeVector
open System
open Nu

/// this extend the available vocabulary for expressing events in Nu for BlazeVector's specific purposes.
[<RequireQualifiedAccess>]
module Events =

    /// this event is called when an enemy dies such that the score can be increased.
    let Dying = stoa<unit> "Dying/Event"