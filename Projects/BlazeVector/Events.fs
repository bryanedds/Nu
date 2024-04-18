namespace BlazeVector
open System
open Prime
open Nu

// this module specifies new events types for BlazeVector.
[<RequireQualifiedAccess>]
module Events =

    // this event is called when the player or an enemy dies
    let DieEvent = stoa<Entity> "Die/Event"

    // event raised by Gameplay screen that lets the game know its time to go back to the title screen
    let QuitEvent = stoa<unit> "Quit/Event"