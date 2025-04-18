namespace BlazeVector
open System
open Nu

// this module specifies user-defined event types for Blaze Vector.
[<RequireQualifiedAccess>]
module Events =

    // this event is called when the player or an enemy dies
    let DeathEvent = stoa<Entity> "Death/Event"

    // event raised by Gameplay screen that lets the game know its time to go back to the title screen
    let QuitEvent = stoa<unit> "Quit/Event"