namespace Breakout
open System
open Nu

// this module specifies user-defined event types for Breakout.
[<RequireQualifiedAccess>]
module Events =

    // event raised by Gameplay screen that lets the game know its time to go back to the title screen
    let QuitEvent = stoa<unit> "Quit/Event"