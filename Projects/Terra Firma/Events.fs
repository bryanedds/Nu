namespace TerraFirma
open System
open Nu

// this module specifies user-defined event types for Terra  Firma.
[<RequireQualifiedAccess>]
module Events =

    // this event is called when an attack is detected during gameplay.
    let AttackEvent = stoa<Entity> "Attack/Event"

    // this event is called when the player or an enemy dies during gameplay.
    let DeathEvent = stoa<Entity> "Death/Event"