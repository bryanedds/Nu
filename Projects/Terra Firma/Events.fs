namespace TerraFirma
open System
open Nu
open TerraFirma

[<RequireQualifiedAccess>]
module Events =

    let AttackEvent = stoa<Entity> "Attack/Event"
    let DieEvent = stoa<Entity> "Die/Event"
    let QuitEvent = stoa<unit> "Quit/Event"