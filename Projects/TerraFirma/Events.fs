namespace TerraFirma
open System
open Prime
open Nu

[<RequireQualifiedAccess>]
module Events =

    let AttackEvent = stoa<Entity> "Attack/Event"
    let DieEvent = stoa<unit> "Die/Event"