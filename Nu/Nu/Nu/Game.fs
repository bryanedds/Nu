namespace Nu
open System
open FSharpx
open FSharpx.Lens.Operators
open Prime
open Nu
open Nu.NuCore
open Nu.Sim
open Nu.Screen

[<AutoOpen>]
module GameModule =

    type Game with
        member this.Register (world : World) : World = this?Register world

module Game =

    let _ = () // nothing in this module