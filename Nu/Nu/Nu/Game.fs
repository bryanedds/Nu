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

    // WISDOM:
    //
    // A simulation that would put physics on another thread should likely do so in a different app
    // domain with communication via .NET remoting to make 100% sure that no sharing is happening.
    //
    // NOTE: for simulation types, value semantics are preferred over open semantics as it eases
    // serialization and other forms of automation. However, perhaps there is a way to get both...

    let gameId =
        { Get = fun (game : Game) -> game.Id
          Set = fun value game -> { game with Id = value }}

    let gameXtension =
        { Get = fun (game : Game) -> game.Xtension
          Set = fun value game -> { game with Xtension = value }}

    let gameXField fieldName =
        { Get = fun (game : Game) -> (?) game fieldName
          Set = fun value game -> (?<-) game fieldName value }