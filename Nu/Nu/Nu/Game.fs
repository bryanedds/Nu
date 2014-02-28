namespace Nu
open System
open FSharpx
open FSharpx.Lens.Operators
open Prime
open Nu
open Nu.NuCore
open Nu.DomainModel
open Nu.Screen

[<AutoOpen>]
module GameModule =

    type GameDispatcher () =
        class
            abstract member Register : Game * World -> World
            default this.Register (_, world) = world
            end

module Game =

    // WISDOM:
    //
    // A simulation that would put physics on another thread should likely do so in a different app
    // domain with communication via .NET remoting to make 100% sure that no sharing is happening.
    //
    // NOTE: for simulation types, value semantics are preferred over open semantics as it eases
    // serialization and other forms of automation. However, perhaps there is a way to get both...

    let gameIdLens =
        { Get = fun (game : Game) -> game.Id
          Set = fun value game -> { game with Id = value }}

    let gameXtensionLens =
        { Get = fun (game : Game) -> game.Xtension
          Set = fun value game -> { game with Xtension = value }}

    let gameDynamicLens memberName =
        { Get = fun (game : Game) -> (?) game memberName
          Set = fun value game -> (?<-) game memberName value }

    let worldOptSelectedScreenAddressLens =
        { Get = fun world -> world.Game.OptSelectedScreenAddress
          Set = fun value world -> { world with Game = { world.Game with OptSelectedScreenAddress = value }}}

    let worldOptSelectedScreenLens =
        { Get = fun world ->
            let optSelectedScreenAddress = get world worldOptSelectedScreenAddressLens
            match optSelectedScreenAddress with
            | None -> None
            | Some selectedScreenAddress -> get world <| worldOptScreenLens selectedScreenAddress
          Set = fun screen world ->
            let optSelectedScreenAddress = get world worldOptSelectedScreenAddressLens
            match optSelectedScreenAddress with
            | None -> failwith "Cannot set a non-existent screen."
            | Some selectedScreenAddress -> set screen.Value world <| worldScreenLens selectedScreenAddress }