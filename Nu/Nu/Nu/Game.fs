namespace Nu
open System
open FSharpx
open FSharpx.Lens.Operators
open Prime
open Nu
open Nu.NuConstants

[<AutoOpen>]
module GameModule =

    // WISDOM:
    //
    // A simulation that would put physics on another thread should likely do so in a different app
    // domain with communication via .NET remoting to make 100% sure that no sharing is happening.
    //
    // NOTE: for simulation types, value semantics are preferred over open semantics as it eases
    // serialization and other forms of automation. However, perhaps there is a way to get both...

    type Game with

        member this.Register (world : World) : World = this?Register world

    type GameDispatcher () =
        
        abstract member Register : World -> World
        default dispatcher.Register world = world

[<AutoOpen>]
module WorldGameModule =

    type World with

        static member private gameOptSelectedScreenAddress =
            { Get = fun game -> game.OptSelectedScreenAddress
              Set = fun optSelectedScreenAddress game -> { game with OptSelectedScreenAddress = optSelectedScreenAddress }}

        static member private worldGame =
            { Get = fun world -> world.Game
              Set = fun game world -> { world with Game = game }}

        static member private worldOptSelectedScreenAddress =
            World.worldGame >>| World.gameOptSelectedScreenAddress

        static member private worldOptSelectedScreen =
            { Get = fun world ->
                let optSelectedScreenAddress = World.getOptSelectedScreenAddress world
                match optSelectedScreenAddress with
                | None -> None
                | Some selectedScreenAddress -> World.getOptScreen selectedScreenAddress world
              Set = fun optScreen world ->
                let optSelectedScreenAddress = World.getOptSelectedScreenAddress world
                match optSelectedScreenAddress with
                | None -> failwith "Cannot set a non-existent screen."
                | Some selectedScreenAddress -> World.setScreen selectedScreenAddress optScreen.Value world }

        static member private worldSelectedScreenAddress =
            { Get = fun world -> Option.get <| get world World.worldOptSelectedScreenAddress
              Set = fun address world -> set (Some address) world World.worldOptSelectedScreenAddress }

        static member private worldSelectedScreen =
            { Get = fun world -> Option.get <| get world World.worldOptSelectedScreen
              Set = fun screen world -> set (Some screen) world World.worldOptSelectedScreen }

        static member getSelectedScreenAddress world = get world World.worldSelectedScreenAddress
        static member setSelectedScreenAddress address world = set address world World.worldSelectedScreenAddress
        static member getOptSelectedScreenAddress world = get world World.worldOptSelectedScreenAddress
        static member setOptSelectedScreenAddress optAddress world = set optAddress world World.worldOptSelectedScreenAddress

        static member getSelectedScreen world = get world World.worldSelectedScreen
        static member setSelectedScreen screen world = set screen world World.worldSelectedScreen
        static member getOptSelectedScreen world = get world World.worldOptSelectedScreen
        static member setOptSelectedScreen optScreen world = set optScreen world World.worldOptSelectedScreen

        static member isAddressSelected (address : Address) world =
            let optScreenAddress = World.getOptSelectedScreenAddress world
            match (address, optScreenAddress) with
            | ([], _) -> true
            | (_, None) -> false
            | (_, Some []) -> false
            | (addressHead :: _, Some (screenAddressHead :: _)) -> addressHead = screenAddressHead