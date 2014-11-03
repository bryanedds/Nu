namespace Nu
open System
open Prime
open Nu
open Nu.Constants

[<AutoOpen>]
module GameModule =

    type Game with

        static member register (game : Game) (world : World) : Game * World =
            game.DispatcherNp.Register (game, world)

        static member setOptSelectedScreenAddress optSelectedScreenAddress game =
            { game with OptSelectedScreenAddress = optSelectedScreenAddress }

        static member make dispatcher optName =
            let id = Core.makeId ()
            let game =
                { Id = id
                  Name = match optName with None -> tcstring id | Some name -> name
                  OptSelectedScreenAddress = None
                  DispatcherNp = dispatcher
                  Xtension = { XFields = Map.empty; CanDefault = false; Sealed = true }
                  CreationTimeNp = DateTime.UtcNow }
            Reflection.attachFields dispatcher game
            game

[<AutoOpen>]
module WorldGameModule =

    type World with

        static member getOptSelectedScreenAddress world = world.Game.OptSelectedScreenAddress
        static member setOptSelectedScreenAddress optAddress world = { world with Game = Game.setOptSelectedScreenAddress optAddress world.Game }
        static member getSelectedScreenAddress world = Option.get <| World.getOptSelectedScreenAddress world
        static member setSelectedScreenAddress address world = World.setOptSelectedScreenAddress (Some address) world
        
        static member getOptSelectedScreen world =
            let optSelectedScreenAddress = World.getOptSelectedScreenAddress world
            match optSelectedScreenAddress with
            | Some selectedScreenAddress -> World.getOptScreen selectedScreenAddress world
            | None -> None

        static member setOptSelectedScreen optScreen world =
            let optSelectedScreenAddress = World.getOptSelectedScreenAddress world
            match optSelectedScreenAddress with
            | Some selectedScreenAddress -> World.setScreen selectedScreenAddress (Option.get optScreen) world
            | None -> failwith "Cannot set a non-existent screen."

        static member getSelectedScreen world = Option.get <| World.getOptSelectedScreen world
        static member setSelectedScreen screen world = World.setOptSelectedScreen (Some screen) world

        static member isAddressSelected address world =
            let optScreenAddress = World.getOptSelectedScreenAddress world
            match (address.AddrList, Option.map (fun address -> address.AddrList) optScreenAddress) with
            | ([], _) -> true
            | (_, None) -> false
            | (_, Some []) -> false
            | (addressHead :: _, Some (screenAddressHead :: _)) -> addressHead = screenAddressHead