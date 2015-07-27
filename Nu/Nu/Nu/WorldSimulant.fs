// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2015.

namespace Nu
open System
open Prime
open Nu

[<AutoOpen>]
module WorldSimulantModule =

    type World with

        /// Query that a simulant is the either currently selected screen or contained by it.
        static member isSimulantSelected<'s when 's :> Simulant> (simulant : 's) world =
            let optScreen = World.getOptSelectedScreen world
            let optScreenNameKeys = Option.map (fun (screen : Screen) -> Address.getNameKeys screen.ScreenAddress) optScreen
            let simulantNameKeys = Address.getNameKeys simulant.SimulantAddress
            match (simulantNameKeys, optScreenNameKeys) with
            | ([], _) -> true
            | (_, None) -> false
            | (_, Some []) -> false
            | (addressHead :: _, Some (screenAddressHead :: _)) -> addressHead = screenAddressHead

        /// Query that the world contains a simulant.
        static member containsSimulant<'a when 'a :> Simulant> (simulant : 'a) world =
            match simulant :> Simulant with
            | :? Game -> true
            | :? Screen as screen -> World.containsScreen screen world
            | :? Group as group -> World.containsGroup group world
            | :? Entity as entity -> World.containsEntity entity world
            | _ -> failwithumf ()