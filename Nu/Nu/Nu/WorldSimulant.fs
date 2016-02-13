// Nu Game Engine.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Nu
open System
open Prime
open Nu

[<AutoOpen>]
module WorldSimulantModule =

    type World with

        /// Query that the world contains a simulant.
        static member containsSimulant<'a when 'a :> Simulant> (simulant : 'a) world =
            (world :> World Eventable).ContainsParticipant simulant

        /// Query that a simulant is the either currently selected screen or contained by it.
        static member isSimulantSelected<'s when 's :> Simulant> (simulant : 's) world =
            let optScreen = World.getOptSelectedScreen world
            let optScreenNames = Option.map (fun (screen : Screen) -> Address.getNames screen.ScreenAddress) optScreen
            let simulantNames = Address.getNames simulant.SimulantAddress
            match (simulantNames, optScreenNames) with
            | ([], _) -> true
            | (_, None) -> false
            | (_, Some []) -> false
            | (addressHead :: _, Some (screenAddressHead :: _)) -> addressHead = screenAddressHead