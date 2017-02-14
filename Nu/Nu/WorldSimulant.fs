// Nu Game Engine.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Nu
open System
open Prime
open Nu

[<AutoOpen>]
module WorldSimulant =

    type World with

        static member tryGetSimulantProperty name (simulant : Simulant) world =
            match simulant with
            | :? Game -> World.tryGetGameProperty name world
            | :? Screen as screen -> World.tryGetScreenProperty name screen world
            | :? Layer as layer -> World.tryGetLayerProperty name layer world
            | :? Entity as entity -> World.tryGetEntityProperty name entity world
            | _ -> None

        static member getSimulantProperty name (simulant : Simulant) world =
            match simulant with
            | :? Game -> World.getGameProperty name world
            | :? Screen as screen -> World.getScreenProperty name screen world
            | :? Layer as layer -> World.getLayerProperty name layer world
            | :? Entity as entity -> World.getEntityProperty name entity world
            | _ -> failwithumf ()

        static member trySetSimulantProperty name property (simulant : Simulant) world =
            match simulant with
            | :? Game -> World.trySetGameProperty name property world
            | :? Screen as screen -> World.trySetScreenProperty name property screen world
            | :? Layer as layer -> World.trySetLayerProperty name property layer world
            | :? Entity as entity -> World.trySetEntityProperty name property entity world
            | _ -> (false, world)

        static member setSimulantProperty name property (simulant : Simulant) world =
            match simulant with
            | :? Game -> World.setGameProperty name property world
            | :? Screen as screen -> World.setScreenProperty name property screen world
            | :? Layer as layer -> World.setLayerProperty name property layer world
            | :? Entity as entity -> World.setEntityProperty name property entity world
            | _ -> failwithumf ()

        static member internal tryGetSimulantScriptFrame (simulant : Simulant) world =
            match simulant with
            | :? Game -> Some (World.getGameScriptFrameNp world)
            | :? Screen as screen -> Some (World.getScreenScriptFrameNp screen world)
            | :? Layer as layer -> Some (World.getLayerScriptFrameNp layer world)
            | :? Entity as entity ->
                match World.tryGetEntityProperty Property? ScriptFrameNp entity world with
                | Some scriptFrameProperty -> Some (fst scriptFrameProperty :?> Scripting.DeclarationFrame)
                | None -> None
            | _ -> failwithumf ()

        static member tryGetSimulantParent (simulant : Simulant) (_ : World) =
            match simulant with
            | :? Game -> None
            | :? Screen -> Some (Simulants.Game :> Simulant)
            | :? Layer as layer -> Some (ltos layer :> Simulant)
            | :? Entity as entity -> Some (etol entity :> Simulant)
            | _ -> failwithumf ()

        static member getSimulantChildren (simulant : Simulant) world =
            match simulant with
            | :? Game -> enumerable<Simulant> (World.getScreens world)
            | :? Screen as screen -> enumerable<Simulant> (World.getLayers screen world)
            | :? Layer as layer -> enumerable<Simulant> (World.getEntities layer world)
            | :? Entity -> Seq.empty
            | _ -> failwithumf ()

        static member tryDeriveSimulant address =
            match Address.getNames address with
            | [] -> Some (Game Address.empty :> Simulant)
            | [_] -> Some (Screen (Address.changeType<Simulant, Screen> address) :> Simulant)
            | [_; _] -> Some (Layer (Address.changeType<Simulant, Layer> address) :> Simulant)
            | [_; _; _] -> Some (Entity (Address.changeType<Simulant, Entity> address) :> Simulant)
            | _ -> None
        
        static member deriveSimulant address =
            match World.tryDeriveSimulant address with
            | Some simulant -> simulant
            | None -> failwithf "Could not derive simulant from address '%s'." (scstring address)

        static member simulantExists (simulant : Simulant) (world : World) =
            (world :> EventWorld<Game, World>).ParticipantExists simulant