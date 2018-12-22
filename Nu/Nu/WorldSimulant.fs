// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2018.

namespace Nu
open System
open Prime
open Nu

[<AutoOpen; ModuleBinding>]
module WorldSimulantModule =

    type World with

        static member internal addSimulantScriptUnsubscription =
            WorldModule.addSimulantScriptUnsubscription

        static member internal unsubscribeSimulantScripts =
            WorldModule.unsubscribeSimulantScripts

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

        static member trySetSimulantProperty name alwaysPublish nonPersistent property (simulant : Simulant) world =
            match simulant with
            | :? Game -> World.trySetGameProperty name property world
            | :? Screen as screen -> World.trySetScreenProperty name property screen world
            | :? Layer as layer -> World.trySetLayerProperty name property layer world
            | :? Entity as entity -> World.trySetEntityProperty name alwaysPublish nonPersistent property entity world
            | _ -> (false, world)

        static member setSimulantProperty name alwaysPublish nonPersistent property (simulant : Simulant) world =
            match simulant with
            | :? Game -> World.setGameProperty name property world
            | :? Screen as screen -> World.setScreenProperty name property screen world
            | :? Layer as layer -> World.setLayerProperty name property layer world
            | :? Entity as entity -> World.setEntityProperty name alwaysPublish nonPersistent property entity world
            | _ -> failwithumf ()

        static member getSimulantDispatcher (simulant : Simulant) (world : World) =
            match simulant with
            | :? Game -> Simulants.Game.GetDispatcher world :> Dispatcher
            | :? Screen as screen -> screen.GetDispatcher world :> Dispatcher
            | :? Layer as layer -> layer.GetDispatcher world :> Dispatcher
            | :? Entity as entity -> entity.GetDispatcher world :> Dispatcher
            | _ -> failwithumf ()

        static member internal tryGetSimulantScriptFrame (simulant : Simulant) world =
            match simulant with
            | :? Game -> Some (World.getGameScriptFrame world)
            | :? Screen as screen -> Some (World.getScreenScriptFrame screen world)
            | :? Layer as layer -> Some (World.getLayerScriptFrame layer world)
            | :? Entity as entity ->
                match World.tryGetEntityProperty Property? ScriptFrame entity world with
                | Some scriptFrameProperty -> Some (scriptFrameProperty.PropertyValue :?> Scripting.DeclarationFrame)
                | None -> None
            | _ -> failwithumf ()

        static member tryDeriveSimulant address =
            match Address.getNames address with
            | [] -> Some (Game Address.empty :> Simulant)
            | [_] -> Some (Screen (Address.changeType<obj, Screen> address) :> Simulant)
            | [_; _] -> Some (Layer (Address.changeType<obj, Layer> address) :> Simulant)
            | [_; _; _] -> Some (Entity (Address.changeType<obj, Entity> address) :> Simulant)
            | _ -> None
        
        static member deriveSimulant address =
            match World.tryDeriveSimulant address with
            | Some simulant -> simulant
            | None -> failwithf "Could not derive simulant from address '%s'." (scstring address)

        [<FunctionBinding>]
        static member getSimulantSelected (simulant : Simulant) world =
            match simulant with
            | :? Game -> true
            | :? Screen as screen -> screen.GetSelected world
            | :? Layer as layer -> layer.GetSelected world
            | :? Entity as entity -> entity.GetSelected world
            | _ -> failwithumf ()

        [<FunctionBinding>]
        static member tryGetSimulantParent (simulant : Simulant) world =
            ignore (world : World)
            match simulant with
            | :? Game -> None
            | :? Screen -> Some (Simulants.Game :> Simulant)
            | :? Layer as layer -> Some (ltos layer :> Simulant)
            | :? Entity as entity -> Some (etol entity :> Simulant)
            | _ -> failwithumf ()

        [<FunctionBinding>]
        static member getSimulantChildren (simulant : Simulant) world =
            match simulant with
            | :? Game -> enumerable<Simulant> (World.getScreens world)
            | :? Screen as screen -> enumerable<Simulant> (World.getLayers screen world)
            | :? Layer as layer -> enumerable<Simulant> (World.getEntities layer world)
            | :? Entity -> Seq.empty
            | _ -> failwithumf ()

        [<FunctionBinding>]
        static member getSimulantExists (simulant : Simulant) (world : World) =
            (world :> EventWorld<Game, World>).ParticipantExists simulant