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

        static member internal tryGetState (simulant : Simulant) world =
            match simulant with
            | :? Entity as entity -> World.getEntityState entity world :> SimulantState |> Some
            | :? Layer as layer -> World.getLayerState layer world :> SimulantState |> Some
            | :? Screen as screen -> World.getScreenState screen world :> SimulantState |> Some
            | :? Game -> World.getGameState world :> SimulantState |> Some
            | _ -> None

        static member internal getState (simulant : Simulant) world =
            match simulant with
            | :? Entity as entity -> World.getEntityState entity world :> SimulantState
            | :? Layer as layer -> World.getLayerState layer world :> SimulantState
            | :? Screen as screen -> World.getScreenState screen world :> SimulantState
            | :? Game -> World.getGameState world :> SimulantState
            | _ -> failwithumf ()

        static member tryGetProperty name (simulant : Simulant) world =
            match simulant with
            | :? Game -> World.tryGetGameProperty name world
            | :? Screen as screen -> World.tryGetScreenProperty name screen world
            | :? Layer as layer -> World.tryGetLayerProperty name layer world
            | :? Entity as entity -> World.tryGetEntityProperty name entity world
            | _ -> None

        static member getProperty name (simulant : Simulant) world =
            match simulant with
            | :? Game -> World.getGameProperty name world
            | :? Screen as screen -> World.getScreenProperty name screen world
            | :? Layer as layer -> World.getLayerProperty name layer world
            | :? Entity as entity -> World.getEntityProperty name entity world
            | _ -> failwithumf ()

        static member trySetProperty name alwaysPublish nonPersistent property (simulant : Simulant) world =
            match simulant with
            | :? Game -> World.trySetGameProperty name property world
            | :? Screen as screen -> World.trySetScreenProperty name property screen world
            | :? Layer as layer -> World.trySetLayerProperty name property layer world
            | :? Entity as entity -> World.trySetEntityProperty name alwaysPublish nonPersistent property entity world
            | _ -> (false, world)

        static member setProperty name alwaysPublish nonPersistent property (simulant : Simulant) world =
            match simulant with
            | :? Game -> World.setGameProperty name property world
            | :? Screen as screen -> World.setScreenProperty name property screen world
            | :? Layer as layer -> World.setLayerProperty name property layer world
            | :? Entity as entity -> World.setEntityProperty name alwaysPublish nonPersistent property entity world
            | _ -> failwithumf ()

        static member getDispatcher (simulant : Simulant) (world : World) =
            match simulant with
            | :? Game -> Simulants.Game.GetDispatcher world :> Dispatcher
            | :? Screen as screen -> screen.GetDispatcher world :> Dispatcher
            | :? Layer as layer -> layer.GetDispatcher world :> Dispatcher
            | :? Entity as entity -> entity.GetDispatcher world :> Dispatcher
            | _ -> failwithumf ()

        static member internal tryGetScriptFrame (simulant : Simulant) world =
            match simulant with
            | :? Game -> Some (World.getGameScriptFrame world)
            | :? Screen as screen -> Some (World.getScreenScriptFrame screen world)
            | :? Layer as layer -> Some (World.getLayerScriptFrame layer world)
            | :? Entity as entity ->
                match World.tryGetEntityProperty Property? ScriptFrame entity world with
                | Some scriptFrameProperty -> Some (scriptFrameProperty.PropertyValue :?> Scripting.DeclarationFrame)
                | None -> None
            | _ -> failwithumf ()

        [<FunctionBinding>]
        static member getSelected (simulant : Simulant) world =
            match simulant with
            | :? Game -> true
            | :? Screen as screen -> screen.GetSelected world
            | :? Layer as layer -> layer.GetSelected world
            | :? Entity as entity -> entity.GetSelected world
            | _ -> failwithumf ()

        [<FunctionBinding>]
        static member tryGetParent (simulant : Simulant) world =
            ignore (world : World)
            match simulant with
            | :? Game -> None
            | :? Screen -> Some (Simulants.Game :> Simulant)
            | :? Layer as layer -> Some (ltos layer :> Simulant)
            | :? Entity as entity -> Some (etol entity :> Simulant)
            | _ -> failwithumf ()

        [<FunctionBinding>]
        static member getParent (simulant : Simulant) world =
            ignore (world : World)
            match simulant with
            | :? Game -> failwithumf ()
            | :? Screen -> Simulants.Game :> Simulant
            | :? Layer as layer -> ltos layer :> Simulant
            | :? Entity as entity -> etol entity :> Simulant
            | _ -> failwithumf ()

        [<FunctionBinding>]
        static member tryGetGrandparent (simulant : Simulant) world =
            match World.tryGetParent simulant world with
            | Some parent -> World.tryGetParent parent world
            | None -> None

        [<FunctionBinding>]
        static member getGrandparent (simulant : Simulant) world =
            let parent = World.getParent simulant world
            World.getParent parent world

        [<FunctionBinding>]
        static member getChildren (simulant : Simulant) world =
            match simulant with
            | :? Game -> enumerable<Simulant> (World.getScreens world)
            | :? Screen as screen -> enumerable<Simulant> (World.getLayers screen world)
            | :? Layer as layer -> enumerable<Simulant> (World.getEntities layer world)
            | :? Entity -> Seq.empty
            | _ -> failwithumf ()

        [<FunctionBinding>]
        static member getExists (simulant : Simulant) (world : World) =
            (world :> EventSystem<World>).ParticipantExists simulant

        static member tryDerive address =
            match Address.getNames address with
            | [] -> Some (Game () :> Simulant)
            | [_] -> Some (Screen (Address.changeType<obj, Screen> address) :> Simulant)
            | [_; _] -> Some (Layer (Address.changeType<obj, Layer> address) :> Simulant)
            | [_; _; _] -> Some (Entity (Address.changeType<obj, Entity> address) :> Simulant)
            | _ -> None

        static member derive address =
            match World.tryDerive address with
            | Some simulant -> simulant
            | None -> failwithf "Could not derive simulant from address '%s'." (scstring address)