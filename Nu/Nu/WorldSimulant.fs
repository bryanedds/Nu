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

        static member internal tryGetProperty name (simulant : Simulant) world =
            match simulant with
            | :? Game -> World.tryGetGameProperty name world
            | :? Screen as screen -> World.tryGetScreenProperty name screen world
            | :? Layer as layer -> World.tryGetLayerProperty name layer world
            | :? Entity as entity -> World.tryGetEntityProperty name entity world
            | _ -> None

        static member internal getProperty name (simulant : Simulant) world =
            match simulant with
            | :? Game -> World.getGameProperty name world
            | :? Screen as screen -> World.getScreenProperty name screen world
            | :? Layer as layer -> World.getLayerProperty name layer world
            | :? Entity as entity -> World.getEntityProperty name entity world
            | _ -> failwithumf ()

        static member internal trySetProperty name alwaysPublish nonPersistent property (simulant : Simulant) world =
            match simulant with
            | :? Game -> World.trySetGameProperty name property world
            | :? Screen as screen -> World.trySetScreenProperty name property screen world
            | :? Layer as layer -> World.trySetLayerProperty name property layer world
            | :? Entity as entity -> World.trySetEntityProperty name alwaysPublish nonPersistent property entity world
            | _ -> (false, world)

        static member internal setProperty name alwaysPublish nonPersistent property (simulant : Simulant) world =
            match simulant with
            | :? Game -> World.setGameProperty name property world
            | :? Screen as screen -> World.setScreenProperty name property screen world
            | :? Layer as layer -> World.setLayerProperty name property layer world
            | :? Entity as entity -> World.setEntityProperty name alwaysPublish nonPersistent property entity world
            | _ -> failwithumf ()

        static member internal attachProperty name alwaysPublish nonPersistent property (simulant : Simulant) world =
            match simulant with
            | :? Game -> World.attachGameProperty name property world
            | :? Screen as screen -> World.attachScreenProperty name property screen world
            | :? Layer as layer -> World.attachLayerProperty name property layer world
            | :? Entity as entity -> World.attachEntityProperty name alwaysPublish nonPersistent property entity world
            | _ -> failwithumf ()

        static member internal detachProperty name (simulant : Simulant) world =
            match simulant with
            | :? Game -> World.detachGameProperty name world
            | :? Screen as screen -> World.detachScreenProperty name screen world
            | :? Layer as layer -> World.detachLayerProperty name layer world
            | :? Entity as entity -> World.detachEntityProperty name entity world
            | _ -> failwithumf ()

        /// Get the given simulant's dispatcher.
        static member getDispatcher (simulant : Simulant) (world : World) =
            match simulant with
            | :? Game -> Default.Game.GetDispatcher world :> Dispatcher
            | :? Screen as screen -> screen.GetDispatcher world :> Dispatcher
            | :? Layer as layer -> layer.GetDispatcher world :> Dispatcher
            | :? Entity as entity -> entity.GetDispatcher world :> Dispatcher
            | _ -> failwithumf ()

        static member internal unregister (simulant : Simulant) (world : World) =
            match simulant with
            | :? Game -> World.unregisterGame world
            | :? Screen as screen -> World.unregisterScreen screen world
            | :? Layer as layer -> World.unregisterLayer layer world
            | :? Entity as entity -> World.unregisterEntity entity world
            | _ -> failwithumf ()

        static member internal register (simulant : Simulant) (world : World) =
            match simulant with
            | :? Game -> World.registerGame world
            | :? Screen as screen -> World.registerScreen screen world
            | :? Layer as layer -> World.registerLayer layer world
            | :? Entity as entity -> World.registerEntity entity world
            | _ -> failwithumf ()

        /// Expand the given simulant content.
        [<FunctionBinding>]
        static member expandContent setScreenSplash guidOpt (content : SimulantContent) origin (parent : Simulant) (world : World) =
            match (content, parent) with
            | ((:? ScreenContent as screenContent), (:? Game as game)) -> World.expandScreenContent setScreenSplash screenContent origin game world |> snd
            | ((:? LayerContent as layerContent), (:? Screen as screen)) -> World.expandLayerContent guidOpt layerContent origin screen world
            | ((:? EntityContent as entityContent), (:? Layer as layer)) -> World.expandEntityContent guidOpt entityContent origin layer world
            | _ -> failwithumf ()

        /// Destroy the given simulant.
        [<FunctionBinding>]
        static member destroy (simulant : Simulant) (world : World) =
            match simulant with
            | :? Screen as screen -> World.destroyScreen screen world
            | :? Layer as layer -> World.destroyLayer layer world
            | :? Entity as entity -> World.destroyEntity entity world
            | _ -> failwithumf ()

        /// Get the script frame in which the given simulant's script code will run.
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

        /// Determine if the given simulant is currently selected.
        [<FunctionBinding>]
        static member getSelected (simulant : Simulant) world =
            match simulant with
            | :? Game -> true
            | :? Screen as screen -> screen.GetSelected world
            | :? Layer as layer -> layer.GetSelected world
            | :? Entity as entity -> entity.GetSelected world
            | _ -> failwithumf ()

        /// Attempt to get the parent of the given simulant.
        [<FunctionBinding>]
        static member tryGetParent (simulant : Simulant) world =
            ignore (world : World)
            match simulant with
            | :? Game -> None
            | :? Screen -> Some (Default.Game :> Simulant)
            | :? Layer as layer -> Some (ltos layer :> Simulant)
            | :? Entity as entity -> Some (etol entity :> Simulant)
            | _ -> failwithumf ()

        /// Get the parent of the given simulant.
        [<FunctionBinding>]
        static member getParent (simulant : Simulant) world =
            ignore (world : World)
            match simulant with
            | :? Game -> failwithumf ()
            | :? Screen -> Default.Game :> Simulant
            | :? Layer as layer -> ltos layer :> Simulant
            | :? Entity as entity -> etol entity :> Simulant
            | _ -> failwithumf ()
        
        /// Attempt to get the parent of the parent of the given simulant.
        [<FunctionBinding>]
        static member tryGetGrandparent (simulant : Simulant) world =
            match World.tryGetParent simulant world with
            | Some parent -> World.tryGetParent parent world
            | None -> None

        /// Get the parent of the parent of the given simulant.
        [<FunctionBinding>]
        static member getGrandparent (simulant : Simulant) world =
            let parent = World.getParent simulant world
            World.getParent parent world

        /// Get the existing child simulants of the given simulant.
        [<FunctionBinding>]
        static member getChildren (simulant : Simulant) world =
            match simulant with
            | :? Game -> enumerable<Simulant> (World.getScreens world)
            | :? Screen as screen -> enumerable<Simulant> (World.getLayers screen world)
            | :? Layer as layer -> enumerable<Simulant> (World.getEntities layer world)
            | :? Entity -> Seq.empty
            | _ -> failwithumf ()

        /// Check that a simulant exists in the world.
        [<FunctionBinding>]
        static member getExists (simulant : Simulant) (world : World) =
            (world :> EventSystem<World>).SimulantExists simulant

        /// Attempt to convert an address to a concrete simulant reference.
        static member tryDerive address =
            match Address.getNames address with
            | [||] -> Some (Default.Game :> Simulant)
            | [|_|] -> Some (Screen (Address.changeType<obj, Screen> address) :> Simulant)
            | [|_; _|] -> Some (Layer (Address.changeType<obj, Layer> address) :> Simulant)
            | [|_; _; _|] -> Some (Entity (Address.changeType<obj, Entity> address) :> Simulant)
            | _ -> None

        /// Convert an address to a concrete simulant reference.
        static member derive address =
            match World.tryDerive address with
            | Some simulant -> simulant
            | None -> failwithf "Could not derive simulant from address '%s'." (scstring address)

        /// Convert an event address to the concrete simulant that it targets.
        static member deriveFromEvent event =
            let eventAddressNames = Address.getNames event
            let eventTargetIndex = Array.findIndex (fun name -> name = "Event") eventAddressNames + 1
            if eventTargetIndex < Array.length eventAddressNames then
                let eventTarget = eventAddressNames |> Array.skip eventTargetIndex |> Address.makeFromArray
                World.derive eventTarget
            else failwithumf ()

        /// Constrain one property to equal the value of another, optionally breaking potential cycles.
        static member equate (left : Lens<'a, World>) (right : Lens<'a, World>) breaking world =
            WorldModule.equate5 left.Name left.This right breaking world

[<AutoOpen>]
module WorldSimulantOperators =

    /// Equate two properties, not breaking potential cycles.
    let (===) left right = World.equate left right false

    /// Equate two properties, breaking potential cycles.
    let (=/=) left right = World.equate left right true