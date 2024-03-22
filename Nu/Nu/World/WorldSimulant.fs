// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu
open System
open System.Reflection
open System.Runtime.CompilerServices
open Prime

[<AutoOpen>]
module WorldSimulantModule =

    type World with

        static member internal addSimulantScriptUnsubscription =
            WorldModule.addSimulantScriptUnsubscription

        static member internal unsubscribeSimulantScripts =
            WorldModule.unsubscribeSimulantScripts

        static member internal tryGetState (simulant : Simulant) world =
            match simulant with
            | :? Entity as entity -> World.getEntityState entity world :> SimulantState |> Some
            | :? Group as group -> World.getGroupState group world :> SimulantState |> Some
            | :? Screen as screen -> World.getScreenState screen world :> SimulantState |> Some
            | :? Game as game -> World.getGameState game world :> SimulantState |> Some
            | _ -> None

        static member internal getState (simulant : Simulant) world =
            match simulant with
            | :? Entity as entity -> World.getEntityState entity world :> SimulantState
            | :? Group as group -> World.getGroupState group world :> SimulantState
            | :? Screen as screen -> World.getScreenState screen world :> SimulantState
            | :? Game as game -> World.getGameState game world :> SimulantState
            | _ -> failwithumf ()

        static member internal tryGetProperty (name, simulant : Simulant, world, property : Property outref) =
            let namesLength = simulant.SimulantAddress.Names.Length
            if namesLength >= 4
            then World.tryGetEntityProperty (name, simulant :?> Entity, world, &property)
            else
                match namesLength with
                | 1 -> World.tryGetGameProperty (name, simulant :?> Game, world, &property)
                | 2 -> World.tryGetScreenProperty (name, simulant :?> Screen, world, &property)
                | 3 -> World.tryGetGroupProperty (name, simulant :?> Group, world, &property)
                | _ -> failwithumf ()

        static member internal getProperty name (simulant : Simulant) world =
            let namesLength = simulant.SimulantAddress.Names.Length
            if namesLength >= 4
            then World.getEntityProperty name (simulant :?> Entity) world
            else
                match namesLength with
                | 1 -> World.getGameProperty name (simulant :?> Game) world
                | 2 -> World.getScreenProperty name (simulant :?> Screen) world
                | 3 -> World.getGroupProperty name (simulant :?> Group) world
                | _ -> failwithumf ()

        static member internal trySetPropertyFast name property (simulant : Simulant) world =
            let namesLength = simulant.SimulantAddress.Names.Length
            if namesLength >= 4
            then World.trySetEntityPropertyFast name property (simulant :?> Entity) world
            else
                match namesLength with
                | 1 -> World.trySetGamePropertyFast name property (simulant :?> Game) world
                | 2 -> World.trySetScreenPropertyFast name property (simulant :?> Screen) world
                | 3 -> World.trySetGroupPropertyFast name property (simulant :?> Group) world
                | _ -> failwithumf ()

        static member internal trySetProperty name property (simulant : Simulant) world =
            let namesLength = simulant.SimulantAddress.Names.Length
            if namesLength >= 4
            then World.trySetEntityProperty name property (simulant :?> Entity) world
            else
                match namesLength with
                | 1 -> World.trySetGameProperty name property (simulant :?> Game) world
                | 2 -> World.trySetScreenProperty name property (simulant :?> Screen) world
                | 3 -> World.trySetGroupProperty name property (simulant :?> Group) world
                | _ -> failwithumf ()

        static member internal setProperty name property (simulant : Simulant) world =
            let namesLength = simulant.SimulantAddress.Names.Length
            if namesLength >= 4
            then World.setEntityProperty name property (simulant :?> Entity) world
            else
                match namesLength with
                | 1 -> World.setGameProperty name property (simulant :?> Game) world
                | 2 -> World.setScreenProperty name property (simulant :?> Screen) world
                | 3 -> World.setGroupProperty name property (simulant :?> Group) world
                | _ -> failwithumf ()

        static member internal attachProperty name property (simulant : Simulant) world =
            match simulant with
            | :? Entity as entity -> World.attachEntityProperty name property entity world
            | :? Group as group -> World.attachGroupProperty name property group world
            | :? Screen as screen -> World.attachScreenProperty name property screen world
            | :? Game as game -> World.attachGameProperty name property game world
            | _ -> failwithumf ()

        static member internal detachProperty name (simulant : Simulant) world =
            match simulant with
            | :? Entity as entity -> World.detachEntityProperty name entity world
            | :? Group as group -> World.detachGroupProperty name group world
            | :? Screen as screen -> World.detachScreenProperty name screen world
            | :? Game as game -> World.detachGameProperty name game world
            | _ -> failwithumf ()

        /// Get the given simulant's dispatcher.
        static member getDispatcher (simulant : Simulant) (world : World) =
            match simulant with
            | :? Entity as entity -> entity.GetDispatcher world :> Dispatcher
            | :? Group as group -> group.GetDispatcher world :> Dispatcher
            | :? Screen as screen -> screen.GetDispatcher world :> Dispatcher
            | :? Game as game -> game.GetDispatcher world :> Dispatcher
            | _ -> failwithumf ()

        static member internal unregister (simulant : Simulant) (world : World) =
            match simulant with
            | :? Entity as entity -> World.unregisterEntity entity world
            | :? Group as group -> World.unregisterGroup group world
            | :? Screen as screen -> World.unregisterScreen screen world
            | :? Game as game -> World.unregisterGame game world
            | _ -> failwithumf ()

        static member internal register (simulant : Simulant) (world : World) =
            match simulant with
            | :? Entity as entity -> World.registerEntity entity world
            | :? Group as group -> World.registerGroup group world
            | :? Screen as screen -> World.registerScreen screen world
            | :? Game as game -> World.registerGame game world
            | _ -> failwithumf ()

        static member internal trySynchronize initializing (simulant : Simulant) (world : World) =
            match simulant with
            | :? Entity as entity -> (World.getEntityDispatcher entity world).TrySynchronize (initializing, entity, world)
            | :? Group as group -> (World.getGroupDispatcher group world).TrySynchronize (initializing, group, world)
            | :? Screen as screen -> (World.getScreenDispatcher screen world).TrySynchronize (initializing, screen, world)
            | :? Game as game -> (World.getGameDispatcher game world).TrySynchronize (initializing, game, world)
            | _ -> failwithumf ()

        /// Destroy the given simulant.
        static member destroyImmediate (simulant : Simulant) (world : World) =
            match simulant with
            | :? Entity as entity -> World.destroyEntityImmediate entity world
            | :? Group as group -> World.destroyGroupImmediate group world
            | :? Screen as screen -> World.destroyScreenImmediate screen world
            | _ -> failwithumf ()

        /// Destroy the given simulant.
        static member destroy (simulant : Simulant) (world : World) =
            match simulant with
            | :? Entity as entity -> World.destroyEntity entity world
            | :? Group as group -> World.destroyGroup group world
            | :? Screen as screen -> World.destroyScreen screen world
            | _ -> failwithumf ()

        /// Edit a simulant with the given operation using the ImGui APIs.
        /// Intended only to be called by editors like Gaia.
        static member edit operation (simulant : Simulant) world =
            match simulant with
            | :? Entity as entity -> World.editEntity operation entity world
            | :? Group as group -> World.editGroup operation group world
            | :? Screen as screen -> World.editScreen operation screen world
            | :? Game as game -> World.editGame operation game world
            | _ -> failwithumf ()

        /// Attempt to truncate a model.
        static member tryTruncateModel<'model> (model : 'model) (simulant : Simulant) world =
            match simulant with
            | :? Entity as entity -> World.tryTruncateEntityModel<'model> model entity world
            | :? Group as group -> World.tryTruncateGroupModel<'model> model group world
            | :? Screen as screen -> World.tryTruncateScreenModel<'model> model screen world
            | :? Game as game -> World.tryTruncateGameModel<'model> model game world
            | _ -> failwithumf ()

        /// Attempt to untruncate a model.
        static member tryUntruncateModel<'model> (model : 'model) (simulant : Simulant) world =
            match simulant with
            | :? Entity as entity -> World.tryUntruncateEntityModel<'model> model entity world
            | :? Group as group -> World.tryUntruncateGroupModel<'model> model group world
            | :? Screen as screen -> World.tryUntruncateScreenModel<'model> model screen world
            | :? Game as game -> World.tryUntruncateGameModel<'model> model game world
            | _ -> failwithumf ()

        /// Attempt to get the parent of the given simulant.
        static member tryGetParent (simulant : Simulant) world =
            ignore (world : World)
            match simulant with
            | :? Entity as entity -> Some entity.Parent
            | :? Group as group -> Some (group.Screen :> Simulant)
            | :? Screen -> Some (Game.Handle :> Simulant)
            | :? Game -> None
            | _ -> failwithumf ()

        /// Get the parent of the given simulant.
        static member getParent (simulant : Simulant) world =
            ignore (world : World)
            match simulant with
            | :? Entity as entity -> entity.Parent
            | :? Group as group -> group.Screen :> Simulant
            | :? Screen -> Game.Handle :> Simulant
            | :? Game -> failwithumf ()
            | _ -> failwithumf ()

        /// Get the existing children of the given simulant.
        static member getChildren (simulant : Simulant) world =
            match simulant with
            | :? Entity as entity -> enumerable<Simulant> (World.getEntityChildren entity world)
            | :? Group as group -> enumerable<Simulant> (World.getEntitiesSovereign group world)
            | :? Screen as screen -> enumerable<Simulant> (World.getGroups screen world)
            | :? Game -> enumerable<Simulant> (World.getScreens world)
            | _ -> failwithumf ()

        /// Check that a simulant exists in the world.
        static member getExists (simulant : Simulant) (world : World) =
            let namesLength = simulant.SimulantAddress |> Address.getNames |> Array.length
            if namesLength >= 4 then
                let entity = simulant :?> Entity
                notNull (entity.EntityStateOpt :> obj) && not entity.EntityStateOpt.Invalidated ||
                SUMap.containsKey (simulant :?> Entity) world.EntityStates
            else
                match namesLength with
                | 1 -> true
                | 2 -> UMap.containsKey (simulant :?> Screen) world.ScreenStates
                | 3 -> UMap.containsKey (simulant :?> Group) world.GroupStates
                | _  -> failwithumf ()

        /// Determine if a simulant is contained by, or is the same as, the currently selected screen or the omni-screen.
        /// Game is always considered 'selected' as well.
        static member getSelected (simulant : Simulant) world =
            match Address.getNames simulant.SimulantAddress with
            | [||] -> failwithumf ()
            | [|_|] -> true
            | names ->
                let screenName = names.[1]
                match World.getOmniScreenOpt world with
                | Some omniScreen when omniScreen.Name = screenName -> true
                | _ ->
                    match World.getSelectedScreenOpt world with
                    | Some screen when screen.Name = screenName -> true
                    | _ -> false

        /// Convert an address to a concrete simulant reference.
        static member derive address =
            let namesLength = address |> Address.getNames |> Array.length
            if namesLength >= 4
            then Entity (Address.changeType<obj, Entity> address) :> Simulant
            else
                match namesLength with
                | 1 -> Game.Handle :> Simulant
                | 2 -> Screen (Address.changeType<obj, Screen> address) :> Simulant
                | 3 -> Group (Address.changeType<obj, Group> address) :> Simulant
                | _ -> failwithumf ()

        /// Convert an event address to the concrete simulant that it targets.
        static member deriveFromEvent event =
            let eventAddressNames = Address.getNames event
            let eventTargetIndex = Array.findIndex (fun name -> name = "Event") eventAddressNames + 1
            if eventTargetIndex < Array.length eventAddressNames then
                let eventTarget = eventAddressNames |> Array.skip eventTargetIndex |> rtoa
                World.derive eventTarget
            else failwithumf ()

[<RequireQualifiedAccess>]
module PropertyDescriptor =

    /// Check that an entity contains the given property descriptor.
    let containsPropertyDescriptor<'s when 's :> SimulantState> (propertyDescriptor : PropertyDescriptor) (simulant : Simulant) world =
        let properties = typeof<'s>.GetProperties ()
        if Seq.exists (fun (property : PropertyInfo) ->
            // NOTE: not including designer properties since their equivalence can't be detected without a value query.
            // TODO: consider adding the required value query here?
            property.Name = propertyDescriptor.PropertyName &&
            property.PropertyType = propertyDescriptor.PropertyType)
            properties then true
        else
            let state = World.getState simulant world
            let xtensionOpt =
                properties |>
                Array.tryFind (fun p -> p.Name = Constants.Engine.XtensionPropertyName && p.PropertyType = typeof<Xtension>) |>
                Option.map (fun p -> p.GetValue state :?> Xtension)
            match xtensionOpt with
            | Some xtension ->
                let mutable p = Unchecked.defaultof<Property>
                Xtension.tryGetProperty (propertyDescriptor.PropertyName, xtension, &p) &&
                p.PropertyType = propertyDescriptor.PropertyType
            | None -> false

    /// Attempt to get the simulant's property value.
    let tryGetValue propertyDescriptor simulant world =
        let propertyName = propertyDescriptor.PropertyName
        match World.tryGetProperty (propertyName, simulant, world) with
        | (true, property) -> Some property.PropertyValue
        | (false, _) -> None

    /// Attempt to set the simulant's property value.
    let trySetValue propertyDescriptor propertyValue simulant world =
        let (propertyName, propertyType) = (propertyDescriptor.PropertyName, propertyDescriptor.PropertyType)
        let property = { PropertyType = propertyType; PropertyValue = propertyValue }
        World.trySetProperty propertyName property simulant world

    /// Get the property descriptors as constructed from the given function in the given context.
    let getPropertyDescriptors<'s when 's :> SimulantState> simulantOpt world =
        match simulantOpt with
        | Some simulant ->
            // OPTIMIZATION: seqs used for speed.
            let properties = typeof<'s>.GetProperties ()
            let properties = Seq.filter (fun (property : PropertyInfo) -> property.Name <> Constants.Engine.TransformPropertyName) properties
            let properties = Seq.filter (fun (property : PropertyInfo) -> property.Name <> Constants.Engine.XtensionPropertyName) properties
            let properties = Seq.filter (fun (property : PropertyInfo) -> property.Name <> "Flags") properties
            let properties = Seq.filter (fun (property : PropertyInfo) -> Seq.isEmpty (property.GetCustomAttributes<ExtensionAttribute> ())) properties
            let properties =
                Seq.filter (fun (property : PropertyInfo) ->
                    property.Name = "Degrees" || property.Name = "DegreesLocal" || // HACK: we allow degrees specifically for the editor.
                    not (Reflection.isPropertyNonPersistentByName property.Name))
                    properties
            let propertyDescriptors =
                Seq.map (fun (property : PropertyInfo) ->
                    let propertyName = property.Name
                    let property = World.getProperty propertyName simulant world
                    let propertyDescriptor = { PropertyType = property.PropertyType; PropertyName = propertyName }
                    propertyDescriptor)
                    properties
            let propertyDescriptors =
                match simulant :> obj with
                | :? Entity as entity ->
                    let properties' = World.getEntityXtensionProperties entity world
                    let propertyDescriptors' =
                        Seq.fold
                            (fun propertyDescriptors' (propertyName, property : Property) ->
                                if property.PropertyType = typeof<ComputedProperty> then
                                    propertyDescriptors'
                                elif not (Reflection.isPropertyNonPersistentByName propertyName) then
                                    let propertyType = property.PropertyType
                                    let propertyDescriptor = { PropertyName = propertyName; PropertyType = propertyType }
                                    propertyDescriptor :: propertyDescriptors'
                                else propertyDescriptors')
                            []
                            properties'
                    Seq.append propertyDescriptors' propertyDescriptors
                | _ -> propertyDescriptors
            List.ofSeq propertyDescriptors
        | None -> []