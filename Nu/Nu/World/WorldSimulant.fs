// Nu Game Engine.
// Copyright (C) Bryan Edds.

namespace Nu
open System
open System.Reflection
open System.Runtime.CompilerServices
open Prime

/// Generalized simulant functions for the world.
[<AutoOpen>]
module WorldSimulantModule =

    type World with

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

        static member internal getXtension (simulant : Simulant) world =
            match simulant with
            | :? Entity as entity -> World.getEntityXtension entity world
            | :? Group as group -> World.getGroupXtension group world
            | :? Screen as screen -> World.getScreenXtension screen world
            | :? Game as game -> World.getGameXtension game world
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

        static member internal attachMissingProperties (simulant : Simulant) world =
            match simulant with
            | :? Entity as entity -> World.attachEntityMissingProperties entity world
            | :? Group as group -> World.attachGroupMissingProperties group world
            | :? Screen as screen -> World.attachScreenMissingProperties screen world
            | :? Game as game -> World.attachGameMissingProperties game world
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

        static member internal trySynchronize initializing reinitializing (simulant : Simulant) (world : World) =
            match simulant with
            | :? Entity as entity -> (World.getEntityDispatcher entity world).TrySynchronize (initializing, reinitializing, entity, world)
            | :? Group as group -> (World.getGroupDispatcher group world).TrySynchronize (initializing, reinitializing, group, world)
            | :? Screen as screen -> (World.getScreenDispatcher screen world).TrySynchronize (initializing, reinitializing, screen, world)
            | :? Game as game -> (World.getGameDispatcher game world).TrySynchronize (initializing, reinitializing, game, world)
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
        static member edit lateBindingsPredicate operation (simulant : Simulant) world =
            match simulant with
            | :? Entity as entity -> World.editEntity lateBindingsPredicate operation entity world
            | :? Group as group -> World.editGroup lateBindingsPredicate operation group world
            | :? Screen as screen -> World.editScreen lateBindingsPredicate operation screen world
            | :? Game as game -> World.editGame lateBindingsPredicate operation game world
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

        /// Notify the engine that a simulant's MMCC model has changed in some automatically undetectable way (such as being mutated directly by user code).
        static member notifyModelChange (simulant : Simulant) world =
            match simulant with
            | :? Entity as entity -> World.notifyEntityModelChange entity world
            | :? Group as group -> World.notifyGroupModelChange group world
            | :? Screen as screen -> World.notifyScreenModelChange screen world
            | :? Game as game -> World.notifyGameModelChange game world
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
            | :? Group as group -> enumerable<Simulant> (World.getSovereignEntities group world)
            | :? Screen as screen -> enumerable<Simulant> (World.getGroups screen world)
            | :? Game -> enumerable<Simulant> (World.getScreens world)
            | _ -> failwithumf ()

        /// Check that a simulant exists in the world.
        static member getExists (simulant : Simulant) (world : World) =
            let namesLength = simulant.SimulantAddress.Names.Length
            if namesLength >= 4 then
                let entity = simulant :?> Entity
                notNull (entity.EntityStateOpt :> obj) && not entity.EntityStateOpt.Invalidated ||
                world.EntityStates.ContainsKey (simulant :?> Entity) 
            else
                match namesLength with
                | 1 -> true
                | 2 -> world.ScreenStates.ContainsKey (simulant :?> Screen) 
                | 3 -> world.GroupStates.ContainsKey (simulant :?> Group) 
                | _  -> failwithumf ()

        /// Determine if a simulant is contained by, or is the same as, any currently selected screen.
        /// Game is always considered 'selected' as well.
        static member getSelected (simulant : Simulant) world =
            match simulant.Names with
            | [||] -> failwithumf ()
            | [|_|] -> true
            | names ->
                let screenName = names.[1]
                match World.getSelectedScreenOpt world with
                | Some screen when screen.Name = screenName -> true
                | _ -> false

        /// Convert an address to a concrete simulant reference.
        static member deriveFromNames (names : string seq) =
            let names = Seq.toArray names // NOTE: this should be fast for arrays because there's an explicit array test in Seq.toArray.
            let namesLength = names.Length
            if namesLength >= 4
            then Entity (rtoa names) :> Simulant
            else
                match namesLength with
                | 1 -> Game.Handle // OPTIMIZATION: avoid allocation.
                | 2 -> Screen (rtoa names)
                | 3 -> Group (rtoa names)
                | _ -> failwithumf ()

        /// Convert an address to a concrete simulant reference.
        static member deriveFromAddress (address : Address) =
            let names = address.Names // OPTIMIZATION: unroll fields to locals to avoid redundant fetches.
            let hashCode = address.HashCode
            let anonymous = address.Anonymous
            let namesLength = names.Length
            if namesLength >= 4
            then Entity { Names = names; HashCode = hashCode; Anonymous = anonymous } :> Simulant
            else
                match namesLength with
                | 1 -> Game.Handle // OPTIMIZATION: avoid allocation.
                | 2 -> Screen { Names = names; HashCode = hashCode; Anonymous = anonymous }
                | 3 -> Group { Names = names; HashCode = hashCode; Anonymous = anonymous }
                | _ -> failwithumf ()

        /// Convert an event address to the concrete simulant that it targets.
        static member deriveFromEventAddress (event : Address) =
            let eventAddressNames = event.Names
            let eventTargetIndex = Array.findIndex (fun name -> name = "Event") eventAddressNames + 1
            if eventTargetIndex < Array.length eventAddressNames then
                let eventTargetNames = eventAddressNames |> Array.skip eventTargetIndex
                World.deriveFromNames eventTargetNames
            else failwithumf ()

        /// Get the property definitions of the given simulant.
        static member getPropertyDefinitions simulant world =
            let state = World.getState simulant world
            Reflection.getReflectivePropertyDefinitions state

        /// Get all the unique reflective property definitions of the given simulant with its containing
        /// dispatcher and / or facet type name, with the property name as key.
        static member getReflectivePropertyDefinitionAndContainingTypes simulant  world =
            let state = World.getState simulant world
            Reflection.getReflectivePropertyDefinitionAndContainingTypes state

/// PropertyDescriptor functions.
[<RequireQualifiedAccess>]
module PropertyDescriptor =

    /// Check that an entity contains the given property descriptor.
    let containsPropertyDescriptor<'s when 's :> SimulantState> propertyName (simulant : Simulant) world =
        let properties = typeof<'s>.GetProperties true
        if Seq.exists (fun (property : PropertyInfo) -> property.Name = propertyName) properties then
            true
        else
            let state = World.getState simulant world
            let xtensionOpt =
                properties
                |> Array.tryFind (fun property -> property.Name = Constants.Engine.XtensionPropertyName && property.PropertyType = typeof<Xtension>)
                |> Option.map (fun property -> property.GetValue state :?> Xtension)
            match xtensionOpt with
            | Some xtension ->
                let mutable p = Unchecked.defaultof<Property>
                Xtension.tryGetProperty (propertyName, xtension, &p)
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

    /// Get the property descriptors and optional containing xtension type name (None for intrinsic properties),
    /// as constructed from the given function in the given context.
    let getPropertyDescriptors<'s when 's :> SimulantState> simulant world =

        // yield property descriptors lazily
        seq {

            // yield intrinsic property descriptors
            let (|IsPersistent|_|) name =
                not (Reflection.isPropertyNonPersistentByName name)
            for property in typeof<'s>.GetProperties true do
                match property.Name with
                | Constants.Engine.TransformPropertyName
                | Constants.Engine.XtensionPropertyName
                | "Flags"
                | "Order" -> ()
                | _ when Seq.notEmpty (property.GetCustomAttributes<ExtensionAttribute> ()) -> ()
                | "Degrees" | "DegreesLocal" // HACK: we allow degrees specifically for the editor.
                | IsPersistent as propertyName ->
                    let property = World.getProperty propertyName simulant world
                    (ValueNone, { PropertyType = property.PropertyType; PropertyName = propertyName })
                | _ -> ()

            // yield extrinsic property descriptors
            let propertyDefinitions = World.getReflectivePropertyDefinitionAndContainingTypes simulant world
            let xtension = World.getXtension simulant world
            for (propertyName, _) in xtension.Properties.Pairs do
                let (lateBindings, property) = propertyDefinitions.[propertyName]
                if property.PropertyType <> typeof<ComputedProperty> &&
                    not (Reflection.isPropertyNonPersistentByName propertyName) then
                    (ValueSome lateBindings, { PropertyName = propertyName; PropertyType = property.PropertyType })}