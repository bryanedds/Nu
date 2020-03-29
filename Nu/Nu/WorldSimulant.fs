// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu
open System
open System.ComponentModel
open System.Reflection
open System.Runtime.CompilerServices
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
            | :? Entity as entity -> Some (World.getEntityScriptFrame entity world)
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
            | :? Layer as layer -> Some (layer.Parent :> Simulant)
            | :? Entity as entity -> Some (entity.Parent :> Simulant)
            | _ -> failwithumf ()

        /// Get the parent of the given simulant.
        [<FunctionBinding>]
        static member getParent (simulant : Simulant) world =
            ignore (world : World)
            match simulant with
            | :? Game -> failwithumf ()
            | :? Screen -> Default.Game :> Simulant
            | :? Layer as layer -> layer.Parent :> Simulant
            | :? Entity as entity -> entity.Parent :> Simulant
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
            (world :> World EventSystem).SimulantExists simulant

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

        /// Take only one event from a stream per update.
        static member internal noMoreThanOncePerUpdate (stream : Stream<'a, World>) =
            stream |>
            Stream.trackEvent4
                (fun (a, current) _ world ->
                    let previous = current
                    let current = World.getUpdateCount world
                    ((a, current), previous < current))
                id (Unchecked.defaultof<'a>, -1L) |>
            Stream.first

        /// Fix the left property to the value of the right, optionally breaking any cycles.
        static member fix (left : Lens<'a, World>) (right : Lens<'a, World>) breaking world =
            match left.This :> obj with
            | null -> failwithumf ()
            | :? Simulant as simulant -> WorldModule.fix5 simulant left right breaking world
            | _ -> failwithumf ()

[<AutoOpen>]
module WorldSimulantOperators =

    /// Fix one property to the value of another, optionally breaking potential cycles.
    let fix<'a> (left : Lens<'a, World>) right breaking world = World.fix left right breaking world

    /// Equate two properties, not breaking potential cycles.
    let inline (===) left right = fix left right false

    /// Equate two properties, breaking potential cycles.
    let inline (=/=) left right = fix left right true

[<RequireQualifiedAccess>]
module PropertyDescriptor =

    /// Check that an entity contains the given property.
    let containsProperty<'s when 's :> Simulant> (property : PropertyInfo) =
        let properties = typeof<'s>.GetProperties property.Name
        Seq.exists (fun item -> item = property) properties

    /// Attempt to get the simulant's property value.
    let tryGetValue propertyDescriptor simulant world =
        let propertyName = propertyDescriptor.PropertyName
        match World.tryGetProperty propertyName simulant world with
        | Some property ->
            match property.PropertyValue with
            | :? DesignerProperty as dp -> Some dp.DesignerValue
            | value -> Some value
        | None -> None

    /// Attempt to set the simulant's property value.
    let trySetValue alwaysPublish nonPersistent propertyDescriptor propertyValue simulant world =
        let (propertyName, propertyType) = (propertyDescriptor.PropertyName, propertyDescriptor.PropertyType)
        let property =
            match World.tryGetProperty propertyName simulant world with
            | Some propertyOld ->
                match propertyOld.PropertyValue with
                | :? DesignerProperty as dp ->
                    let designerProperty = { dp with DesignerType = propertyType; DesignerValue = propertyValue }
                    { PropertyType = typeof<DesignerProperty>; PropertyValue = designerProperty }
                | _ -> { PropertyType = propertyType; PropertyValue = propertyValue }
            | None -> { PropertyType = propertyType; PropertyValue = propertyValue }
        World.trySetProperty propertyName alwaysPublish nonPersistent property simulant world

    /// Get the property descriptors as constructed from the given function in the given context.
    let getPropertyDescriptors<'s when 's :> SimulantState> makePropertyDescriptor contextOpt =
        match contextOpt with
        | Some (simulant, world) ->
            // OPTIMIZATION: seqs used for speed.
            let properties = typeof<'s>.GetProperties ()
            let typeConverterAttribute = TypeConverterAttribute typeof<SymbolicConverter>
            let properties = Seq.filter (fun (property : PropertyInfo) -> property.Name <> Property? Xtension) properties
            let properties = Seq.filter (fun (property : PropertyInfo) -> property.Name <> Property? Transform) properties
            let properties = Seq.filter (fun (property : PropertyInfo) -> property.Name <> Property? Flags) properties
            let properties = Seq.filter (fun (property : PropertyInfo) -> Seq.isEmpty (property.GetCustomAttributes<ExtensionAttribute> ())) properties
            let properties = Seq.filter (fun (property : PropertyInfo) -> Reflection.isPropertyPersistentByName property.Name) properties
            let propertyDescriptors =
                Seq.map (fun (property : PropertyInfo) ->
                    let propertyName = property.Name
                    let property = World.getProperty property.Name simulant world
                    let propertyType =
                        match property.PropertyValue with
                        | :? DesignerProperty as designerProperty -> designerProperty.DesignerType
                        | _ -> property.PropertyType
                    let property = { PropertyType = propertyType; PropertyName = propertyName }
                    makePropertyDescriptor (property, [|typeConverterAttribute|]))
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
                                elif Reflection.isPropertyPersistentByName propertyName then
                                    let propertyType =
                                        match property.PropertyValue with
                                        | :? DesignerProperty as designerProperty -> designerProperty.DesignerType
                                        | _ -> property.PropertyType
                                    let propertyDescriptor = { PropertyName = propertyName; PropertyType = propertyType }
                                    let propertyDescriptor = makePropertyDescriptor (propertyDescriptor, [|typeConverterAttribute|]) : System.ComponentModel.PropertyDescriptor
                                    propertyDescriptor :: propertyDescriptors'
                                else propertyDescriptors')
                            []
                            properties'
                    Seq.append propertyDescriptors' propertyDescriptors
                | _ -> propertyDescriptors
            List.ofSeq propertyDescriptors
        | None -> []