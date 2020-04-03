// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu
open System
open Prime
open Nu

/// Describes a generalized simulant value independent of the engine.
/// Not used for serialization.
type [<NoEquality; NoComparison>] SimulantDescriptor =
    { SimulantNameOpt : string option
      SimulantDispatcherName : string
      SimulantProperties : Map<string, Property>
      SimulantChildren : SimulantDescriptor list }

/// Describes a game value independent of the engine.
/// Used to directly serialize a game.
type [<NoComparison>] GameDescriptor =
    { GameDispatcherName : string
      GameProperties : Map<string, Symbol>
      ScreenDescriptors : ScreenDescriptor list }

    /// The empty game descriptor.
    static member empty =
        { GameDispatcherName = String.Empty
          GameProperties = Map.empty
          ScreenDescriptors = [] }

/// Describes a screen value independent of the engine.
/// Used to directly serialize a screen.
and [<NoComparison>] ScreenDescriptor =
    { ScreenDispatcherName : string
      ScreenProperties : Map<string, Symbol>
      LayerDescriptors : LayerDescriptor list }

    /// The empty screen descriptor.
    static member empty =
        { ScreenDispatcherName = String.Empty
          ScreenProperties = Map.empty
          LayerDescriptors = [] }

    /// Derive a name from the dispatcher.
    static member getNameOpt dispatcher =
        dispatcher.ScreenProperties |>
        Map.tryFind (Property? Name) |>
        Option.map symbolToValue<string>

/// Describes a layer value independent of the engine.
/// Used to directly serialize a layer.
and [<NoComparison>] LayerDescriptor =
    { LayerDispatcherName : string
      LayerProperties : Map<string, Symbol>
      EntitieDescriptors : EntityDescriptor list }

    /// The empty layer descriptor.
    static member empty =
        { LayerDispatcherName = String.Empty
          LayerProperties = Map.empty
          EntitieDescriptors = [] }

    /// Derive a name from the dispatcher.
    static member getNameOpt dispatcher =
        dispatcher.LayerProperties |>
        Map.tryFind (Property? Name) |>
        Option.map symbolToValue<string>

/// Describes an entity value independent of the engine.
/// Used to directly serialize an entity.
and [<NoComparison>] EntityDescriptor =
    { EntityDispatcherName : string
      EntityProperties : Map<string, Symbol> }

    /// The empty entity descriptor.
    static member empty =
        { EntityDispatcherName = String.Empty
          EntityProperties = Map.empty }

    /// Derive a name from the dispatcher.
    static member getNameOpt dispatcher =
        dispatcher.EntityProperties |>
        Map.tryFind (Property? Name) |>
        Option.map symbolToValue<string>

/// Initializes a property.
type [<NoEquality; NoComparison>] PropertyInitializer =
    | PropertyDefinition of PropertyDefinition
    | EventHandlerDefinition of (Event -> obj) * obj Address
    | FixDefinition of World Lens * World Lens * bool

/// Contains primitives for describing simulants.
[<RequireQualifiedAccess>]
module Describe =

    let private initializersToProperties initializers world =
        initializers |>
        List.map (fun initializer ->
            match initializer with
            | PropertyDefinition def -> Some (def.PropertyType, def.PropertyName, def.PropertyExpr)
            | EventHandlerDefinition _ -> None
            | FixDefinition _ -> None) |>
        List.definitize |>
        List.map (fun (ty, name, expr) ->
            let valueOpt =
                match expr with
                | DefineExpr value -> Some value
                | VariableExpr fn -> Some (fn world)
                | ComputedExpr _ -> None // computed property cannot be an initializer...
            match valueOpt with
            | Some value ->
                let property =
                    match name with
                    | "StaticData"  | "Model" | _ when name.EndsWith "Model" ->
                        // HACK: need to convert these to designer properties...
                        { PropertyType = typeof<DesignerProperty>; PropertyValue = { DesignerType = ty; DesignerValue = value }}
                    | _ -> { PropertyType = ty; PropertyValue = value }
                Some (name, property)
            | None -> None) |>
        List.definitize |>
        Map.ofList

    let private initializersToEventHandlers initializers (simulant : Simulant) =
        initializers |>
        List.map (fun initializer ->
            match initializer with
            | PropertyDefinition _ -> None
            | EventHandlerDefinition (handler, partialAddress) -> Some (handler, partialAddress --> simulant.SimulantAddress, simulant)
            | FixDefinition _ -> None) |>
        List.definitize

    let private initializersToFixes initializers (simulant : Simulant) =
        initializers |>
        List.map (fun initializer ->
            match initializer with
            | PropertyDefinition _ -> None
            | EventHandlerDefinition _ -> None
            | FixDefinition (left, right, breaking) -> Some (simulant, left, right, breaking)) |>
        List.definitize

    /// Describe a simulant with the given initializers and contained children.
    let simulant5 dispatcherName nameOpt (initializers : PropertyInitializer list) children simulant world =
        let properties = initializersToProperties initializers world
        let eventHandlers = initializersToEventHandlers initializers simulant
        let fixes = initializersToFixes initializers simulant
        let descriptor = { SimulantNameOpt = nameOpt; SimulantDispatcherName = dispatcherName; SimulantProperties = properties; SimulantChildren = children }
        (descriptor, eventHandlers, fixes)

    /// Describe a simulant with the given initializers and contained children.
    let simulant<'d when 'd :> GameDispatcher> nameOpt initializers children simulant world =
        simulant5 typeof<'d>.Name nameOpt initializers children simulant world

    /// Describe a game with the given initializers and contained screens.
    let game5 dispatcherName (initializers : PropertyInitializer list) (screens : SimulantDescriptor list) (game : Game) world =
        simulant5 dispatcherName None initializers screens game world

    /// Describe a game with the given initializers and contained screens.
    let game<'d when 'd :> GameDispatcher> initializers screens game world =
        game5 typeof<'d>.Name initializers screens game world

    /// Describe a screen with the given initializers and contained layers.
    let screen5 dispatcherName nameOpt (initializers : PropertyInitializer list) (layers : SimulantDescriptor list) (screen : Screen) world =
        simulant5 dispatcherName nameOpt initializers layers screen world

    /// Describe a screen with the given initializers and contained layers.
    let screen<'d when 'd :> ScreenDispatcher> nameOpt initializers layers screen world =
        screen5 typeof<'d>.Name nameOpt initializers layers screen world

    /// Describe a layer with the given initializers and contained entities.
    let layer5 dispatcherName nameOpt (initializers : PropertyInitializer list) (entities : SimulantDescriptor list) (layer : Layer) world =
        simulant5 dispatcherName nameOpt initializers entities layer world

    /// Describe a layer with the given initializers and contained entities.
    let layer<'d when 'd :> LayerDispatcher> initializers entities world =
        layer5 typeof<'d>.Name initializers entities world

    /// Describe an entity with the given initializers.
    let entity4 dispatcherName nameOpt (initializers : PropertyInitializer list) (entity : Entity) world =
        simulant5 dispatcherName nameOpt initializers [] entity world

    /// Describe an entity with the given initializers.
    let entity<'d when 'd :> EntityDispatcher> nameOpt initializers world =
        entity4 typeof<'d>.Name nameOpt initializers world