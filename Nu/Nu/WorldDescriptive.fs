// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2018.

namespace Nu
open System
open Prime
open Nu

/// A marker interface for simulant descriptors.
type SimulantDescriptor =
    interface
        abstract Children : SimulantDescriptor list
        end

/// Describes a game value independent of the engine.
type [<NoComparison>] GameDescriptor =
    { GameDispatcherName : string
      GameProperties : Map<string, Symbol>
      ScreenDescriptors : ScreenDescriptor list }

    /// The empty game descriptor.
    static member empty =
        { GameDispatcherName = String.Empty
          GameProperties = Map.empty
          ScreenDescriptors = [] }

    interface SimulantDescriptor with
        member this.Children =
            this.ScreenDescriptors |> enumerable<SimulantDescriptor> |> List.ofSeq

/// Describes a screen value independent of the engine.
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
          
    interface SimulantDescriptor with
        member this.Children =
            this.LayerDescriptors |> enumerable<SimulantDescriptor> |> List.ofSeq

/// Describes a layer value independent of the engine.
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

    interface SimulantDescriptor with
        member this.Children =
            this.EntitieDescriptors |> enumerable<SimulantDescriptor> |> List.ofSeq

/// Describes an entity value independent of the engine.
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

    interface SimulantDescriptor with
        member this.Children = []

/// A snapshot of a simulant.
type [<NoEquality; NoComparison>] SimulantSnapshot =
    { SimulantNameOpt : string option
      SimulantDispatcherName : string
      SimulantProperties : Map<string, Property>
      SimulantChildren : SimulantSnapshot list }

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

    let private simulantToDescriptor dispatcherName definitions (children : SimulantDescriptor list) (simulant : Simulant) =
        match simulant with
        | :? Game -> { GameDispatcherName = dispatcherName; GameProperties = definitions; ScreenDescriptors = children |> Seq.cast<ScreenDescriptor> |> List.ofSeq } :> SimulantDescriptor
        | :? Screen -> { ScreenDispatcherName = dispatcherName; ScreenProperties = definitions; LayerDescriptors = children |> Seq.cast<LayerDescriptor> |> List.ofSeq } :> SimulantDescriptor
        | :? Layer -> { LayerDispatcherName = dispatcherName; LayerProperties = definitions; EntitieDescriptors = children |> Seq.cast<EntityDescriptor> |> List.ofSeq } :> SimulantDescriptor
        | :? Entity -> { EntityDispatcherName = dispatcherName; EntityProperties = definitions } :> SimulantDescriptor
        | _ -> failwithumf ()

    /// Describe a simulant with the given definitions and contained children.
    let simulant5 dispatcherName (initializers : PropertyInitializer list) children simulant world =
        let properties = initializersToProperties initializers world
        let eventHandlers = initializersToEventHandlers initializers simulant
        let fixes = initializersToFixes initializers simulant
        let snapshot = { SimulantNameOpt = None; SimulantDispatcherName = dispatcherName; SimulantProperties = properties; SimulantChildren = children }
        (snapshot, eventHandlers, fixes)

    /// Describe a simulant with the given definitions and contained children.
    let simulant<'d when 'd :> GameDispatcher> initializers children simulant world =
        simulant5 typeof<'d>.Name initializers children simulant world

    /// Describe a game with the given definitions and contained screens.
    let game5 dispatcherName (initializers : PropertyInitializer list) (screens : SimulantSnapshot list) (game : Game) world =
        simulant5 dispatcherName initializers screens game world

    /// Describe a game with the given definitions and contained screens.
    let game<'d when 'd :> GameDispatcher> initializers screens game world =
        game5 typeof<'d>.Name initializers screens game world

    /// Describe a screen with the given definitions and contained layers.
    let screen5 dispatcherName (initializers : PropertyInitializer list) (layers : SimulantSnapshot list) (screen : Screen) world =
        simulant5 dispatcherName initializers layers screen world

    /// Describe a screen with the given definitions and contained layers.
    let screen<'d when 'd :> ScreenDispatcher> definitions layers screen world =
        screen5 typeof<'d>.Name definitions layers screen world

    /// Describe a layer with the given definitions and contained entities.
    let layer5 dispatcherName (initializers : PropertyInitializer list) (entities : SimulantSnapshot list) (layer : Layer) world =
        simulant5 dispatcherName initializers entities layer world

    /// Describe a layer with the given definitions and contained entities.
    let layer<'d when 'd :> LayerDispatcher> definitions entities world =
        layer5 typeof<'d>.Name definitions entities world

    /// Describe an entity with the given definitions.
    let entity4 dispatcherName (initializers : PropertyInitializer list) (entity : Entity) world =
        simulant5 dispatcherName initializers [] entity world

    /// Describe an entity with the given definitions.
    let entity<'d when 'd :> EntityDispatcher> definitions world =
        entity4 typeof<'d>.Name definitions world