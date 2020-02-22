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

/// Initializes a property.
type [<NoEquality; NoComparison>] PropertyInitializer =
    | PropertyDefinition of PropertyDefinition
    | EventHandlerDefinition of (Event -> obj) * obj Address
    | EquationDefinition of string * World Lens * bool

/// Contains primitives for describing simulants.
[<RequireQualifiedAccess>]
module Describe =

    let private initializersToDefinitions initializers world =
        initializers |>
        Seq.map (fun initializer ->
            match initializer with
            | PropertyDefinition def -> Some (def.PropertyType, def.PropertyName, def.PropertyExpr)
            | EventHandlerDefinition _ -> None
            | EquationDefinition _ -> None) |>
        Seq.definitize |>
        Seq.map (fun (ty, name, expr) ->
            let value = match expr with DefineExpr value -> value | VariableExpr fn -> fn world
            let symbol = valueToSymbol value
            let symbol = match name with "Model" -> Symbols ([Text (ty.AssemblyQualifiedName, None); symbol], None) | _ -> symbol
            (name, symbol)) |>
        Map.ofSeq

    let private initializersToEventHandlers initializers (simulant : Simulant) =
        initializers |>
        Seq.map (fun initializer ->
            match initializer with
            | PropertyDefinition _ -> None
            | EventHandlerDefinition (handler, address) -> Some (handler, address --> simulant.SimulantAddress, simulant)
            | EquationDefinition _ -> None) |>
        Seq.definitize |>
        Seq.toList

    let private initializersToEquations initializers (simulant : Simulant)=
        initializers |>
        Seq.map (fun initializer ->
            match initializer with
            | PropertyDefinition _ -> None
            | EventHandlerDefinition _ -> None
            | EquationDefinition (leftName, right, breaking) -> Some (leftName, simulant, right, breaking)) |>
        Seq.definitize |>
        Seq.toList

    let private simulantToDescriptor dispatcherName definitions (children : SimulantDescriptor seq) (simulant : Simulant) =
        match simulant with
        | :? Game -> { GameDispatcherName = dispatcherName; GameProperties = definitions; ScreenDescriptors = children |> Seq.cast<ScreenDescriptor> |> List.ofSeq } :> SimulantDescriptor
        | :? Screen -> { ScreenDispatcherName = dispatcherName; ScreenProperties = definitions; LayerDescriptors = children |> Seq.cast<LayerDescriptor> |> List.ofSeq } :> SimulantDescriptor
        | :? Layer -> { LayerDispatcherName = dispatcherName; LayerProperties = definitions; EntitieDescriptors = children |> Seq.cast<EntityDescriptor> |> List.ofSeq } :> SimulantDescriptor
        | :? Entity -> { EntityDispatcherName = dispatcherName; EntityProperties = definitions } :> SimulantDescriptor
        | _ -> failwithumf ()

    /// Describe a simulant with the given definitions and contained children.
    let simulant5 dispatcherName (initializers : PropertyInitializer seq) children simulant world =
        let definitions = initializersToDefinitions initializers world
        let eventHandlers = initializersToEventHandlers initializers simulant
        let equations = initializersToEquations initializers simulant
        let descriptor = simulantToDescriptor dispatcherName definitions children simulant
        (descriptor, eventHandlers, equations)

    /// Describe a simulant with the given definitions and contained children.
    let simulant<'d when 'd :> GameDispatcher> initializers children simulant world =
        simulant5 typeof<'d>.Name initializers children simulant world

    /// Describe a game with the given definitions and contained screens.
    let game5 dispatcherName (initializers : PropertyInitializer seq) (screens : ScreenDescriptor seq) (game : Game) world =
        simulant5 dispatcherName initializers (Seq.cast<SimulantDescriptor> screens) game world |> Triple.mapA cast<GameDescriptor>

    /// Describe a game with the given definitions and contained screens.
    let game<'d when 'd :> GameDispatcher> initializers screens game world =
        game5 typeof<'d>.Name initializers screens game world

    /// Describe a screen with the given definitions and contained layers.
    let screen5 dispatcherName (initializers : PropertyInitializer seq) (layers : LayerDescriptor seq) (screen : Screen) world =
        simulant5 dispatcherName initializers (Seq.cast<SimulantDescriptor> layers) screen world |> Triple.mapA cast<ScreenDescriptor>

    /// Describe a screen with the given definitions and contained layers.
    let screen<'d when 'd :> ScreenDispatcher> definitions layers screen world =
        screen5 typeof<'d>.Name definitions layers screen world

    /// Describe a layer with the given definitions and contained entities.
    let layer5 dispatcherName (initializers : PropertyInitializer seq) (entities : EntityDescriptor seq) (layer : Layer) world =
        simulant5 dispatcherName initializers (Seq.cast<SimulantDescriptor> entities) layer world |> Triple.mapA cast<LayerDescriptor>

    /// Describe a layer with the given definitions and contained entities.
    let layer<'d when 'd :> LayerDispatcher> definitions entities world =
        layer5 typeof<'d>.Name definitions entities world

    /// Describe an entity with the given definitions.
    let entity4 dispatcherName (initializers : PropertyInitializer seq) (entity : Entity) world =
        simulant5 dispatcherName initializers Seq.empty entity world |> Triple.mapA cast<EntityDescriptor>

    /// Describe an entity with the given definitions.
    let entity<'d when 'd :> EntityDispatcher> definitions world =
        entity4 typeof<'d>.Name definitions world