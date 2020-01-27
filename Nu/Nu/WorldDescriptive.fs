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
      Screens : ScreenDescriptor list }

    /// The empty game descriptor.
    static member empty =
        { GameDispatcherName = String.Empty
          GameProperties = Map.empty
          Screens = [] }

    interface SimulantDescriptor with
        member this.Children =
            this.Screens |> enumerable<SimulantDescriptor> |> List.ofSeq

/// Describes a screen value independent of the engine.
and [<NoComparison>] ScreenDescriptor =
    { ScreenDispatcherName : string
      ScreenProperties : Map<string, Symbol>
      Layers : LayerDescriptor list }

    /// The empty screen descriptor.
    static member empty =
        { ScreenDispatcherName = String.Empty
          ScreenProperties = Map.empty
          Layers = [] }

    /// Derive a name from the dispatcher.
    static member getNameOpt dispatcher =
        dispatcher.ScreenProperties |>
        Map.tryFind (Property? Name) |>
        Option.map symbolToValue<string>
          
    interface SimulantDescriptor with
        member this.Children =
            this.Layers |> enumerable<SimulantDescriptor> |> List.ofSeq

/// Describes a layer value independent of the engine.
and [<NoComparison>] LayerDescriptor =
    { LayerDispatcherName : string
      LayerProperties : Map<string, Symbol>
      Entities : EntityDescriptor list }

    /// The empty layer descriptor.
    static member empty =
        { LayerDispatcherName = String.Empty
          LayerProperties = Map.empty
          Entities = [] }

    /// Derive a name from the dispatcher.
    static member getNameOpt dispatcher =
        dispatcher.LayerProperties |>
        Map.tryFind (Property? Name) |>
        Option.map symbolToValue<string>

    interface SimulantDescriptor with
        member this.Children =
            this.Entities |> enumerable<SimulantDescriptor> |> List.ofSeq

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
    | Equation of string * World Lens * bool

/// Contains primitives for describing simulants.
/// TODO: get rid of code duplication in here.
module Describe =

    /// Describe a game with the given definitions and contained screens.
    let game3 dispatcherName (initializers : PropertyInitializer seq) (screens : ScreenDescriptor seq) (game : Game) world =
        let definitions =
            initializers |>
            Seq.map (fun def -> match def with PropertyDefinition def -> Some (def.PropertyName, def.PropertyExpr) | EventHandlerDefinition _ | Equation _ -> None) |>
            Seq.definitize |>
            Seq.map (mapSnd (function DefineExpr value -> value | VariableExpr fn -> fn world)) |>
            Seq.map (mapSnd valueToSymbol) |>
            Map.ofSeq
        let eventHandlers =
            initializers |>
            Seq.map (fun def -> match def with EventHandlerDefinition (handler, address) -> Some (handler, address --> game, game :> Simulant) | PropertyDefinition _ | Equation _ -> None) |>
            Seq.definitize |>
            Seq.toList
        let equations =
            initializers |>
            Seq.map (fun def -> match def with Equation (leftName, right, breaking) -> Some (leftName, game :> Simulant, right, breaking) | PropertyDefinition _ | EventHandlerDefinition _ -> None) |>
            Seq.definitize |>
            Seq.toList
        let descriptor =
            { GameDispatcherName = dispatcherName
              GameProperties = definitions
              Screens = List.ofSeq screens }
        (descriptor, eventHandlers, equations)

    /// Describe a game with the given definitions and contained screens.
    let game<'d when 'd :> GameDispatcher> initializers screens game world =
        game3 typeof<'d>.Name initializers screens game world

    /// Describe a screen with the given definitions and contained layers.
    let screen3 dispatcherName (initializers : PropertyInitializer seq) (layers : LayerDescriptor seq) (screen : Screen) world =
        let definitions =
            initializers |>
            Seq.map (fun def -> match def with PropertyDefinition def -> Some (def.PropertyName, def.PropertyExpr) | EventHandlerDefinition _ | Equation _ -> None) |>
            Seq.definitize |>
            Seq.map (mapSnd (function DefineExpr value -> value | VariableExpr fn -> fn world)) |>
            Seq.map (mapSnd valueToSymbol) |>
            Map.ofSeq
        let eventHandlers =
            initializers |>
            Seq.map (fun def -> match def with EventHandlerDefinition (handler, address) -> Some (handler, address --> screen, screen :> Simulant) | PropertyDefinition _ | Equation _ -> None) |>
            Seq.definitize |>
            Seq.toList
        let equations =
            initializers |>
            Seq.map (fun def -> match def with Equation (leftName, right, breaking) -> Some (leftName, screen :> Simulant, right, breaking) | EventHandlerDefinition _ | PropertyDefinition _ -> None) |>
            Seq.definitize |>
            Seq.toList
        let descriptor =
            { ScreenDispatcherName = dispatcherName
              ScreenProperties = definitions
              Layers = List.ofSeq layers }
        (descriptor, eventHandlers, equations)

    /// Describe a screen with the given definitions and contained layers.
    let screen<'d when 'd :> ScreenDispatcher> definitions layers screen world =
        screen3 typeof<'d>.Name definitions layers screen world

    /// Describe a layer with the given definitions and contained entities.
    let layer3 dispatcherName (initializers : PropertyInitializer seq) (entities : EntityDescriptor seq) (layer : Layer) world =
        let definitions =
            initializers |>
            Seq.map (fun def -> match def with PropertyDefinition def -> Some (def.PropertyName, def.PropertyExpr) | EventHandlerDefinition _ | Equation _ -> None) |>
            Seq.definitize |>
            Seq.map (mapSnd (function DefineExpr value -> value | VariableExpr fn -> fn world)) |>
            Seq.map (mapSnd valueToSymbol) |>
            Map.ofSeq
        let eventHandlers =
            initializers |>
            Seq.map (fun def -> match def with EventHandlerDefinition (handler, address) -> Some (handler, address --> layer, layer :> Simulant) | PropertyDefinition _ | Equation _ -> None) |>
            Seq.definitize |>
            Seq.toList
        let equations =
            initializers |>
            Seq.map (fun def -> match def with Equation (leftName, right, breaking) -> Some (leftName, layer :> Simulant, right, breaking) | EventHandlerDefinition _ | PropertyDefinition _ -> None) |>
            Seq.definitize |>
            Seq.toList
        let descriptor =
            { LayerDispatcherName = dispatcherName
              LayerProperties = definitions
              Entities = List.ofSeq entities }
        (descriptor, eventHandlers, equations)

    /// Describe a layer with the given definitions and contained entities.
    let layer<'d when 'd :> LayerDispatcher> definitions entities world =
        layer3 typeof<'d>.Name definitions entities world

    /// Describe an entity with the given definitions.
    let entity2 dispatcherName (initializers : PropertyInitializer seq) (entity : Entity) world =
        let definitions =
            initializers |>
            Seq.map (fun def -> match def with PropertyDefinition def -> Some (def.PropertyName, def.PropertyExpr) | EventHandlerDefinition _ | Equation _ -> None) |>
            Seq.definitize |>
            Seq.map (mapSnd (function DefineExpr value -> value | VariableExpr fn -> fn world)) |>
            Seq.map (mapSnd valueToSymbol) |>
            Map.ofSeq
        let eventHandlers =
            initializers |>
            Seq.map (fun def -> match def with EventHandlerDefinition (handler, address) -> Some (handler, address --> entity, entity :> Simulant) | PropertyDefinition _ | Equation _ -> None) |>
            Seq.definitize |>
            Seq.toList
        let equations =
            initializers |>
            Seq.map (fun def -> match def with Equation (leftName, right, breaking) -> Some (leftName, entity :> Simulant, right, breaking) | EventHandlerDefinition _ | PropertyDefinition _ -> None) |>
            Seq.definitize |>
            Seq.toList
        let descriptor =
            { EntityDispatcherName = dispatcherName
              EntityProperties = definitions }
        (descriptor, eventHandlers, equations)

    /// Describe an entity with the given definitions.
    let entity<'d when 'd :> EntityDispatcher> definitions world =
        entity2 typeof<'d>.Name definitions world