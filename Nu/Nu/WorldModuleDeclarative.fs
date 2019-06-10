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

/// Contains primitives for describing simulants.
module Describe =

    /// Describe a game with the given properties values and contained screens.
    let game3 dispatcherName (properties : PropertyDefinition seq) (screens : ScreenDescriptor seq) world =
        let properties =
            properties |>
            Seq.map (fun def -> (def.PropertyName, def.PropertyExpr)) |>
            Seq.map (mapSnd (function DefineExpr value -> value | VariableExpr fn -> fn world)) |>
            Seq.map (mapSnd valueToSymbol) |>
            Map.ofSeq
        { GameDispatcherName = dispatcherName
          GameProperties = properties
          Screens = List.ofSeq screens }

    /// Describe a game with the given properties values and contained screens.
    let game<'d when 'd :> GameDispatcher> properties screens world =
        game3 typeof<'d>.Name properties screens world

    /// Describe a screen with the given properties values and contained layers.
    let screen3 dispatcherName (properties : PropertyDefinition seq) (layers : LayerDescriptor seq) world =
        let properties =
            properties |>
            Seq.map (fun def -> (def.PropertyName, def.PropertyExpr)) |>
            Seq.map (mapSnd (function DefineExpr value -> value | VariableExpr fn -> fn world)) |>
            Seq.map (mapSnd valueToSymbol) |>
            Map.ofSeq
        { ScreenDispatcherName = dispatcherName
          ScreenProperties = properties
          Layers = List.ofSeq layers }

    /// Describe a screen with the given properties values and contained layers.
    let screen<'d when 'd :> ScreenDispatcher> properties layers world =
        screen3 typeof<'d>.Name properties layers world

    /// Describe a layer with the given properties values and contained entities.
    let layer3 dispatcherName (properties : PropertyDefinition seq) (entities : EntityDescriptor seq) world =
        let properties =
            properties |>
            Seq.map (fun def -> (def.PropertyName, def.PropertyExpr)) |>
            Seq.map (mapSnd (function DefineExpr value -> value | VariableExpr fn -> fn world)) |>
            Seq.map (mapSnd valueToSymbol) |>
            Map.ofSeq
        { LayerDispatcherName = dispatcherName
          LayerProperties = properties
          Entities = List.ofSeq entities }

    /// Describe a layer with the given properties values and contained entities.
    let layer<'d when 'd :> LayerDispatcher> properties entities world =
        layer3 typeof<'d>.Name properties entities world

    /// Describe an entity with the given properties values.
    let entity2 dispatcherName (properties : PropertyDefinition seq) world =
        let properties =
            properties |>
            Seq.map (fun def -> (def.PropertyName, def.PropertyExpr)) |>
            Seq.map (mapSnd (function DefineExpr value -> value | VariableExpr fn -> fn world)) |>
            Seq.map (mapSnd valueToSymbol) |>
            Map.ofSeq
        { EntityDispatcherName = dispatcherName
          EntityProperties = properties }

    /// Describe an entity with the given properties values.
    let entity<'d when 'd :> EntityDispatcher> properties world =
        entity2 typeof<'d>.Name properties world

/// Describes the behavior of a screen.
type [<NoComparison>] ScreenBehavior =
    | Vanilla
    | Omniscreen
    | Dissolve of DissolveData
    | Splash of DissolveData * SplashData * Screen

/// Describes the layout of a simulant
type SimulantLayout = interface end

/// Describes the layout of an entity.
type [<NoEquality; NoComparison>] EntityLayout =
    | EntityFromProperties of string * string * PropertyDefinition list
    | EntityFromFile of string * string
    interface SimulantLayout

    /// Expand an entity layout to its constituent parts.
    static member expand layout world =
        match layout with
        | EntityFromProperties (dispatcherName, name, properties) -> Left (name, Describe.entity2 dispatcherName properties world)
        | EntityFromFile (name, filePath) -> Right (name, filePath)

/// Describes the layout of a layer.
type [<NoEquality; NoComparison>] LayerLayout =
    | LayerFromProperties of string * string * PropertyDefinition list * EntityLayout list
    | LayerFromFile of string * string
    interface SimulantLayout
    
    /// Expand a layer layout to its constituent parts.
    static member expand layout world =
        match layout with
        | LayerFromProperties (dispatcherName, name, properties, entities) ->
            let descriptorsPlus = List.map (flip EntityLayout.expand world) entities
            let descriptors = Either.getLeftValues descriptorsPlus |> List.map (fun (entityName, descriptor) -> { descriptor with EntityProperties = Map.add (Property? Name) (valueToSymbol entityName) descriptor.EntityProperties })
            let filePaths = Either.getRightValues descriptorsPlus |> List.map (fun (entityName, path) -> (name, entityName, path))
            Left (name, Describe.layer3 dispatcherName properties descriptors world, filePaths)
        | LayerFromFile (name, filePath) -> Right (name, filePath)

/// Describes the layout of a screen.
type [<NoEquality; NoComparison>] ScreenLayout =
    | ScreenFromProperties of string * string * ScreenBehavior * PropertyDefinition list * LayerLayout list
    | ScreenFromLayerFile of string * ScreenBehavior * Type * string
    | ScreenFromFile of string * ScreenBehavior * string
    interface SimulantLayout

    /// Expand a screen layout to its constituent parts.
    static member expand layout world =
        match layout with
        | ScreenFromProperties (dispatcherName, name, behavior, properties, layers) ->
            let descriptorsPlusPlus = List.map (flip LayerLayout.expand world) layers
            let descriptorsPlus = Either.getLeftValues descriptorsPlusPlus
            let descriptors = descriptorsPlus |> List.map (fun (layerName, descriptor, _) -> { descriptor with LayerProperties = Map.add (Property? Name) (valueToSymbol layerName) descriptor.LayerProperties })
            let layerFilePaths = Either.getRightValues descriptorsPlusPlus |> List.map (fun (layerName, filePath) -> (name, layerName, filePath))
            let entityFilePaths = List.map (fun (_, _, filePaths) -> List.map (fun (layerName, entityName, filePath) -> (name, layerName, entityName, filePath)) filePaths) descriptorsPlus |> List.concat
            Left (name, Describe.screen3 dispatcherName properties descriptors world, behavior, layerFilePaths, entityFilePaths)
        | ScreenFromLayerFile (name, behavior, ty, filePath) -> Right (name, behavior, Some ty, filePath)
        | ScreenFromFile (name, behavior, filePath) -> Right (name, behavior, None, filePath)

/// Describes the layout of a game.
type [<NoEquality; NoComparison>] GameLayout =
    | GameFromProperties of string * PropertyDefinition list * ScreenLayout list
    | GameFromFile of string
    interface SimulantLayout

    /// Expand a game layout to its constituent parts.
    static member expand layout world =
        match layout with
        | GameFromProperties (dispatcherName, properties, screens) ->
            let descriptorsPlusPlus = List.map (flip ScreenLayout.expand world) screens
            let descriptorsPlus = Either.getLeftValues descriptorsPlusPlus
            let descriptors = List.map (fun (screenName, descriptor, _, _, _) -> { descriptor with ScreenProperties = Map.add (Property? Name) (valueToSymbol screenName) descriptor.ScreenProperties }) descriptorsPlus
            let screenBehaviors = List.map (fun (screenName, _, behavior, _, _) -> (screenName, behavior)) descriptorsPlus |> Map.ofList
            let screenFilePaths = Either.getRightValues descriptorsPlusPlus
            let layerFilePaths = List.map (fun (_, _, _, layerFilePaths, _) -> layerFilePaths) descriptorsPlus |> List.concat
            let entityFilePaths = List.map (fun (_, _, _, _, entityFilePaths) -> entityFilePaths) descriptorsPlus |> List.concat
            Left (Describe.game3 dispatcherName properties descriptors world, screenBehaviors, screenFilePaths, layerFilePaths, entityFilePaths)
        | GameFromFile filePath -> Right filePath

/// Contains primitives for describing simulant layouts.    
module Layout =

    /// Describe a game with the given properties values and contained screens.
    let game<'d when 'd :> GameDispatcher> properties children =
        GameFromProperties (typeof<'d>.Name, properties, children)

    /// Describe a screen with the given properties values and contained layers.
    let screen<'d when 'd :> ScreenDispatcher> (screen : Screen) behavior properties children =
        ScreenFromProperties (typeof<'d>.Name, screen.ScreenName, behavior, properties, children)

    /// Describe a layer with the given properties values and contained entities.
    let layer<'d when 'd :> LayerDispatcher> (layer : Layer) properties children =
        LayerFromProperties (typeof<'d>.Name, layer.LayerName, properties, children)

    /// Describe an entity with the given properties values.
    let entity<'d when 'd :> EntityDispatcher> (entity : Entity) properties =
        EntityFromProperties (typeof<'d>.Name, entity.EntityName, properties)

    /// Describe a game to be loaded from a file.
    let gameFromFile<'d when 'd :> GameDispatcher> filePath =
        GameFromFile filePath

    /// Describe a screen to be loaded from a file.
    let screenFromFile<'d when 'd :> ScreenDispatcher> (screen : Screen) behavior filePath =
        ScreenFromFile (screen.ScreenName, behavior, filePath)

    /// Describe a screen to be loaded from a file.
    let screenFromLayerFile<'d when 'd :> ScreenDispatcher> (screen : Screen) behavior filePath =
        ScreenFromLayerFile (screen.ScreenName, behavior, typeof<'d>, filePath)

    /// Describe a layer to be loaded from a file.
    let layerFromFile<'d when 'd :> LayerDispatcher> (layer : Layer) filePath =
        LayerFromFile (layer.LayerName, filePath)

    /// Describe an entity to be loaded from a file.
    let entityFromFile<'d when 'd :> EntityDispatcher> (entity : Entity) filePath =
        EntityFromFile (entity.EntityName, filePath)

type [<NoEquality; NoComparison>] View =
    | Render of RenderDescriptor
    | PlaySound of single * Audio AssetTag
    | PlaySong of int * single * Audio AssetTag
    | FadeOutSong of int
    | StopSong
    | Effect of (World -> World)

[<AutoOpen>]
module DeclarativeOperators =

    /// Pair an empty list of commands with a model.
    let inline just model = (model, [])

module Declarative =
    let Game = Game.Prop
    let Screen = Screen.Prop
    let Layer = Layer.Prop
    let Entity = Entity.Prop