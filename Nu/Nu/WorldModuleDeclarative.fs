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
    let game3 dispatcherName (properties : (World PropertyTag * obj) seq) (screens : ScreenDescriptor seq) =
        let properties = properties |> Seq.map (fun (tag : World PropertyTag, value) -> (tag.Name, valueToSymbol value)) |> Map.ofSeq
        { GameDispatcherName = dispatcherName
          GameProperties = properties
          Screens = List.ofSeq screens }

    /// Describe a game with the given properties values and contained screens.
    let game<'d when 'd :> GameDispatcher> properties screens =
        game3 typeof<'d>.Name properties screens

    /// Describe a screen with the given properties values and contained layers.
    let screen3 dispatcherName (properties : (World PropertyTag * obj) seq) (layers : LayerDescriptor seq) =
        let properties = properties |> Seq.map (fun (tag : World PropertyTag, value) -> (tag.Name, valueToSymbol value)) |> Map.ofSeq
        { ScreenDispatcherName = dispatcherName
          ScreenProperties = properties
          Layers = List.ofSeq layers }

    /// Describe a screen with the given properties values and contained layers.
    let screen<'d when 'd :> ScreenDispatcher> properties layers =
        screen3 typeof<'d>.Name properties layers

    /// Describe a layer with the given properties values and contained entities.
    let layer3 dispatcherName (properties : (World PropertyTag * obj) seq) (entities : EntityDescriptor seq) =
        let properties = properties |> Seq.map (fun (tag : World PropertyTag, value) -> (tag.Name, valueToSymbol value)) |> Map.ofSeq
        { LayerDispatcherName = dispatcherName
          LayerProperties = properties
          Entities = List.ofSeq entities }

    /// Describe a layer with the given properties values and contained entities.
    let layer<'d when 'd :> LayerDispatcher> properties entities =
        layer3 typeof<'d>.Name properties entities

    /// Describe an entity with the given properties values.
    let entity2 dispatcherName (properties : (World PropertyTag * obj) seq) =
        let properties = properties |> Seq.map (fun (tag : World PropertyTag, value) -> (tag.Name, valueToSymbol value)) |> Map.ofSeq
        { EntityDispatcherName = dispatcherName
          EntityProperties = properties }

    /// Describe an entity with the given properties values.
    let entity<'d when 'd :> EntityDispatcher> properties =
        entity2 typeof<'d>.Name properties

/// Describes the behavior of a screen.
type [<NoComparison>] ScreenBehavior =
    | Vanilla
    | Omniscreen
    | Dissolve of DissolveData
    | Splash of DissolveData * SplashData * Screen

/// Describes the view for a simulant
type SimulantView = interface end

/// Describes the view for an entity.
type [<NoComparison>] EntityView =
    | EntityFromProperties of string * string * (World PropertyTag * obj) list
    | EntityFromFile of string * string
    interface SimulantView

    /// Expand an entity view to its constituent parts.
    static member expand entityView =
        match entityView with
        | EntityFromProperties (dispatcherName, name, properties) -> Left (name, Describe.entity2 dispatcherName properties)
        | EntityFromFile (name, filePath) -> Right (name, filePath)

/// Describes the view for a layer.
type [<NoComparison>] LayerView =
    | LayerFromProperties of string * string * (World PropertyTag * obj) list * EntityView list
    | LayerFromFile of string * string
    interface SimulantView
    
    /// Expand a layer view to its constituent parts.
    static member expand layerView =
        match layerView with
        | LayerFromProperties (dispatcherName, name, properties, entities) ->
            let descriptorsPlus = List.map EntityView.expand entities
            let descriptors = Either.getLeftValues descriptorsPlus [] |> List.map (fun (entityName, descriptor) -> { descriptor with EntityProperties = Map.add (Property? Name) (valueToSymbol entityName) descriptor.EntityProperties })
            let filePaths = Either.getRightValues descriptorsPlus [] |> List.map (fun (entityName, path) -> (name, entityName, path))
            Left (name, Describe.layer3 dispatcherName properties descriptors, filePaths)
        | LayerFromFile (name, filePath) -> Right (name, filePath)

/// Describes the view for a screen.
type [<NoComparison>] ScreenView =
    | ScreenFromProperties of string * string * ScreenBehavior * (World PropertyTag * obj) list * LayerView list
    | ScreenFromLayerFile of string * ScreenBehavior * Type * string
    | ScreenFromFile of string * ScreenBehavior * string
    interface SimulantView

    /// Expand a screen view to its constituent parts.
    static member expand screenView =
        match screenView with
        | ScreenFromProperties (dispatcherName, name, behavior, properties, layers) ->
            let descriptorsPlusPlus = List.map LayerView.expand layers
            let descriptorsPlus = Either.getLeftValues descriptorsPlusPlus []
            let descriptors = descriptorsPlus |> List.map (fun (layerName, descriptor, _) -> { descriptor with LayerProperties = Map.add (Property? Name) (valueToSymbol layerName) descriptor.LayerProperties })
            let layerFilePaths = Either.getRightValues descriptorsPlusPlus [] |> List.map (fun (layerName, filePath) -> (name, layerName, filePath))
            let entityFilePaths = List.map (fun (_, _, filePaths) -> List.map (fun (layerName, entityName, filePath) -> (name, layerName, entityName, filePath)) filePaths) descriptorsPlus |> List.concat
            Left (name, Describe.screen3 dispatcherName properties descriptors, behavior, layerFilePaths, entityFilePaths)
        | ScreenFromLayerFile (name, behavior, ty, filePath) -> Right (name, behavior, Some ty, filePath)
        | ScreenFromFile (name, behavior, filePath) -> Right (name, behavior, None, filePath)

/// Describes the view for a game.
type [<NoComparison>] GameView =
    | GameFromProperties of string * (World PropertyTag * obj) list * ScreenView list
    | GameFromFile of string
    interface SimulantView

    /// Expand a game view to its constituent parts.
    static member expand gameView =
        match gameView with
        | GameFromProperties (dispatcherName, properties, screens) ->
            let descriptorsPlusPlus = List.map ScreenView.expand screens
            let descriptorsPlus = Either.getLeftValues descriptorsPlusPlus []
            let descriptors = List.map (fun (screenName, descriptor, _, _, _) -> { descriptor with ScreenProperties = Map.add (Property? Name) (valueToSymbol screenName) descriptor.ScreenProperties }) descriptorsPlus
            let screenBehaviors = List.map (fun (screenName, _, behavior, _, _) -> (screenName, behavior)) descriptorsPlus |> Map.ofList
            let screenFilePaths = Either.getRightValues descriptorsPlusPlus []
            let layerFilePaths = List.map (fun (_, _, _, layerFilePaths, _) -> layerFilePaths) descriptorsPlus |> List.concat
            let entityFilePaths = List.map (fun (_, _, _, _, entityFilePaths) -> entityFilePaths) descriptorsPlus |> List.concat
            Left (Describe.game3 dispatcherName properties descriptors, screenBehaviors, screenFilePaths, layerFilePaths, entityFilePaths)
        | GameFromFile filePath -> Right filePath

/// Contains primitives for describing simulant views.    
module View =

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

[<AutoOpen>]
module WorldModelDeclarative =

    /// Create a property declaration.
    let prop (p : PropertyTag<'a, World>) (v : 'a) =
        (p :> PropertyTag<World>, v :> obj)

    /// Pair an empty list of commands with a model.
    let inline just model = (model, [])