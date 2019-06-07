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
      ScreenNameOpt : string option
      ScreenProperties : Map<string, Symbol>
      Layers : LayerDescriptor list }

    /// The empty screen descriptor.
    static member empty =
        { ScreenDispatcherName = String.Empty
          ScreenNameOpt = None
          ScreenProperties = Map.empty
          Layers = [] }
          
    interface SimulantDescriptor with
        member this.Children =
            this.Layers |> enumerable<SimulantDescriptor> |> List.ofSeq

/// Describes a layer value independent of the engine.
and [<NoComparison>] LayerDescriptor =
    { LayerDispatcherName : string
      LayerNameOpt : string option
      LayerProperties : Map<string, Symbol>
      Entities : EntityDescriptor list }

    /// The empty layer descriptor.
    static member empty =
        { LayerDispatcherName = String.Empty
          LayerNameOpt = None
          LayerProperties = Map.empty
          Entities = [] }

    interface SimulantDescriptor with
        member this.Children =
            this.Entities |> enumerable<SimulantDescriptor> |> List.ofSeq

/// Describes an entity value independent of the engine.
and [<NoComparison>] EntityDescriptor =
    { EntityDispatcherName : string
      EntityNameOpt : string option
      EntityProperties : Map<string, Symbol> }

    /// The empty entity descriptor.
    static member empty =
        { EntityDispatcherName = String.Empty
          EntityNameOpt = None
          EntityProperties = Map.empty }

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
    let screen4 dispatcherName screenNameOpt (properties : (World PropertyTag * obj) seq) (layers : LayerDescriptor seq) =
        let properties = properties |> Seq.map (fun (tag : World PropertyTag, value) -> (tag.Name, valueToSymbol value)) |> Map.ofSeq
        { ScreenDispatcherName = dispatcherName
          ScreenNameOpt = screenNameOpt
          ScreenProperties = properties
          Layers = List.ofSeq layers }

    /// Describe a screen with the given properties values and contained layers.
    let screen<'d when 'd :> ScreenDispatcher> screenNameOpt properties layers =
        screen4 typeof<'d>.Name screenNameOpt properties layers

    /// Describe a layer with the given properties values and contained entities.
    let layer4 dispatcherName layerNameOpt (properties : (World PropertyTag * obj) seq) (entities : EntityDescriptor seq) =
        let properties = properties |> Seq.map (fun (tag : World PropertyTag, value) -> (tag.Name, valueToSymbol value)) |> Map.ofSeq
        { LayerDispatcherName = dispatcherName
          LayerNameOpt = layerNameOpt
          LayerProperties = properties
          Entities = List.ofSeq entities }

    /// Describe a layer with the given properties values and contained entities.
    let layer<'d when 'd :> LayerDispatcher> layerNameOpt properties entities =
        layer4 typeof<'d>.Name layerNameOpt properties entities

    /// Describe an entity with the given properties values.
    let entity3 dispatcherName nameOpt (properties : (World PropertyTag * obj) seq) =
        let properties = properties |> Seq.map (fun (tag : World PropertyTag, value) -> (tag.Name, valueToSymbol value)) |> Map.ofSeq
        { EntityDispatcherName = dispatcherName
          EntityNameOpt = nameOpt
          EntityProperties = properties }

    /// Describe an entity with the given properties values.
    let entity<'d when 'd :> EntityDispatcher> nameOpt properties =
        entity3 typeof<'d>.Name nameOpt properties

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
    | EntityFromDescriptor of string * string * (World PropertyTag * obj) list
    | EntityFromFile of string * string
    interface SimulantView

    /// Expand an entity view to its constituent parts.
    static member expand entityView =
        match entityView with
        | EntityFromDescriptor (dispatcherName, name, properties) -> Left (Describe.entity3 dispatcherName (Some name) properties)
        | EntityFromFile (name, filePath) -> Right (name, filePath)

/// Describes the view for a layer.
type [<NoComparison>] LayerView =
    | LayerFromDescriptor of string * string * (World PropertyTag * obj) list * EntityView list
    | LayerFromFile of string * string
    interface SimulantView
    
    /// Expand a layer view to its constituent parts.
    static member expand layerView =
        match layerView with
        | LayerFromDescriptor (dispatcherName, name, properties, entities) ->
            let entitiesAndFilePaths = List.map EntityView.expand entities
            let entityDescriptors = Either.getLeftValues entitiesAndFilePaths []
            let entityFilePaths = Either.getRightValues entitiesAndFilePaths [] |> List.map (fun (entityName, path) -> (name, entityName, path))
            Left (Describe.layer4 dispatcherName (Some name) properties entityDescriptors, entityFilePaths)
        | LayerFromFile (name, filePath) -> Right (name, filePath)

/// Describes the view for a screen.
type [<NoComparison>] ScreenView =
    | ScreenFromDescriptor of string * string * ScreenBehavior * (World PropertyTag * obj) list * LayerView list
    | ScreenFromLayerFile of string * ScreenBehavior * Type * string
    | ScreenFromFile of string * ScreenBehavior * string
    interface SimulantView

    /// Expand a screen view to its constituent parts.
    static member expand screenView =
        match screenView with
        | ScreenFromDescriptor (dispatcherName, name, behavior, properties, layers) ->
            let descriptorsPlusPlus = List.map LayerView.expand layers
            let descriptorsPlus = Either.getLeftValues descriptorsPlusPlus []
            let layerFilePaths = Either.getRightValues descriptorsPlusPlus [] |> List.map (fun (layerName, filePath) -> (name, layerName, filePath))
            let entityFilePaths = List.map (fun (_, filePaths) -> List.map (fun (layerName, entityName, filePath) -> (name, layerName, entityName, filePath)) filePaths) descriptorsPlus |> List.concat
            let descriptors = descriptorsPlus |> List.map fst
            Left (Describe.screen4 dispatcherName (Some name) properties descriptors, behavior, layerFilePaths, entityFilePaths)
        | ScreenFromLayerFile (name, behavior, ty, filePath) -> Right (name, behavior, Some ty, filePath)
        | ScreenFromFile (name, behavior, filePath) -> Right (name, behavior, None, filePath)

/// Describes the view for a game.
type [<NoComparison>] GameView =
    | GameFromDescriptor of string * (World PropertyTag * obj) list * ScreenView list
    | GameFromFile of string
    interface SimulantView

    /// Expand a game view to its constituent parts.
    static member expand gameView =
        match gameView with
        | GameFromDescriptor (dispatcherName, properties, screens) ->
            let descriptorsPlusPlus = List.map ScreenView.expand screens
            let descriptorsPlus = Either.getLeftValues descriptorsPlusPlus []
            let filePaths = Either.getRightValues descriptorsPlusPlus []
            let layerFilePaths = List.map (fun (_, _, layerFilePaths, _) -> layerFilePaths) descriptorsPlus
            let entityFilePaths = List.map (fun (_, _, _, entityFilePaths) -> entityFilePaths) descriptorsPlus
            let descriptors = List.map (fun (descriptor, _, _, _) -> descriptor) descriptorsPlus
            Left (Describe.game3 dispatcherName properties descriptors, filePaths, layerFilePaths, entityFilePaths)
        | GameFromFile filePath -> Right filePath

/// Contains primitives for describing simulant views.    
module View =

    /// Describe a game with the given properties values and contained screens.
    let game<'d when 'd :> GameDispatcher> properties children =
        GameFromDescriptor (typeof<'d>.Name, properties, children)

    /// Describe a screen with the given properties values and contained layers.
    let screen<'d when 'd :> ScreenDispatcher> screenName behavior properties children =
        ScreenFromDescriptor (typeof<'d>.Name, screenName, behavior, properties, children)

    /// Describe a layer with the given properties values and contained entities.
    let layer<'d when 'd :> LayerDispatcher> layerName properties children =
        LayerFromDescriptor (typeof<'d>.Name, layerName, properties, children)

    /// Describe an entity with the given properties values.
    let entity<'d when 'd :> EntityDispatcher> entityName properties =
        EntityFromDescriptor (typeof<'d>.Name, entityName, properties)

    /// Describe a game to be loaded from a file.
    let gameFromFile<'d when 'd :> GameDispatcher> filePath =
        GameFromFile filePath

    /// Describe a screen to be loaded from a file.
    let screenFromFile<'d when 'd :> ScreenDispatcher> screenName behavior filePath =
        ScreenFromFile (screenName, behavior, filePath)

    /// Describe a screen to be loaded from a file.
    let screenFromLayerFile<'d when 'd :> ScreenDispatcher> screenName behavior filePath =
        ScreenFromLayerFile (screenName, behavior, typeof<'d>, filePath)

    /// Describe a layer to be loaded from a file.
    let layerFromFile<'d when 'd :> LayerDispatcher> layerName filePath =
        LayerFromFile (layerName, filePath)

    /// Describe an entity to be loaded from a file.
    let entityFromFile<'d when 'd :> EntityDispatcher> entityName filePath =
        EntityFromFile (entityName, filePath)

[<AutoOpen>]
module WorldModelDeclarative =

    /// Create a property declaration.
    let prop (p : PropertyTag<'a, World>) (v : 'a) =
        (p :> PropertyTag<World>, v :> obj)

    /// Pair an empty list of commands with a model.
    let inline just model = (model, [])