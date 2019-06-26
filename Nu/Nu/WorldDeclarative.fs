// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2018.

namespace Nu
open System
open Prime
open Nu

/// Describes the behavior of a screen.
type [<NoComparison>] ScreenBehavior =
    | Vanilla
    | OmniScreen
    | Dissolve of DissolveData
    | Splash of DissolveData * SplashData * Screen

/// Describes the content of a simulant
type SimulantContent = interface end

/// Describes the content of an entity.
type [<NoEquality; NoComparison>] EntityContent =
    | EntitiesFromStream of World Lens * (obj -> EntityContent)
    | EntityFromDefinitions of string * string * PropertyInitializer list * EntityContent list
    | EntityFromFile of string * string
    interface SimulantContent

    /// Expand an entity content to its constituent parts.
    static member expand content (layer : Layer) (world : World) =
        match content with
        | EntitiesFromStream (lens, mapper) ->
            Choice1Of3 (lens, mapper)
        | EntityFromDefinitions (dispatcherName, name, initializers, content) ->
            let (descriptor, equationsEntity) = Describe.entity2 dispatcherName initializers (layer / name) world
            Choice2Of3 (name, descriptor, equationsEntity, (layer / name, content))
        | EntityFromFile (name, filePath) ->
            Choice3Of3 (name, filePath)

/// Describes the content of a layer.
and [<NoEquality; NoComparison>] LayerContent =
    | LayersFromStream of World Lens * (obj -> LayerContent)
    | LayerFromDefinitions of string * string * PropertyInitializer list * EntityContent list
    | LayerFromFile of string * string
    interface SimulantContent

    /// Expand a layer content to its constituent parts.
    static member expand content screen (world : World) =
        match content with
        | LayersFromStream (lens, mapper) ->
            Choice1Of3 (lens, mapper)
        | LayerFromDefinitions (dispatcherName, name, initializers, content) ->
            let layer = screen / name
            let expansions = List.map (fun content -> EntityContent.expand content layer world) content
            let streams = List.map (function Choice1Of3 stream -> Some (layer, fst stream, snd stream) | _ -> None) expansions |> List.definitize
            let descriptors = List.map (function Choice2Of3 (entityName, descriptor, _, _) -> Some { descriptor with EntityProperties = Map.add Property? Name (valueToSymbol entityName) descriptor.EntityProperties } | _ -> None) expansions |> List.definitize
            let equations = List.map (function Choice2Of3 (_, _, equations, _) -> Some equations | _ -> None) expansions |> List.definitize |> List.concat
            let entityContents = List.map (function Choice2Of3 (_, _, _, entityContents) -> Some entityContents | _ -> None) expansions |> List.definitize
            let filePaths = List.map (function Choice3Of3 filePath -> Some filePath | _ -> None) expansions |> List.definitize |> List.map (fun (entityName, path) -> (name, entityName, path))
            let (descriptor, equationsLayer) = Describe.layer3 dispatcherName initializers descriptors layer world
            Choice2Of3 (name, descriptor, equations @ equationsLayer, streams, filePaths, entityContents)
        | LayerFromFile (name, filePath) ->
            Choice3Of3 (name, filePath)

/// Describes the content of a screen.
and [<NoEquality; NoComparison>] ScreenContent =
    | ScreenFromDefinitions of string * string * ScreenBehavior * PropertyInitializer list * LayerContent list
    | ScreenFromLayerFile of string * ScreenBehavior * Type * string
    | ScreenFromFile of string * ScreenBehavior * string
    interface SimulantContent

    /// Expand a screen content to its constituent parts.
    static member expand content (_ : Game) world =
        match content with
        | ScreenFromDefinitions (dispatcherName, name, behavior, initializers, content) ->
            let screen = Screen name
            let expansions = List.map (fun content -> LayerContent.expand content screen world) content
            let streams = List.map (function Choice1Of3 stream -> Some (screen, fst stream, snd stream) | _ -> None) expansions |> List.definitize
            let descriptors = List.map (function Choice2Of3 (layerName, descriptor, _, _, _, _) -> Some { descriptor with LayerProperties = Map.add (Property? Name) (valueToSymbol layerName) descriptor.LayerProperties } | _ -> None) expansions |> List.definitize
            let equations = List.map (function Choice2Of3 (_, _, equations, _, _, _) -> Some equations | _ -> None) expansions |> List.definitize |> List.concat
            let entityStreams = List.map (function Choice2Of3 (_, _, _, stream, _, _) -> Some stream | _ -> None) expansions |> List.definitize |> List.concat
            let entityFilePaths = List.map (function Choice2Of3 (_, _, _, _, filePaths, _) -> Some (List.map (fun (layerName, entityName, filePath) -> (name, layerName, entityName, filePath)) filePaths) | _ -> None) expansions |> List.definitize |> List.concat
            let entityContents = List.map (function Choice2Of3 (_, _, _, _, _, entityContents) -> Some entityContents | _ -> None) expansions |> List.definitize |> List.concat
            let layerFilePaths = List.map (function Choice3Of3 (layerName, filePath) -> Some (name, layerName, filePath) | _ -> None) expansions |> List.definitize
            let (descriptor, equationsScreen) = Describe.screen3 dispatcherName initializers descriptors screen world
            Left (name, descriptor, equations @ equationsScreen, behavior, streams, entityStreams, layerFilePaths, entityFilePaths, entityContents)
        | ScreenFromLayerFile (name, behavior, ty, filePath) -> Right (name, behavior, Some ty, filePath)
        | ScreenFromFile (name, behavior, filePath) -> Right (name, behavior, None, filePath)

/// Describes the content of a game.
and [<NoEquality; NoComparison>] GameContent =
    | GameFromDefinitions of string * PropertyInitializer list * ScreenContent list
    | GameFromFile of string
    interface SimulantContent

    /// Expand a game content to its constituent parts.
    static member expand content world =
        match content with
        | GameFromDefinitions (dispatcherName, initializers, content) ->
            let game = Game ()
            let expansions = List.map (fun content -> ScreenContent.expand content game world) content
            let descriptors = Either.getLeftValues expansions |> List.map (fun (screenName, descriptor, _, _, _, _, _, _, _) -> { descriptor with ScreenProperties = Map.add (Property? Name) (valueToSymbol screenName) descriptor.ScreenProperties })
            let equations = Either.getLeftValues expansions |> List.map (fun (_, _, equations, _, _, _, _, _, _) -> equations) |> List.concat
            let layerStreams = Either.getLeftValues expansions |> List.map (fun (_, _, _, _, stream, _, _, _, _) -> stream) |> List.concat
            let entityStreams = Either.getLeftValues expansions |> List.map (fun (_, _, _, _, _, stream, _, _, _) -> stream) |> List.concat
            let screenBehaviors = Either.getLeftValues expansions |> List.map (fun (screenName, _, _,  _, behavior, _, _, _, _) -> (screenName, behavior)) |> Map.ofList
            let layerFilePaths = Either.getLeftValues expansions |> List.map (fun (_, _, _, _, _, _, layerFilePaths, _, _) -> layerFilePaths) |> List.concat
            let entityFilePaths = Either.getLeftValues expansions |> List.map (fun (_, _, _, _, _, _, _, entityFilePaths, _) -> entityFilePaths) |> List.concat
            let entityContents = Either.getLeftValues expansions |> List.map (fun (_, _, _, _, _, _, _, _, entityContents) -> entityContents) |> List.concat
            let screenFilePaths = Either.getRightValues expansions
            let (descriptor, equationsGame) = Describe.game3 dispatcherName initializers descriptors game world
            Left (descriptor, equations @ equationsGame, screenBehaviors, layerStreams, entityStreams, screenFilePaths, layerFilePaths, entityFilePaths, entityContents)
        | GameFromFile filePath -> Right filePath

type [<NoEquality; NoComparison>] View =
    | Render of RenderDescriptor
    | PlaySound of single * Audio AssetTag
    | PlaySong of int * single * Audio AssetTag
    | FadeOutSong of int
    | StopSong
    | Effect of (World -> World)

[<AutoOpen>]
module DeclarativeOperators =

    /// Pair a model with an empty list of commands.
    let inline just model = (model, [])

    /// Initialize a property.
    let init lens value = PropertyDefinition (define lens value)

    /// Equate two properties, optionally breaking any potential update cycles.
    let equate3 (left : Lens<'a, World>) (right : Lens<'a, World>) breaking =
        if right.This :> obj |> isNull
        then failwith "Equate expects an authentic Lens where its This is not null."
        else Equation (left.Name, right, breaking)

    /// Equate two properties.
    let equate left right =
        equate3 left right false

    /// Equate two properties, breaking any update cycles.
    let equateBreaking left right =
        equate3 left right true

    /// Initialize a property.
    let inline (==) left right = init left right

    /// Equate two properties.
    let inline (==>) left right = equate left right

    /// Equate two properties, breaking any update cycles.
    let inline (=/>) left right = equateBreaking left right

module Declarative =
    let Game = Game.Prop
    let Screen = Screen.Prop
    let Layer = Layer.Prop
    let Entity = Entity.Prop