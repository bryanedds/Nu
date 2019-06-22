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
    | EntityFromDefinitions of string * string * DescriptiveDefinition list
    | EntityFromFile of string * string
    interface SimulantContent

    /// Expand an entity content to its constituent parts.
    static member expand content (layer : Layer) (world : World) =
        match content with
        | EntitiesFromStream (lens, mapper) ->
            Choice1Of3 (lens, mapper)
        | EntityFromDefinitions (dispatcherName, name, definitions) ->
            let (descriptor, definitions) = Describe.entity2 dispatcherName definitions (layer => name) world
            Choice2Of3 (name, descriptor, definitions)
        | EntityFromFile (name, filePath) ->
            Choice3Of3 (name, filePath)

/// Describes the content of a layer.
and [<NoEquality; NoComparison>] LayerContent =
    | LayerFromDefinitions of string * string * DescriptiveDefinition list * EntityContent list
    | LayerFromFile of string * string
    interface SimulantContent
    
    /// Expand a layer content to its constituent parts.
    static member expand content screen (world : World) =
        match content with
        | LayerFromDefinitions (dispatcherName, name, definitions, entities) ->
            let layer = screen => name
            let expansions = List.map (fun content -> EntityContent.expand content layer world) entities
            let streams = List.map (function Choice1Of3 stream -> Some (layer, fst stream, snd stream) | _ -> None) expansions |> List.definitize
            let (descriptors, equations) =
                List.map (function Choice2Of3 dae -> Some dae | _ -> None) expansions |>
                List.definitize |>
                List.map (fun (entityName, descriptor, equation) ->
                    let entityProperties = Map.add Property? Name (valueToSymbol entityName) descriptor.EntityProperties
                    let descriptor = { descriptor with EntityProperties = entityProperties }
                    (descriptor, equation)) |>
                List.unzip |>
                mapSnd List.concat
            let filePaths = List.map (function Choice3Of3 filePath -> Some filePath | _ -> None) expansions |> List.definitize |> List.map (fun (entityName, path) -> (name, entityName, path))
            let (descriptor, equationsLayer) = Describe.layer3 dispatcherName definitions descriptors layer world
            let equationsAll = equations @ equationsLayer
            Left (name, descriptor, equationsAll, streams, filePaths)
        | LayerFromFile (name, filePath) -> Right (name, filePath)

/// Describes the content of a screen.
and [<NoEquality; NoComparison>] ScreenContent =
    | ScreenFromDefinitions of string * string * ScreenBehavior * DescriptiveDefinition list * LayerContent list
    | ScreenFromLayerFile of string * ScreenBehavior * Type * string
    | ScreenFromFile of string * ScreenBehavior * string
    interface SimulantContent

    /// Expand a screen content to its constituent parts.
    static member expand content (_ : Game) world =
        match content with
        | ScreenFromDefinitions (dispatcherName, name, behavior, definitions, layers) ->
            let screen = Screen name
            let expansions = List.map (fun content -> LayerContent.expand content screen world) layers
            let descriptors = Either.getLeftValues expansions |> List.map (fun (layerName, descriptor, _, _, _) -> { descriptor with LayerProperties = Map.add (Property? Name) (valueToSymbol layerName) descriptor.LayerProperties })
            let equations = Either.getLeftValues expansions |> List.map (fun (_, _, equations, _, _) -> equations) |> List.concat
            let entityStreams = Either.getLeftValues expansions |> List.map (fun (_, _, _, stream, _) -> stream) |> List.concat
            let entityFilePaths = Either.getLeftValues expansions |> List.map (fun (_, _, _, _, filePaths) -> List.map (fun (layerName, entityName, filePath) -> (name, layerName, entityName, filePath)) filePaths) |> List.concat
            let layerFilePaths = Either.getRightValues expansions |> List.map (fun (layerName, filePath) -> (name, layerName, filePath))
            let (descriptor, equationsScreen) = Describe.screen3 dispatcherName definitions descriptors screen world
            let equationsAll = equations @ equationsScreen
            Left (name, descriptor, equationsAll, behavior, entityStreams, layerFilePaths, entityFilePaths)
        | ScreenFromLayerFile (name, behavior, ty, filePath) -> Right (name, behavior, Some ty, filePath)
        | ScreenFromFile (name, behavior, filePath) -> Right (name, behavior, None, filePath)

/// Describes the content of a game.
and [<NoEquality; NoComparison>] GameContent =
    | GameFromDefinitions of string * DescriptiveDefinition list * ScreenContent list
    | GameFromFile of string
    interface SimulantContent

    /// Expand a game content to its constituent parts.
    static member expand content world =
        match content with
        | GameFromDefinitions (dispatcherName, definitions, screens) ->
            let game = Game ()
            let expansions = List.map (fun content -> ScreenContent.expand content game world) screens
            let descriptors = Either.getLeftValues expansions |> List.map (fun (screenName, descriptor, _, _, _, _, _) -> { descriptor with ScreenProperties = Map.add (Property? Name) (valueToSymbol screenName) descriptor.ScreenProperties })
            let equations = Either.getLeftValues expansions |> List.map (fun (_, _, equations, _, _, _, _) -> equations) |> List.concat
            let entityStreams = Either.getLeftValues expansions |> List.map (fun (_, _, _, _, stream, _, _) -> stream) |> List.concat
            let screenBehaviors = Either.getLeftValues expansions |> List.map (fun (screenName, _,  _, behavior, _, _, _) -> (screenName, behavior)) |> Map.ofList
            let entityFilePaths = Either.getLeftValues expansions |> List.map (fun (_, _, _, _, _, _, entityFilePaths) -> entityFilePaths) |> List.concat
            let layerFilePaths = Either.getLeftValues expansions |> List.map (fun (_, _, _, _, _, layerFilePaths, _) -> layerFilePaths) |> List.concat
            let screenFilePaths = Either.getRightValues expansions
            let (descriptor, equationsGame) = Describe.game3 dispatcherName definitions descriptors game world
            let equationsAll = equations @ equationsGame
            Left (descriptor, equationsAll, screenBehaviors, entityStreams, screenFilePaths, layerFilePaths, entityFilePaths)
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

    /// Pair an empty list of commands with a model.
    let inline just model = (model, [])

    /// Declare an instruction to set a property.
    let init lens value = PropertyDefinition (define lens value)

    /// Declare an instruction to equate two properties, optionally breaking any potential update cycles.
    let equate3 (left : Lens<'a, World>) (right : Lens<'a, World>) breaking =
        if right.This :> obj |> isNull
        then failwith "Equate expects an authentic Lens where its This is not null."
        else Equate (left.Name, right, breaking)

    /// Declare an instruction to equate two properties.
    let equate left right =
        equate3 left right false

    /// Declare an instruction to equate two properties, breaking any potential update cycles.
    let equateBreaking left right =
        equate3 left right true

    /// Declare an instruction to set a property.
    let inline (===) left right = init left right

    /// Declare an instruction to equate two properties.
    let inline (=|=) left right = equate3 left right false

    /// Declare an instruction to equate two properties, breaking any potential update cycles.
    let inline (=/=) left right = equate3 left right true

module Declarative =
    let Game = Game.Prop
    let Screen = Screen.Prop
    let Layer = Layer.Prop
    let Entity = Entity.Prop