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

/// Describes the content of an entity.
type [<NoEquality; NoComparison>] EntityContent =
    | EntitiesFromStream of Lens<obj, World> * (obj -> int) option * (int -> Lens<obj, World> -> Layer -> World -> EntityContent)
    | EntityFromDefinitions of string * string * PropertyInitializer list * EntityContent list
    | EntityFromFile of string * string
    interface SimulantContent

    /// Expand an entity content to its constituent parts.
    static member expand content (layer : Layer) (world : World) =
        match content with
        | EntitiesFromStream (lens, indexerOpt, mapper) ->
            Choice1Of3 (lens, indexerOpt, mapper)
        | EntityFromDefinitions (dispatcherName, name, initializers, content) ->
            let (descriptor, handlersEntity, equationsEntity) = Describe.entity4 dispatcherName initializers (layer / name) world
            Choice2Of3 (name, descriptor, handlersEntity, equationsEntity, (layer / name, content))
        | EntityFromFile (name, filePath) ->
            Choice3Of3 (name, filePath)

/// Describes the content of a layer.
type [<NoEquality; NoComparison>] LayerContent =
    | LayersFromStream of Lens<obj, World> * (obj -> int) option * (int -> Lens<obj, World> -> Screen -> World -> LayerContent)
    | LayerFromDefinitions of string * string * PropertyInitializer list * EntityContent list
    | LayerFromFile of string * string
    interface SimulantContent

    /// Expand a layer content to its constituent parts.
    static member expand content screen (world : World) =
        match content with
        | LayersFromStream (lens, indexerOpt, mapper) ->
            Choice1Of3 (lens, indexerOpt, mapper)
        | LayerFromDefinitions (dispatcherName, name, initializers, content) ->
            let layer = screen / name
            let expansions = List.map (fun content -> EntityContent.expand content layer world) content
            let streams = List.map (function Choice1Of3 (lens, indexerOpt, mapper) -> Some (layer, lens, indexerOpt, mapper) | _ -> None) expansions |> List.definitize
            let descriptors = List.map (function Choice2Of3 (entityName, descriptor, _, _, _) -> Some { descriptor with EntityProperties = Map.add Property? Name (valueToSymbol entityName) descriptor.EntityProperties } | _ -> None) expansions |> List.definitize
            let handlers = List.map (function Choice2Of3 (_, _, handlers, _, _) -> Some handlers | _ -> None) expansions |> List.definitize |> List.concat
            let equations = List.map (function Choice2Of3 (_, _, _, equations, _) -> Some equations | _ -> None) expansions |> List.definitize |> List.concat
            let entityContents = List.map (function Choice2Of3 (_, _, _, _, entityContents) -> Some entityContents | _ -> None) expansions |> List.definitize
            let filePaths = List.map (function Choice3Of3 filePath -> Some filePath | _ -> None) expansions |> List.definitize |> List.map (fun (entityName, path) -> (name, entityName, path))
            let (descriptor, handlersLayer, equationsLayer) = Describe.layer5 dispatcherName initializers descriptors layer world
            Choice2Of3 (name, descriptor, handlers @ handlersLayer, equations @ equationsLayer, streams, filePaths, entityContents)
        | LayerFromFile (name, filePath) ->
            Choice3Of3 (name, filePath)

/// Describes the content of a screen.
type [<NoEquality; NoComparison>] ScreenContent =
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
            let streams = List.map (function Choice1Of3 (lens, indexerOpt, mapper) -> Some (screen, lens, indexerOpt, mapper) | _ -> None) expansions |> List.definitize
            let descriptors = List.map (function Choice2Of3 (layerName, descriptor, _, _, _, _, _) -> Some { descriptor with LayerProperties = Map.add (Property? Name) (valueToSymbol layerName) descriptor.LayerProperties } | _ -> None) expansions |> List.definitize
            let handlers = List.map (function Choice2Of3 (_, _, handlers, _, _, _, _) -> Some handlers | _ -> None) expansions |> List.definitize |> List.concat
            let equations = List.map (function Choice2Of3 (_, _, _, equations, _, _, _) -> Some equations | _ -> None) expansions |> List.definitize |> List.concat
            let entityStreams = List.map (function Choice2Of3 (_, _, _, _, stream, _, _) -> Some stream | _ -> None) expansions |> List.definitize |> List.concat
            let entityFilePaths = List.map (function Choice2Of3 (_, _, _, _, _, filePaths, _) -> Some (List.map (fun (layerName, entityName, filePath) -> (name, layerName, entityName, filePath)) filePaths) | _ -> None) expansions |> List.definitize |> List.concat
            let entityContents = List.map (function Choice2Of3 (_, _, _, _, _, _, entityContents) -> Some entityContents | _ -> None) expansions |> List.definitize |> List.concat
            let layerFilePaths = List.map (function Choice3Of3 (layerName, filePath) -> Some (name, layerName, filePath) | _ -> None) expansions |> List.definitize
            let (descriptor, handlersScreen, equationsScreen) = Describe.screen5 dispatcherName initializers descriptors screen world
            Left (name, descriptor, handlers @ handlersScreen, equations @ equationsScreen, behavior, streams, entityStreams, layerFilePaths, entityFilePaths, entityContents)
        | ScreenFromLayerFile (name, behavior, ty, filePath) -> Right (name, behavior, Some ty, filePath)
        | ScreenFromFile (name, behavior, filePath) -> Right (name, behavior, None, filePath)

/// Describes the content of a game.
type [<NoEquality; NoComparison>] GameContent =
    | GameFromDefinitions of string * PropertyInitializer list * ScreenContent list
    | GameFromFile of string
    interface SimulantContent

    /// Expand a game content to its constituent parts.
    static member expand content world =
        match content with
        | GameFromDefinitions (dispatcherName, initializers, content) ->
            let game = Game ()
            let expansions = List.map (fun content -> ScreenContent.expand content game world) content
            let descriptors = Either.getLeftValues expansions |> List.map (fun (screenName, descriptor, _, _, _, _, _, _, _, _) -> { descriptor with ScreenProperties = Map.add (Property? Name) (valueToSymbol screenName) descriptor.ScreenProperties })
            let handlers = Either.getLeftValues expansions |> List.map (fun (_, _, handlers, _, _, _, _, _, _, _) -> handlers) |> List.concat
            let equations = Either.getLeftValues expansions |> List.map (fun (_, _, _, equations, _, _, _, _, _, _) -> equations) |> List.concat
            let layerStreams = Either.getLeftValues expansions |> List.map (fun (_, _, _, _, _, stream, _, _, _, _) -> stream) |> List.concat
            let entityStreams = Either.getLeftValues expansions |> List.map (fun (_, _, _, _, _, _, stream, _, _, _) -> stream) |> List.concat
            let screenBehaviors = Either.getLeftValues expansions |> List.map (fun (screenName, _, _,  _, _, behavior, _, _, _, _) -> (screenName, behavior)) |> Map.ofList
            let layerFilePaths = Either.getLeftValues expansions |> List.map (fun (_, _, _, _, _, _, _, layerFilePaths, _, _) -> layerFilePaths) |> List.concat
            let entityFilePaths = Either.getLeftValues expansions |> List.map (fun (_, _, _, _, _, _, _, _, entityFilePaths, _) -> entityFilePaths) |> List.concat
            let entityContents = Either.getLeftValues expansions |> List.map (fun (_, _, _, _, _, _, _, _, _, entityContents) -> entityContents) |> List.concat
            let screenFilePaths = Either.getRightValues expansions
            let (descriptor, handlersGame, equationsGame) = Describe.game5 dispatcherName initializers descriptors game world
            Left (descriptor, handlers @ handlersGame, equations @ equationsGame, screenBehaviors, layerStreams, entityStreams, screenFilePaths, layerFilePaths, entityFilePaths, entityContents)
        | GameFromFile filePath -> Right filePath

/// The output of a simulant.
/// NOTE: I almost wonder if this should be called something more abstract, like Output or something.
type [<NoEquality; NoComparison>] View =
    | Render of RenderDescriptor
    | PlaySound of single * Audio AssetTag
    | PlaySong of int * single * Audio AssetTag
    | FadeOutSong of int
    | StopSong
    | Effect of (World -> World)

/// Opens up some functions to make simulant lenses more accessible.
module Declarative =

    let Game = Game.Prop
    let Screen = Screen.Prop
    let Layer = Layer.Prop
    let Entity = Entity.Prop

[<AutoOpen>]
module DeclarativeOperators =

    /// Make a lens.
    let lens<'a> name get set this =
        Lens.make<'a, World> name get set this

    /// Make a read-only lens.
    let lensReadOnly<'a> name get this =
        Lens.makeReadOnly<'a, World> name get this

    /// Initialize a property.
    let set lens value =
        PropertyDefinition (define lens value)

    /// Equate two properties, optionally breaking any potential update cycles.
    /// HACK: equateByName allows the use of fake lenses in declarative usage.
    /// NOTE: the downside to using fake lenses is that composed fakse lenses do not function.
    let equateByName (left : Lens<'a, World>) (right : Lens<'a, World>) breaking =
        if right.This :> obj |> isNull
        then failwith "Equate expects an authentic right lens (where its This field is not null)."
        else EquationDefinition (left.Name, right, breaking)

    /// Initialize a property.
    let inline (==) left right = set left right

    /// Equate two properties by name.
    let inline (<==) left right = equateByName left right false

    /// Equate two properties by name, breaking any update cycles.
    let inline (</==) left right = equateByName left right true

[<AutoOpen>]
module WorldDeclarative =

    let mutable internal LensError = false

    type World with

        /// Transform a stream into existing simulants.
        static member streamSimulants
            (lensSeq : Lens<obj seq, World>)
            (mapper : int -> Lens<obj, World> -> Simulant -> World -> SimulantContent)
            (origin : ContentOrigin)
            (parent : Simulant)
            (stream : Stream<Lens<(int * obj) option, World> seq, World>) =
            stream |>
            Stream.insert Gen.id |>
            Stream.mapWorld (fun (guid, lenses) world ->
                lenses |>
                Seq.map (fun lens -> (lens.Get world, { Lens.dereference lens with Validate = fun world -> Option.isSome (lens.Get world) })) |>
                Seq.filter (fst >> Option.isSome) |>
                Seq.take (Lens.get lensSeq world |> Seq.length) |>
                Seq.map (fun (opt, lens) ->
                    let (index, _) = Option.get opt
                    let guid = Gen.idDeterministic index guid
                    let lens = lens.Map snd
                    PartialComparable.make guid (index, lens)) |>
                Set.ofSeq) |>
            Stream.fold (fun (p, _, _) c ->
                (c, Set.difference c p, Set.difference p c))
                (Set.empty, Set.empty, Set.empty) |>
            Stream.optimizeBy
                Triple.fst |>
            Stream.mapEffect (fun evt world ->
                let (current, added, removed) = evt.Data
                let world =
                    Seq.fold (fun world guidAndContent ->
                        let (guid, (index, lens)) = PartialComparable.unmake guidAndContent
                        let content = mapper index lens parent world
                        match World.tryGetKeyedValue (scstring guid) world with
                        | None -> WorldModule.expandContent Unchecked.defaultof<_> (Some guid) content origin parent world
                        | Some _ -> world)
                        world added
                let world =
                    Seq.fold (fun world guidAndContent ->
                        let (guid, _) = PartialComparable.unmake guidAndContent
                        match World.tryGetKeyedValue (scstring guid) world with
                        | Some simulant ->
                            let world = World.removeKeyedValue (scstring guid) world
                            WorldModule.destroy simulant world
                        | None -> failwithumf ())
                        world removed
                (current, world))

        /// Turn an entity stream into a series of live simulants.
        static member expandSimulantStream (lens : Lens<obj, World>) indexerOpt mapper origin parent world =
            let lensSeq = Lens.map Reflection.objToObjSeq lens
            Stream.make (Events.Register --> lens.This.SimulantAddress) |>
            Stream.sum (Stream.make lens.ChangeEvent) |>
            Stream.map (fun _ -> Lens.explodeIndexedOpt indexerOpt lensSeq) |>
            World.streamSimulants lensSeq mapper origin parent |>
            Stream.subscribe (fun _ value -> value) Default.Game $ world