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
    | EntitiesFromStream of Lens<obj, World> * (obj -> int) option * (int -> Lens<obj, World> -> World -> EntityContent)
    | EntityFromDefinitions of string * string * PropertyInitializer list * EntityContent list
    | EntityFromFile of string * string
    interface SimulantContent

    /// Expand an entity content to its constituent parts.
    static member expand content (layer : Layer) (world : World) =
        match content with
        | EntitiesFromStream (lens, indexerOpt, mapper) ->
            Choice1Of3 (lens, indexerOpt, mapper)
        | EntityFromDefinitions (dispatcherName, name, initializers, content) ->
            let (snapshot, handlersEntity, fixesEntity) = Describe.entity4 dispatcherName initializers (layer / name) world
            Choice2Of3 (name, snapshot, handlersEntity, fixesEntity, (layer / name, content))
        | EntityFromFile (name, filePath) ->
            Choice3Of3 (name, filePath)

/// Describes the content of a layer.
type [<NoEquality; NoComparison>] LayerContent =
    | LayersFromStream of Lens<obj, World> * (obj -> int) option * (int -> Lens<obj, World> -> World -> LayerContent)
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
            let snapshots = List.map (function Choice2Of3 (entityName, snapshot, _, _, _) -> Some { snapshot with SimulantNameOpt = Some entityName } | _ -> None) expansions |> List.definitize
            let handlers = List.map (function Choice2Of3 (_, _, handlers, _, _) -> Some handlers | _ -> None) expansions |> List.definitize |> List.concat
            let fixes = List.map (function Choice2Of3 (_, _, _, fixes, _) -> Some fixes | _ -> None) expansions |> List.definitize |> List.concat
            let entityContents = List.map (function Choice2Of3 (_, _, _, _, entityContents) -> Some entityContents | _ -> None) expansions |> List.definitize
            let filePaths = List.map (function Choice3Of3 filePath -> Some filePath | _ -> None) expansions |> List.definitize |> List.map (fun (entityName, path) -> (name, entityName, path))
            let (snapshot, handlersLayer, fixesLayer) = Describe.layer5 dispatcherName initializers snapshots layer world
            Choice2Of3 (name, snapshot, handlers @ handlersLayer, fixes @ fixesLayer, streams, filePaths, entityContents)
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
            let snapshots = List.map (function Choice2Of3 (layerName, snapshot, _, _, _, _, _) -> Some { snapshot with SimulantNameOpt = Some layerName } | _ -> None) expansions |> List.definitize
            let handlers = List.map (function Choice2Of3 (_, _, handlers, _, _, _, _) -> Some handlers | _ -> None) expansions |> List.definitize |> List.concat
            let fixes = List.map (function Choice2Of3 (_, _, _, fixes, _, _, _) -> Some fixes | _ -> None) expansions |> List.definitize |> List.concat
            let entityStreams = List.map (function Choice2Of3 (_, _, _, _, stream, _, _) -> Some stream | _ -> None) expansions |> List.definitize |> List.concat
            let entityFilePaths = List.map (function Choice2Of3 (_, _, _, _, _, filePaths, _) -> Some (List.map (fun (layerName, entityName, filePath) -> (name, layerName, entityName, filePath)) filePaths) | _ -> None) expansions |> List.definitize |> List.concat
            let entityContents = List.map (function Choice2Of3 (_, _, _, _, _, _, entityContents) -> Some entityContents | _ -> None) expansions |> List.definitize |> List.concat
            let layerFilePaths = List.map (function Choice3Of3 (layerName, filePath) -> Some (name, layerName, filePath) | _ -> None) expansions |> List.definitize
            let (snapshot, handlersScreen, fixesScreen) = Describe.screen5 dispatcherName initializers snapshots screen world
            Left (name, snapshot, handlers @ handlersScreen, fixes @ fixesScreen, behavior, streams, entityStreams, layerFilePaths, entityFilePaths, entityContents)
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
            let snapshots = Either.getLeftValues expansions |> List.map (fun (screenName, snapshot, _, _, _, _, _, _, _, _) -> { snapshot with SimulantNameOpt = Some screenName })
            let handlers = Either.getLeftValues expansions |> List.map (fun (_, _, handlers, _, _, _, _, _, _, _) -> handlers) |> List.concat
            let fixes = Either.getLeftValues expansions |> List.map (fun (_, _, _, fixes, _, _, _, _, _, _) -> fixes) |> List.concat
            let layerStreams = Either.getLeftValues expansions |> List.map (fun (_, _, _, _, _, stream, _, _, _, _) -> stream) |> List.concat
            let entityStreams = Either.getLeftValues expansions |> List.map (fun (_, _, _, _, _, _, stream, _, _, _) -> stream) |> List.concat
            let screenBehaviors = Either.getLeftValues expansions |> List.map (fun (screenName, _, _,  _, _, behavior, _, _, _, _) -> (screenName, behavior)) |> Map.ofList
            let layerFilePaths = Either.getLeftValues expansions |> List.map (fun (_, _, _, _, _, _, _, layerFilePaths, _, _) -> layerFilePaths) |> List.concat
            let entityFilePaths = Either.getLeftValues expansions |> List.map (fun (_, _, _, _, _, _, _, _, entityFilePaths, _) -> entityFilePaths) |> List.concat
            let entityContents = Either.getLeftValues expansions |> List.map (fun (_, _, _, _, _, _, _, _, _, entityContents) -> entityContents) |> List.concat
            let screenFilePaths = Either.getRightValues expansions
            let (snapshot, handlersGame, fixesGame) = Describe.game5 dispatcherName initializers snapshots game world
            Left (snapshot, handlers @ handlersGame, fixes @ fixesGame, screenBehaviors, layerStreams, entityStreams, screenFilePaths, layerFilePaths, entityFilePaths, entityContents)
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

    let Game = Game.Lens
    let Screen = Screen.Lens
    let Layer = Layer.Lens
    let Entity = Entity.Lens

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

    /// Initialize a property.
    let inline (==) left right =
        set left right

    /// Fix the left property to the value of the right.
    /// HACK: fix3 allows the use of fake lenses in declarative usage.
    /// NOTE: the downside to using fake lenses is that composed fake lenses do not function.
    let fix3 (left : Lens<'a, World>) (right : Lens<'a, World>) breaking =
        if right.This :> obj |> isNull
        then failwith "Equate expects an authentic right lens (where its This field is not null)."
        else FixDefinition (left, right, breaking)

    /// Fix the left property to the value of the right.
    let inline (<==) left right =
        fix3 left right false

    /// Fix the left property to the value of the right, breaking any update cycles.
    let inline (</==) left right =
        fix3 left right true

[<AutoOpen>]
module WorldDeclarative =

    let mutable internal LensError = false

    type World with

        /// Transform a stream into existing simulants.
        static member streamSimulants
            (lensSeq : Lens<obj seq, World>)
            (mapper : int -> Lens<obj, World> -> World -> SimulantContent)
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
                        let content = mapper index lens world
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