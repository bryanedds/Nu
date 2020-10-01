// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu
open System
open System.Collections
open Prime
open Nu

/// Describes the behavior of a screen.
type [<StructuralEquality; NoComparison>] ScreenBehavior =
    | Vanilla
    | Dissolve  of DissolveDescriptor * SongDescriptor option
    | Splash of DissolveDescriptor * SplashDescriptor * Screen
    | OmniScreen

/// Describes the content of an entity.
type [<NoEquality; NoComparison>] EntityContent =
    | EntitiesFromStream of Lens<obj, World> * (obj -> obj) * (obj -> World -> obj seq) * (obj -> int) option * (int -> Lens<obj, World> -> World -> EntityContent)
    | EntityFromInitializers of string * string * PropertyInitializer list * EntityContent list
    | EntityFromFile of string * string * string * PropertyInitializer list * EntityContent list
    interface SimulantContent

    /// Expand an entity content to its constituent parts.
    static member expand content (layer : Layer) (world : World) =
        match content with
        | EntitiesFromStream (lens, sieve, unfold, indexOpt, mapper) ->
            Left (lens, sieve, unfold, indexOpt, mapper)
        | EntityFromInitializers (dispatcherName, name, initializers, content) ->
            let (descriptor, handlersEntity, bindsEntity) = Describe.entity4 dispatcherName (Some name) initializers (layer / name) world
            Right (name, Left descriptor, handlersEntity, bindsEntity, (layer / name, content))
        | EntityFromFile (dispatcherName, name, filePath, initializers, content) ->
            let (_, handlersEntity, bindsEntity) = Describe.entity4 dispatcherName (Some name) initializers (layer / name) world
            Right (name, Right filePath, handlersEntity, bindsEntity, (layer / name, content))

/// Describes the content of a layer.
type [<NoEquality; NoComparison>] LayerContent =
    | LayersFromStream of Lens<obj, World> * (obj -> obj) * (obj -> World -> obj seq) * (obj -> int) option * (int -> obj -> World -> LayerContent)
    | LayerFromInitializers of string * string * PropertyInitializer list * EntityContent list
    | LayerFromFile of string * string * string * PropertyInitializer list * EntityContent list
    interface SimulantContent

    /// Expand a layer content to its constituent parts.
    static member expand content screen (world : World) =
        match content with
        | LayersFromStream (lens, sieve, unfold, indexOpt, mapper) ->
            Left (lens, sieve, unfold, indexOpt, mapper)
        | LayerFromInitializers (dispatcherName, name, initializers, content) ->
            let layer = screen / name
            let expansions = List.map (fun content -> EntityContent.expand content layer world) content
            let streams = List.map (function Left (lens, sieve, unfold, indexOpt, mapper) -> Some (layer, lens, sieve, unfold, indexOpt, mapper) | _ -> None) expansions |> List.definitize
            let descriptors = List.map (function Right (_, Left descriptor, _, _, _) -> Some descriptor | _ -> None) expansions |> List.definitize
            let filePaths = List.map (function Right (entityName, Right filePath, _, _, _) -> Some (name, entityName, filePath) | _ -> None) expansions |> List.definitize
            let handlers = List.map (function Right (_, _, handlers, _, _) -> Some handlers | _ -> None) expansions |> List.definitize |> List.concat
            let binds = List.map (function Right (_, _, _, binds, _) -> Some binds | _ -> None) expansions |> List.definitize |> List.concat
            let entityContents = List.map (function Right (_, _, _, _, entityContents) -> Some entityContents | _ -> None) expansions |> List.definitize
            let (descriptor, handlersLayer, bindsLayer) = Describe.layer5 dispatcherName (Some name) initializers descriptors layer world
            Right (name, Left descriptor, handlers @ handlersLayer, binds @ bindsLayer, streams, filePaths, entityContents)
        | LayerFromFile (dispatcherName, name, filePath, initializers, content) ->
            let layer = screen / name
            let expansions = List.map (fun content -> EntityContent.expand content layer world) content
            let streams = List.map (function Left (lens, sieve, unfold, indexOpt, mapper) -> Some (layer, lens, sieve, unfold, indexOpt, mapper) | _ -> None) expansions |> List.definitize
            let descriptors = List.map (function Right (_, Left descriptor, _, _, _) -> Some descriptor | _ -> None) expansions |> List.definitize
            let filePaths = List.map (function Right (entityName, Right filePath, _, _, _) -> Some (name, entityName, filePath) | _ -> None) expansions |> List.definitize
            let handlers = List.map (function Right (_, _, handlers, _, _) -> Some handlers | _ -> None) expansions |> List.definitize |> List.concat
            let binds = List.map (function Right (_, _, _, binds, _) -> Some binds | _ -> None) expansions |> List.definitize |> List.concat
            let entityContents = List.map (function Right (_, _, _, _, entityContents) -> Some entityContents | _ -> None) expansions |> List.definitize
            let (_, handlersLayer, bindsLayer) = Describe.layer5 dispatcherName (Some name) initializers descriptors layer world
            Right (name, Right filePath, handlers @ handlersLayer, binds @ bindsLayer, streams, filePaths, entityContents)

/// Describes the content of a screen.
type [<NoEquality; NoComparison>] ScreenContent =
    | ScreenFromInitializers of string * string * ScreenBehavior * PropertyInitializer list * LayerContent list
    | ScreenFromLayerFile of string * ScreenBehavior * Type * string
    | ScreenFromFile of string * ScreenBehavior * string
    interface SimulantContent

    /// Expand a screen content to its constituent parts.
    static member expand content (_ : Game) world =
        match content with
        | ScreenFromInitializers (dispatcherName, name, behavior, initializers, content) ->
            let screen = Screen name
            let expansions = List.map (fun content -> LayerContent.expand content screen world) content
            let streams = List.map (function Left (lens, sieve, unfold, indexOpt, mapper) -> Some (screen, lens, sieve, unfold, indexOpt, mapper) | _ -> None) expansions |> List.definitize
            let descriptors = List.map (function Right (_, Left descriptor, _, _, _, _, _) -> Some descriptor | _ -> None) expansions |> List.definitize
            let handlers = List.map (function Right (_, _, handlers, _, _, _, _) -> Some handlers | _ -> None) expansions |> List.definitize |> List.concat
            let binds = List.map (function Right (_, _, _, binds, _, _, _) -> Some binds | _ -> None) expansions |> List.definitize |> List.concat
            let entityStreams = List.map (function Right (_, _, _, _, stream, _, _) -> Some stream | _ -> None) expansions |> List.definitize |> List.concat
            let entityFilePaths = List.map (function Right (_, _, _, _, _, filePaths, _) -> Some (List.map (fun (layerName, entityName, filePath) -> (name, layerName, entityName, filePath)) filePaths) | _ -> None) expansions |> List.definitize |> List.concat
            let entityContents = List.map (function Right (_, _, _, _, _, _, entityContents) -> Some entityContents | _ -> None) expansions |> List.definitize |> List.concat
            let layerFilePaths = List.map (function Right (layerName, Right filePath, _, _, _, _, _) -> Some (name, layerName, filePath) | _ -> None) expansions |> List.definitize
            let (descriptor, handlersScreen, bindsScreen) = Describe.screen5 dispatcherName (Some name) initializers descriptors screen world
            Left (name, descriptor, handlers @ handlersScreen, binds @ bindsScreen, behavior, streams, entityStreams, layerFilePaths, entityFilePaths, entityContents)
        | ScreenFromLayerFile (name, behavior, ty, filePath) -> Right (name, behavior, Some ty, filePath)
        | ScreenFromFile (name, behavior, filePath) -> Right (name, behavior, None, filePath)

/// Describes the content of a game.
type [<NoEquality; NoComparison>] GameContent =
    | GameFromInitializers of string * PropertyInitializer list * ScreenContent list
    | GameFromFile of string
    interface SimulantContent

    /// Expand a game content to its constituent parts.
    static member expand content world =
        match content with
        | GameFromInitializers (dispatcherName, initializers, content) ->
            let game = Game ()
            let expansions = List.map (fun content -> ScreenContent.expand content game world) content
            let descriptors = Either.getLeftValues expansions |> List.map (fun (_, descriptor, _, _, _, _, _, _, _, _) -> descriptor)
            let handlers = Either.getLeftValues expansions |> List.map (fun (_, _, handlers, _, _, _, _, _, _, _) -> handlers) |> List.concat
            let binds = Either.getLeftValues expansions |> List.map (fun (_, _, _, binds, _, _, _, _, _, _) -> binds) |> List.concat
            let layerStreams = Either.getLeftValues expansions |> List.map (fun (_, _, _, _, _, stream, _, _, _, _) -> stream) |> List.concat
            let entityStreams = Either.getLeftValues expansions |> List.map (fun (_, _, _, _, _, _, stream, _, _, _) -> stream) |> List.concat
            let screenBehaviors = Either.getLeftValues expansions |> List.map (fun (screenName, _, _,  _, _, behavior, _, _, _, _) -> (screenName, behavior)) |> Map.ofList
            let layerFilePaths = Either.getLeftValues expansions |> List.map (fun (_, _, _, _, _, _, _, layerFilePaths, _, _) -> layerFilePaths) |> List.concat
            let entityFilePaths = Either.getLeftValues expansions |> List.map (fun (_, _, _, _, _, _, _, _, entityFilePaths, _) -> entityFilePaths) |> List.concat
            let entityContents = Either.getLeftValues expansions |> List.map (fun (_, _, _, _, _, _, _, _, _, entityContents) -> entityContents) |> List.concat
            let screenFilePaths = Either.getRightValues expansions
            let (descriptor, handlersGame, bindsGame) = Describe.game5 dispatcherName initializers descriptors game world
            Left (descriptor, handlers @ handlersGame, binds @ bindsGame, screenBehaviors, layerStreams, entityStreams, screenFilePaths, layerFilePaths, entityFilePaths, entityContents)
        | GameFromFile filePath -> Right filePath

/// The passive side-effects of a simulant.
type [<NoEquality; NoComparison>] View =
    | Render of single * single * obj AssetTag * RenderDescriptor
    | PlaySound of single * Sound AssetTag
    | PlaySong of int * single * Song AssetTag
    | FadeOutSong of int
    | StopSong
    | Tag of string * obj

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

    /// Bind the left property to the value of the right.
    /// HACK: bind3 allows the use of fake lenses in declarative usage.
    /// NOTE: the downside to using fake lenses is that composed fake lenses do not function.
    let bind3 (left : Lens<'a, World>) (right : Lens<'a, World>) =
        if right.This :> obj |> isNull
        then failwith "bind3 expects an authentic right lens (where its This field is not null)."
        else BindDefinition (left, right)

    /// Bind the left property to the value of the right.
    let inline (<==) left right =
        bind3 left right

[<AutoOpen>]
module WorldDeclarative =

    let mutable internal LensError = false

    type World with

        /// Turn an entity lens into a series of live simulants.
        /// OPTIMIZATION: lots of optimizations going on in here including inlining and mutation!
        static member expandSimulants
            (lens : Lens<obj, World>)
            (sieve : obj -> obj)
            (unfold : obj -> World -> obj seq)
            (indexOpt : (obj -> int) option)
            (mapper : int -> Lens<obj, World> -> World -> SimulantContent)
            (origin : ContentOrigin)
            (parent : Simulant)
            world =
            let expansionId = Gen.id
            let previousSetKey = Gen.id
            let sieve' = fun a ->
                match lens.PayloadOpt with
                | Some payload ->
                    let (indices, _, _) = payload :?> Payload
                    let item = Array.fold (fun current index -> IEnumerable.item index (current :?> IEnumerable)) a indices
                    sieve item
                | None -> sieve a
            let mapper' = fun a (_ : obj option) (_ : World) -> sieve' a.Value
            let filter = fun a a2Opt _ -> match a2Opt with Some a2 -> a <> a2 | None -> true
            let lensSeq = lens |> Lens.map sieve |> Lens.mapWorld unfold
            let lenses = Lens.explodeIndexedOpt indexOpt lensSeq
            let subscription = fun _ world ->
                let items = Lens.get lensSeq world
                let mutable current = USet.makeEmpty Functional
                let mutable count = Seq.length items
                let mutable enr = lenses.GetEnumerator ()
                while count <> 0 && enr.MoveNext () do
                    let lens' = enr.Current
                    match lens'.Get world with
                    | Some (index, _) ->
                        let guid = Gen.idDeterministic index expansionId
                        let lens'' = { Lens.dereference lens' with Validate = fun world -> Option.isSome (lens'.Get world) } --> snd
                        let item = PartialComparable.make guid (index, lens'')
                        current <- USet.add item current
                        count <- count - 1
                    | None -> ()
                // NOTE: this is an alternative implementation that I thought would be faster but is not yet seeming to be.
                // Perhaps it will be faster in that cases where there are large gaps in the indices.
                //let items = Lens.get lensSeq world
                //let mutable current = USet.makeEmpty Functional
                //let mutable enr = items.GetEnumerator ()
                //let mutable i = 0
                //while enr.MoveNext () do
                //    let item = enr.Current
                //    let index = match indexOpt with Some indexer -> indexer item | None -> i
                //    let expansionIdDet = Gen.idDeterministic index expansionId
                //    let lensOpt = lenses |> Seq.item index
                //    let lensItem = { Lens.dereference lensOpt with Validate = fun world -> Option.isSome (lensOpt.Get world) } --> snd
                //    current <- USet.add (PartialComparable.make expansionIdDet (index, lensItem)) current
                //    i <- i + 1
                let previous =
                    match World.tryGetKeyedValue<PartialComparable<Guid, int * Lens<obj, World>> USet> previousSetKey world with
                    | Some previous -> previous
                    | None -> USet.makeEmpty Functional
                let added = USet.differenceFast current previous
                let removed = USet.differenceFast previous current
                let changed = added.Count <> 0 || removed.Count <> 0
                let world = World.addKeyedValue previousSetKey current world
                let world =
                    if changed then
                        let world =
                            Seq.fold (fun world guidAndContent ->
                                let (guid, _) = PartialComparable.unmake guidAndContent
                                match World.tryGetKeyedValue guid world with
                                | Some simulant ->
                                    let world = World.removeKeyedValue guid world
                                    WorldModule.destroy simulant world
                                | None -> world)
                                world removed
                        let world =
                            Seq.fold (fun world guidAndContent ->
                                let (guid, (index, lens)) = PartialComparable.unmake guidAndContent
                                let payloadOpt =
                                    match lens.PayloadOpt with
                                    | Some payload ->
                                        let (indices, _, _) = payload :?> Payload
                                        (Array.add index indices, Gen.id, mapper') :> obj |> Some
                                    | None -> ([|index|], Gen.id, mapper') :> obj |> Some
                                let lens = { lens with PayloadOpt = payloadOpt }
                                let content = mapper index lens world
                                match World.tryGetKeyedValue guid world with
                                | None ->
                                    let (simulantOpt, world) = WorldModule.expandContent Unchecked.defaultof<_> content origin parent world
                                    match simulantOpt with
                                    | Some simulant -> World.addKeyedValue guid simulant world
                                    | None -> world
                                | Some _ -> world)
                                world added
                        world
                    else world
                (Cascade, world)
            let (_, world) = subscription (Unchecked.defaultof<_>) world // expand simulants immediately rather than waiting for parent registration
            let (_, world) = World.monitorCompressed Gen.id (Some mapper') (Some filter) None (Left subscription) lens.ChangeEvent parent world
            world