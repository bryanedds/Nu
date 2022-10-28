// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu
open System
open Prime
open Nu

/// Describes the behavior of a screen.
type [<NoEquality; NoComparison>] ScreenBehavior =
    | Vanilla
    | Dissolve of DissolveDescriptor * SongDescriptor option
    | Splash of DissolveDescriptor * SplashDescriptor * SongDescriptor option * Screen
    | OmniScreen

/// Describes the content of an entity.
type [<NoEquality; NoComparison>] EntityContent =
    | EntitiesFromStream of Lens<obj, World> * (obj -> obj) * (obj -> MapGeneralized) * (obj -> Lens<obj, World> -> EntityContent)
    | EntityFromInitializers of string * string * PropertyInitializer list * EntityContent list
    | EntityFromFile of string * string
    interface SimulantContent

    /// Expand an entity content to its constituent parts.
    static member expand content (group : Group) (world : World) =
        match content with
        | EntitiesFromStream (lens, sieve, unfold, mapper) ->
            Choice1Of3 (lens, sieve, unfold, mapper)
        | EntityFromInitializers (dispatcherName, name, initializers, content) ->
            let (descriptor, handlersEntity, bindsEntity) = Describe.entity4 dispatcherName (Some [|name|]) initializers (group / name) world
            Choice2Of3 (name, descriptor, handlersEntity, bindsEntity, (group / name, content))
        | EntityFromFile (name, filePath) ->
            Choice3Of3 (name, filePath)

/// Describes the content of a group.
type [<NoEquality; NoComparison>] GroupContent =
    | GroupsFromStream of Lens<obj, World> * (obj -> obj) * (obj -> MapGeneralized) * (obj -> Lens<obj, World> -> GroupContent)
    | GroupFromInitializers of string * string * PropertyInitializer list * EntityContent list
    | GroupFromFile of string * string
    interface SimulantContent

    /// Expand a group content to its constituent parts.
    static member expand content screen (world : World) =
        match content with
        | GroupsFromStream (lens, sieve, unfold, mapper) ->
            Choice1Of3 (lens, sieve, unfold, mapper)
        | GroupFromInitializers (dispatcherName, name, initializers, content) ->
            let group = screen / name
            let expansions = List.map (fun content -> EntityContent.expand content group world) content
            let streams = List.map (function Choice1Of3 (lens, sieve, unfold, mapper) -> Some (group, lens, sieve, unfold, mapper) | _ -> None) expansions |> List.definitize
            let descriptors = List.map (function Choice2Of3 (_, descriptor, _, _, _) -> Some descriptor | _ -> None) expansions |> List.definitize
            let handlers = List.map (function Choice2Of3 (_, _, handlers, _, _) -> Some handlers | _ -> None) expansions |> List.definitize |> List.concat
            let binds = List.map (function Choice2Of3 (_, _, _, binds, _) -> Some binds | _ -> None) expansions |> List.definitize |> List.concat
            let entityContents = List.map (function Choice2Of3 (_, _, _, _, entityContents) -> Some entityContents | _ -> None) expansions |> List.definitize
            let filePaths = List.map (function Choice3Of3 filePath -> Some filePath | _ -> None) expansions |> List.definitize |> List.map (fun (entityName, path) -> (name, entityName, path))
            let (descriptor, handlersGroup, bindsGroup) = Describe.group5 dispatcherName (Some [|name|]) initializers descriptors group world
            Choice2Of3 (name, descriptor, handlers @ handlersGroup, binds @ bindsGroup, streams, filePaths, entityContents)
        | GroupFromFile (name, filePath) ->
            Choice3Of3 (name, filePath)

/// Describes the content of a screen.
type [<NoEquality; NoComparison>] ScreenContent =
    | ScreenFromInitializers of string * string * ScreenBehavior * PropertyInitializer list * GroupContent list
    | ScreenFromGroupFile of string * ScreenBehavior * Type * string
    | ScreenFromFile of string * ScreenBehavior * string
    interface SimulantContent

    /// Expand a screen content to its constituent parts.
    static member expand content (_ : Game) world =
        match content with
        | ScreenFromInitializers (dispatcherName, name, behavior, initializers, content) ->
            let screen = Screen name
            let expansions = List.map (fun content -> GroupContent.expand content screen world) content
            let streams = List.map (function Choice1Of3 (lens, sieve, unfold, mapper) -> Some (screen, lens, sieve, unfold, mapper) | _ -> None) expansions |> List.definitize
            let descriptors = List.map (function Choice2Of3 (_, descriptor, _, _, _, _, _) -> Some descriptor | _ -> None) expansions |> List.definitize
            let handlers = List.map (function Choice2Of3 (_, _, handlers, _, _, _, _) -> Some handlers | _ -> None) expansions |> List.definitize |> List.concat
            let binds = List.map (function Choice2Of3 (_, _, _, binds, _, _, _) -> Some binds | _ -> None) expansions |> List.definitize |> List.concat
            let entityStreams = List.map (function Choice2Of3 (_, _, _, _, stream, _, _) -> Some stream | _ -> None) expansions |> List.definitize |> List.concat
            let entityFilePaths = List.map (function Choice2Of3 (_, _, _, _, _, filePaths, _) -> Some (List.map (fun (groupName, entityName, filePath) -> (name, groupName, entityName, filePath)) filePaths) | _ -> None) expansions |> List.definitize |> List.concat
            let entityContents = List.map (function Choice2Of3 (_, _, _, _, _, _, entityContents) -> Some entityContents | _ -> None) expansions |> List.definitize |> List.concat
            let groupFilePaths = List.map (function Choice3Of3 (groupName, filePath) -> Some (name, groupName, filePath) | _ -> None) expansions |> List.definitize
            let (descriptor, handlersScreen, bindsScreen) = Describe.screen5 dispatcherName (Some [|name|]) initializers descriptors screen world
            Left (name, descriptor, handlers @ handlersScreen, binds @ bindsScreen, behavior, streams, entityStreams, groupFilePaths, entityFilePaths, entityContents)
        | ScreenFromGroupFile (name, behavior, ty, filePath) -> Right (name, behavior, Some ty, filePath)
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
            let descriptors = Either.getLefts expansions |> List.map (fun (_, descriptor, _, _, _, _, _, _, _, _) -> descriptor)
            let handlers = Either.getLefts expansions |> List.map (fun (_, _, handlers, _, _, _, _, _, _, _) -> handlers) |> List.concat
            let binds = Either.getLefts expansions |> List.map (fun (_, _, _, binds, _, _, _, _, _, _) -> binds) |> List.concat
            let groupStreams = Either.getLefts expansions |> List.map (fun (_, _, _, _, _, stream, _, _, _, _) -> stream) |> List.concat
            let entityStreams = Either.getLefts expansions |> List.map (fun (_, _, _, _, _, _, stream, _, _, _) -> stream) |> List.concat
            let screenBehaviors = Either.getLefts expansions |> List.map (fun (screenName, _, _,  _, _, behavior, _, _, _, _) -> (screenName, behavior)) |> Map.ofList
            let groupFilePaths = Either.getLefts expansions |> List.map (fun (_, _, _, _, _, _, _, groupFilePaths, _, _) -> groupFilePaths) |> List.concat
            let entityFilePaths = Either.getLefts expansions |> List.map (fun (_, _, _, _, _, _, _, _, entityFilePaths, _) -> entityFilePaths) |> List.concat
            let entityContents = Either.getLefts expansions |> List.map (fun (_, _, _, _, _, _, _, _, _, entityContents) -> entityContents) |> List.concat
            let screenFilePaths = Either.getRights expansions
            let (descriptor, handlersGame, bindsGame) = Describe.game5 dispatcherName initializers descriptors game world
            Left (descriptor, handlers @ handlersGame, binds @ bindsGame, screenBehaviors, groupStreams, entityStreams, screenFilePaths, groupFilePaths, entityFilePaths, entityContents)
        | GameFromFile filePath -> Right filePath

/// Opens up some functions to make simulant lenses more accessible.
module Declarative =

    let Game = Game.Lens
    let Screen = Screen.Lens
    let Group = Group.Lens
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
    /// HACK: bind2 allows the use of fake lenses in declarative usage.
    /// NOTE: the downside to using fake lenses is that composed fake lenses do not function.
    let bind2 (left : Lens<'a, World>) (right : Lens<'a, World>) =
        if right.This :> obj |> isNull
        then failwith "bind3 expects an authentic right lens (where its This field is not null)."
        else BindDefinition (left, right)

    /// Bind the left property to the value of the right.
    let inline (<==) left right =
        bind2 left right

    /// Link the left property with the right property (two-way binding).
    /// HACK: link2 allows the use of fake lenses in declarative usage.
    /// NOTE: the downside to using fake lenses is that composed fake lenses do not function.
    let link2 (left : Lens<'a, World>) (right : Lens<'a, World>) =
        if right.This :> obj |> isNull
        then failwith "link3 expects an authentic right lens (where its This field is not null)."
        else LinkDefinition (left, right)

    /// Link the left property with the right property (two-way bind).
    let inline (<=>) left right =
        link2 left right

[<AutoOpen>]
module WorldDeclarative =

    type World with

        static member internal increaseBindingCount (simulant : Simulant) world =
            match simulant with
            | :? Entity as entity ->
                match World.tryGetKeyedValueFast<UMap<Entity Address, int>> (EntityBindingCountsId, world) with
                | (true, entityBindingCounts) ->
                    match entityBindingCounts.TryGetValue entity.EntityAddress with
                    | (true, entityBindingCount) ->
                        let entityBindingCount = inc entityBindingCount
                        let entityBindingCounts = UMap.add entity.EntityAddress entityBindingCount entityBindingCounts
                        let world =
                            if entityBindingCount = 1 && World.getEntityExists entity world
                            then World.setEntityPublishChangeBindings true entity world |> snd'
                            else world
                        World.addKeyedValue EntityBindingCountsId entityBindingCounts world
                    | (false, _) ->
                        let entityBindingCounts = UMap.add entity.EntityAddress 1 entityBindingCounts
                        let world =
                            if World.getEntityExists entity world
                            then World.setEntityPublishChangeBindings true entity world |> snd'
                            else world
                        World.addKeyedValue EntityBindingCountsId entityBindingCounts world
                | (false, _) ->
                    let config = World.getCollectionConfig world
                    let entityBindingCounts = UMap.makeEmpty HashIdentity.Structural config
                    let entityBindingCounts = UMap.add entity.EntityAddress 1 entityBindingCounts
                    let world =
                        if World.getEntityExists entity world
                        then World.setEntityPublishChangeBindings true entity world |> snd'
                        else world
                    World.addKeyedValue EntityBindingCountsId entityBindingCounts world
            | _ -> world

        static member internal decreaseBindingCount (simulant : Simulant) world =
            match simulant with
            | :? Entity as entity ->
                match World.tryGetKeyedValueFast<UMap<Entity Address, int>> (EntityBindingCountsId, world) with
                | (true, entityBindingCounts) ->
                    match entityBindingCounts.TryGetValue entity.EntityAddress with
                    | (true, entityBindingCount) ->
                        let entityBindingCount = dec entityBindingCount
                        let entityBindingCounts =
                            if entityBindingCount = 0
                            then UMap.remove entity.EntityAddress entityBindingCounts
                            else UMap.add entity.EntityAddress entityBindingCount entityBindingCounts
                        let world =
                            if entityBindingCount = 0 && World.getEntityExists entity world
                            then World.setEntityPublishChangeBindings false entity world |> snd'
                            else world
                        World.addKeyedValue EntityBindingCountsId entityBindingCounts world
                    | (false, _) -> failwithumf ()
                | (false, _) -> failwithumf ()
            | _ -> world

        static member internal addPropertyBinding propertyBindingKey propertyAddress left (right : World Lens) world =
            match right.ParentOpt with
            | Some parent ->
                match world.ElmishBindingsMap.TryGetValue propertyAddress with
                | (true, elmishBindings) ->
                    let elmishBindings =
                        { elmishBindings with
                            EBSParents = UMap.add propertyBindingKey parent elmishBindings.EBSParents
                            EBSBindings =
                                let config = World.getCollectionConfig world
                                match elmishBindings.EBSBindings.TryGetValue (Right parent) with
                                | (true, PropertyBindingGroup propertyBindingGroup) ->
                                    let propertyBinding = { PBLeft = left; PBRight = right; PBPrevious = ValueNone; PBDivergenceId = world.DivergenceId }
                                    let propertyBindingGroup = { propertyBindingGroup with PBGParentPrevious = ValueNone; PBGPropertyBindings = OMap.add propertyBindingKey propertyBinding propertyBindingGroup.PBGPropertyBindings }
                                    OMap.add (Right parent) (PropertyBindingGroup propertyBindingGroup) elmishBindings.EBSBindings
                                | (_, _) ->
                                    let propertyBinding = { PBLeft = left; PBRight = right; PBPrevious = ValueNone; PBDivergenceId = world.DivergenceId }
                                    let propertyBindingGroup = { PBGParentPrevious = ValueNone; PBGParent = parent; PBGPropertyBindings = OMap.singleton HashIdentity.Structural config propertyBindingKey propertyBinding; PBGDivergenceId = world.DivergenceId }
                                    OMap.add (Right parent) (PropertyBindingGroup propertyBindingGroup) elmishBindings.EBSBindings }
                    let elmishBindingsMap = UMap.add propertyAddress elmishBindings world.ElmishBindingsMap
                    World.choose { world with ElmishBindingsMap = elmishBindingsMap }
                | (false, _) ->
                    let config = World.getCollectionConfig world
                    let elmishBindings =
                        { EBSParents = UMap.singleton HashIdentity.Structural config propertyBindingKey parent
                          EBSBindings = OMap.singleton HashIdentity.Structural config (Right parent) (PropertyBinding { PBLeft = left; PBRight = right; PBPrevious = ValueNone; PBDivergenceId = world.DivergenceId }) }
                    let elmishBindingsMap = UMap.add propertyAddress elmishBindings world.ElmishBindingsMap
                    World.choose { world with ElmishBindingsMap = elmishBindingsMap }
            | None ->
                match world.ElmishBindingsMap.TryGetValue propertyAddress with
                | (true, elmishBindings) ->
                    let elmishBindings = { elmishBindings with EBSBindings = OMap.add (Left propertyBindingKey) (PropertyBinding { PBLeft = left; PBRight = right; PBPrevious = ValueNone; PBDivergenceId = world.DivergenceId }) elmishBindings.EBSBindings }
                    let elmishBindingsMap = UMap.add propertyAddress elmishBindings world.ElmishBindingsMap
                    World.choose { world with ElmishBindingsMap = elmishBindingsMap }
                | (false, _) ->
                    let config = World.getCollectionConfig world
                    let elmishBindings =
                        { EBSParents = UMap.makeEmpty HashIdentity.Structural config
                          EBSBindings = OMap.singleton HashIdentity.Structural config (Left propertyBindingKey) (PropertyBinding { PBLeft = left; PBRight = right; PBPrevious = ValueNone; PBDivergenceId = world.DivergenceId }) }
                    let elmishBindingsMap = UMap.add propertyAddress elmishBindings world.ElmishBindingsMap
                    World.choose { world with ElmishBindingsMap = elmishBindingsMap }

        static member internal removePropertyBinding propertyBindingKey propertyAddress world =
            match world.ElmishBindingsMap.TryGetValue propertyAddress with
            | (true, elmishBindings) ->
                match elmishBindings.EBSParents.TryGetValue propertyBindingKey with
                | (true, parent) ->
                    let elmishBindings =
                        { elmishBindings with
                            EBSParents = UMap.remove propertyBindingKey elmishBindings.EBSParents
                            EBSBindings =
                                match elmishBindings.EBSBindings.TryGetValue (Right parent) with
                                | (true, PropertyBindingGroup propertyBindingGroup) ->
                                    let propertyBindingGroup = { propertyBindingGroup with PBGPropertyBindings = OMap.remove propertyBindingKey propertyBindingGroup.PBGPropertyBindings }
                                    if OMap.isEmpty propertyBindingGroup.PBGPropertyBindings
                                    then OMap.remove (Right parent) elmishBindings.EBSBindings
                                    else OMap.add (Right parent) (PropertyBindingGroup propertyBindingGroup) elmishBindings.EBSBindings
                                | (_, _) -> elmishBindings.EBSBindings }
                    let elmishBindingsMap =
                        if OMap.isEmpty elmishBindings.EBSBindings
                        then UMap.remove propertyAddress world.ElmishBindingsMap
                        else UMap.add propertyAddress elmishBindings world.ElmishBindingsMap
                    World.choose { world with ElmishBindingsMap = elmishBindingsMap }
                | (false, _) ->
                    let elmishBindings = { elmishBindings with EBSBindings = OMap.remove (Left propertyBindingKey) elmishBindings.EBSBindings }
                    let elmishBindingsMap =
                        if OMap.isEmpty elmishBindings.EBSBindings
                        then UMap.remove propertyAddress world.ElmishBindingsMap
                        else UMap.add propertyAddress elmishBindings world.ElmishBindingsMap
                    World.choose { world with ElmishBindingsMap = elmishBindingsMap }
            | (false, _) -> world

        static member internal addContentBinding contentBinding world =
            let propertyAddress = PropertyAddress.make contentBinding.CBSource.Name contentBinding.CBSource.This
            let bindingsMap =
                match world.ElmishBindingsMap.TryGetValue propertyAddress  with
                | (true, elmishBindings) ->
                    let elmishBindings = { elmishBindings with EBSBindings = OMap.add (Left contentBinding.CBSimulantKey) (ContentBinding contentBinding) elmishBindings.EBSBindings }
                    UMap.add propertyAddress elmishBindings world.ElmishBindingsMap
                | (false, _) ->
                    let config = World.getCollectionConfig world
                    let elmishBindings =
                        { EBSParents = UMap.makeEmpty HashIdentity.Structural config
                          EBSBindings = OMap.singleton HashIdentity.Structural config (Left contentBinding.CBSimulantKey) (ContentBinding contentBinding) }
                    UMap.add propertyAddress elmishBindings world.ElmishBindingsMap
            World.choose { world with ElmishBindingsMap = bindingsMap }

        static member internal removeContentBinding contentBinding world =
            let propertyAddress = PropertyAddress.make contentBinding.CBSource.Name contentBinding.CBSource.This
            let bindingsMap =
                match world.ElmishBindingsMap.TryGetValue propertyAddress with
                | (true, elmishBindings) -> 
                    let elmishBindings = { elmishBindings with EBSBindings = OMap.remove (Left contentBinding.CBSimulantKey) elmishBindings.EBSBindings }
                    if OMap.isEmpty elmishBindings.EBSBindings
                    then UMap.remove propertyAddress world.ElmishBindingsMap
                    else UMap.add propertyAddress elmishBindings world.ElmishBindingsMap
                | (false, _) -> world.ElmishBindingsMap
            World.choose { world with ElmishBindingsMap = bindingsMap }

        /// Turn a lens into a series of live simulants.
        static member expandSimulants
            (lens : Lens<obj, World>)
            (sieve : obj -> obj)
            (unfold : obj -> MapGeneralized)
            (mapper : IComparable -> Lens<obj, World> -> SimulantContent)
            (origin : ContentOrigin)
            (owner : Simulant)
            (parent : Simulant)
            world =

            // construct the generalized lens with internal caching
            let lensGeneralized =
                let mutable lensResult = Unchecked.defaultof<obj> // ELMISH_CACHE
                let mutable sieveResultOpt = ValueNone // ELMISH_CACHE
                let mutable unfoldResultOpt = ValueNone // ELMISH_CACHE
                Lens.map (fun a ->
                    let struct (b, c) =
                        if a === lensResult then
                            match (sieveResultOpt, unfoldResultOpt) with
                            | (ValueSome sieveResult, ValueSome unfoldResult) -> struct (sieveResult, unfoldResult)
                            | (ValueSome sieveResult, ValueNone) -> struct (sieveResult, unfold sieveResult)
                            | (ValueNone, ValueSome _) -> failwithumf ()
                            | (ValueNone, ValueNone) -> let b = sieve a in struct (b, unfold b)
                        else
                            match (sieveResultOpt, unfoldResultOpt) with
                            | (ValueSome sieveResult, ValueSome unfoldResult) -> let b = sieve a in if b === sieveResult then struct (b, unfoldResult) else struct (b, unfold b)
                            | (ValueSome _, ValueNone) -> let b = sieve a in struct (b, unfold b)
                            | (ValueNone, ValueSome _) -> failwithumf ()
                            | (ValueNone, ValueNone) -> let b = sieve a in struct (b, unfold b)
                    lensResult <- a
                    sieveResultOpt <- ValueSome b
                    unfoldResultOpt <- ValueSome c
                    c)
                    lens

            // add content binding
            let contentKey = Gen.id
            let contentBinding = { CBMapper = mapper; CBSource = lensGeneralized; CBOrigin = origin; CBOwner = owner; CBParent = parent; CBSimulantKey = Gen.id; CBContentKey = contentKey }
            let world = World.increaseBindingCount lensGeneralized.This world
            let world = World.addContentBinding contentBinding world

            // handle removing content binding
            let world =
                World.monitor
                    (fun _ world ->
                        let world = World.removeContentBinding contentBinding world
                        let world = World.decreaseBindingCount contentBinding.CBSource.This world
                        (Cascade, world))
                    (Events.Unregistering --> owner.SimulantAddress) owner world

            // synchronize simulants immediately rather than waiting for parent registration if this is the first time through
            let world =
                if Lens.validate contentBinding.CBSource world then
                    let mapGeneralized = Lens.getWithoutValidation contentBinding.CBSource world
                    let lensesCurrent = World.makeLensesCurrent mapGeneralized.Keys contentBinding.CBSource world
                    World.synchronizeSimulants contentBinding.CBMapper contentBinding.CBContentKey mapGeneralized lensesCurrent contentBinding.CBOrigin contentBinding.CBOwner contentBinding.CBParent world
                else
                    let config = World.getCollectionConfig world
                    let lensesCurrent = USet.makeEmpty (LensComparer ()) config
                    World.synchronizeSimulants mapper contentKey (MapGeneralized.make Map.empty) lensesCurrent origin owner parent world

            // fin
            world