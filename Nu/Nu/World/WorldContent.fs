// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu
open System
open System.Collections.Immutable
open Prime
open Nu

[<RequireQualifiedAccess>]
module Content =

    /// Describe a game to be loaded from a file.
    let gameFromFile<'d when 'd :> GameDispatcher> filePath =
        GameFromFile filePath

    /// Describe a game with the given initializers and contained screens.
    let game<'d when 'd :> GameDispatcher> initializers screens =
        GameFromInitializers (typeof<'d>.Name, initializers, screens)

    /// Describe a screen to be loaded from a file.
    let screenFromFile<'d when 'd :> ScreenDispatcher> screenName behavior filePath =
        ScreenFromFile (screenName, behavior, filePath)

    /// Describe a screen to be loaded from a file.
    let screenFromGroupFile<'d when 'd :> ScreenDispatcher> screenName behavior filePath =
        ScreenFromGroupFile (screenName, behavior, typeof<'d>, filePath)

    /// Describe a screen with the given initializers and contained groups.
    let screen<'d when 'd :> ScreenDispatcher> screenName behavior initializers groups =
        ScreenFromInitializers (typeof<'d>.Name, screenName, behavior, initializers, groups)

    /// Describe groups to be instantiated from a lens, caching what is sieved from the lens for greater efficiency.
    let groupsPlus
        (lens : Lens<'a, World>)
        (sieve : 'a -> 'b)
        (unfold : 'b -> Map<'k, 'c>)
        (mapper : 'k -> Lens<'c, World> -> GroupContent) =
        let lens = Lens.map box lens
        let sieve = fun (a : obj) -> sieve (a :?> 'a) :> obj
        let unfold = fun (b : obj) -> MapGeneralized.make (unfold (b :?> 'b))
        let mapper = fun (key : obj) (c : obj) -> mapper (key :?> 'k) (c :?> Lens<obj, World> |> Lens.map cast<'c>)
        GroupsFromStream (lens, sieve, unfold, mapper)
        
    /// Describe groups to be instantiated from a lens.
    let groups
        (lens : Lens<'a, World>)
        (unfold : 'a -> Map<'k, 'b>)
        (mapper : 'k -> Lens<'b, World> -> GroupContent) =
        groupsPlus lens unfold id mapper

    /// Describe a group to be conditionally instantiated from a lens.
    let groupOpt (lens : Lens<'a option, World>) (mapper : Lens<'a, World> -> GroupContent) =
        let (sieve : 'a option -> Map<int, 'a>) = fun aOpt -> match aOpt with Some a -> Map.singleton 0 a | None -> Map.empty
        let mapper = fun _ b -> mapper b
        groups lens sieve mapper

    /// Describe groups to be instantiated from a map lens.
    /// Unless the lens is very efficiently producing the map, you may want to use the Content.groups function instead
    /// to cache map creation where possible.
    let groupMap
        (lens : Lens<Map<'k, 'v>, World>)
        (mapper : 'k -> Lens<'v, World> -> GroupContent) =
        groups lens id mapper

    /// Describe groups to be instantiated from a block lens.
    let groupBlock
        (lens : Lens<'a ImmutableArray, World>)
        (mapper : int -> Lens<'a, World> -> GroupContent) =
        groupMap (lens --> fun a -> a |> Seq.indexed |> Map.ofSeq) mapper

    /// Describe optional groups to be instantiated from a block lens.
    let groupOpts
        (lens : Lens<'a option ImmutableArray, World>)
        (mapper : int -> Lens<'a, World> -> GroupContent) =
        groupMap (lens --> fun a -> a |> Seq.indexed |> Seq.filter (snd >> Option.isSome) |> Seq.map (fun (i, aOpt) -> (i, Option.get aOpt)) |> Map.ofSeq) mapper

    /// Describe a group to be loaded from a file.
    let groupFromFile<'d when 'd :> GroupDispatcher> groupName filePath =
        GroupFromFile (groupName, filePath)

    /// Describe a group with the given initializers and contained entities.
    let group<'d when 'd :> GroupDispatcher> groupName initializers entities =
        GroupFromInitializers (typeof<'d>.Name, groupName, initializers, entities)

    /// Describe a group with two instantiation paths.
    let groupEither lens mapper =
        groups lens (fun a ->
            let (key, value) = match a with Right r -> (0, Right r) | Left l -> (1, Left l)
            Map.singleton key value)
            (fun _ -> mapper)

    /// Describe a group with two instantiation paths.
    let groupOfEither lens split mapper =
        groupsPlus lens split (fun a ->
            let (key, value) = match a with Right r -> (0, Right r) | Left l -> (1, Left l)
            Map.singleton key value)
            (fun _ -> mapper)

    /// Describe a group with three instantiation paths.
    let groupChoice3 lens mapper =
        groups lens (fun a ->
            let (key, value) =
                match a with
                | Choice1Of3 v -> (0, Choice1Of3 v)
                | Choice2Of3 v -> (1, Choice2Of3 v)
                | Choice3Of3 v -> (2, Choice3Of3 v)
            Map.singleton key value)
            (fun _ -> mapper)

    /// Describe a group with three instantiation paths.
    let groupOf3 lens split mapper =
        groupsPlus lens split (fun a ->
            let (key, value) =
                match a with
                | Choice1Of3 v -> (0, Choice1Of3 v)
                | Choice2Of3 v -> (1, Choice2Of3 v)
                | Choice3Of3 v -> (2, Choice3Of3 v)
            Map.singleton key value)
            (fun _ -> mapper)

    /// Describe a group with four instantiation paths.
    let groupChoice4 lens mapper =
        groups lens (fun a ->
            let (key, value) =
                match a with
                | Choice1Of4 v -> (0, Choice1Of4 v)
                | Choice2Of4 v -> (1, Choice2Of4 v)
                | Choice3Of4 v -> (2, Choice3Of4 v)
                | Choice4Of4 v -> (3, Choice4Of4 v)
            Map.singleton key value)
            (fun _ -> mapper)

    /// Describe a group with three instantiation paths.
    let groupOf4 lens split mapper =
        groupsPlus lens split (fun a ->
            let (key, value) =
                match a with
                | Choice1Of4 v -> (0, Choice1Of4 v)
                | Choice2Of4 v -> (1, Choice2Of4 v)
                | Choice3Of4 v -> (2, Choice3Of4 v)
                | Choice4Of4 v -> (3, Choice4Of4 v)
            Map.singleton key value)
            (fun _ -> mapper)

    /// Describe a group with five instantiation paths.
    let groupChoice5 lens mapper =
        groups lens (fun a ->
            let (key, value) =
                match a with
                | Choice1Of5 v -> (0, Choice1Of5 v)
                | Choice2Of5 v -> (1, Choice2Of5 v)
                | Choice3Of5 v -> (2, Choice3Of5 v)
                | Choice4Of5 v -> (3, Choice4Of5 v)
                | Choice5Of5 v -> (4, Choice5Of5 v)
            Map.singleton key value)
            (fun _ -> mapper)

    /// Describe a group with five instantiation paths.
    let groupOf5 lens split mapper =
        groupsPlus lens split (fun a ->
            let (key, value) =
                match a with
                | Choice1Of5 v -> (0, Choice1Of5 v)
                | Choice2Of5 v -> (1, Choice2Of5 v)
                | Choice3Of5 v -> (2, Choice3Of5 v)
                | Choice4Of5 v -> (3, Choice4Of5 v)
                | Choice5Of5 v -> (4, Choice5Of5 v)
            Map.singleton key value)
            (fun _ -> mapper)

    /// Describe a group with six instantiation paths.
    let groupChoice6 lens mapper =
        groups lens (fun a ->
            let (key, value) =
                match a with
                | Choice1Of6 v -> (0, Choice1Of6 v)
                | Choice2Of6 v -> (1, Choice2Of6 v)
                | Choice3Of6 v -> (2, Choice3Of6 v)
                | Choice4Of6 v -> (3, Choice4Of6 v)
                | Choice5Of6 v -> (4, Choice5Of6 v)
                | Choice6Of6 v -> (5, Choice6Of6 v)
            Map.singleton key value)
            (fun _ -> mapper)

    /// Describe a group with six instantiation paths.
    let groupOf6 lens split mapper =
        groupsPlus lens split (fun a ->
            let (key, value) =
                match a with
                | Choice1Of6 v -> (0, Choice1Of6 v)
                | Choice2Of6 v -> (1, Choice2Of6 v)
                | Choice3Of6 v -> (2, Choice3Of6 v)
                | Choice4Of6 v -> (3, Choice4Of6 v)
                | Choice5Of6 v -> (4, Choice5Of6 v)
                | Choice6Of6 v -> (5, Choice6Of6 v)
            Map.singleton key value)
            (fun _ -> mapper)

    /// Describe a group with seven instantiation paths.
    let groupChoice7 lens mapper =
        groups lens (fun a ->
            let (key, value) =
                match a with
                | Choice1Of7 v -> (0, Choice1Of7 v)
                | Choice2Of7 v -> (1, Choice2Of7 v)
                | Choice3Of7 v -> (2, Choice3Of7 v)
                | Choice4Of7 v -> (3, Choice4Of7 v)
                | Choice5Of7 v -> (4, Choice5Of7 v)
                | Choice6Of7 v -> (5, Choice6Of7 v)
                | Choice7Of7 v -> (6, Choice7Of7 v)
            Map.singleton key value)
            (fun _ -> mapper)

    /// Describe a group with seven instantiation paths.
    let groupOf7 lens split mapper =
        groupsPlus lens split (fun a ->
            let (key, value) =
                match a with
                | Choice1Of7 v -> (0, Choice1Of7 v)
                | Choice2Of7 v -> (1, Choice2Of7 v)
                | Choice3Of7 v -> (2, Choice3Of7 v)
                | Choice4Of7 v -> (3, Choice4Of7 v)
                | Choice5Of7 v -> (4, Choice5Of7 v)
                | Choice6Of7 v -> (5, Choice6Of7 v)
                | Choice7Of7 v -> (6, Choice7Of7 v)
            Map.singleton key value)
            (fun _ -> mapper)

    /// Describe a group to be conditionally instantiated from a lens.
    let groupWhen lens predicate (mapper : Lens<'a, World> -> GroupContent) =
        let mapper = fun _ a -> mapper a
        groups lens (fun a -> if predicate a then Map.singleton 0 a else Map.empty) mapper

    /// Describe a group to be instantiated when a screen is selected.
    let groupWhenScreenSelected (screen : Screen) (mapper : Lens<unit, World> -> GroupContent) =
        let mapper = (fun lens -> mapper (Lens.map (constant ()) lens))
        groupWhen Simulants.Game.SelectedScreenOpt (fun screenOpt -> screenOpt = Some screen) mapper

    /// Describe a group to be conditionally instantiated from a lens.
    let groupIf<'d, 'a when 'd :> GroupDispatcher> groupName (lens : Lens<'a, World>) predicate initializers content =
        groupWhen lens predicate (fun _ -> group<'d> groupName initializers content)

    /// Describe a group to be instantiated when a screen is selected.
    let groupIfScreenSelected<'d, 'a when 'd :> GroupDispatcher> groupName screen initializers content =
        groupWhenScreenSelected screen (fun _ -> group<'d> groupName initializers content)

    /// Describe a group to be conditionally loaded from a file.
    let groupFromFileIf<'d, 'a when 'd :> GroupDispatcher> groupName (lens : Lens<'a, World>) predicate filePath =
        groupWhen lens predicate (fun _ -> groupFromFile groupName filePath)

    /// Describe a group to be conditionally loaded from a file when a screen is selected.
    let groupFromFileIfScreenSelected<'d, 'a when 'd :> GroupDispatcher> groupName screen filePath =
        groupWhenScreenSelected screen (fun _ -> groupFromFile groupName filePath)

    /// Describe entities to be instantiated from a lens, caching what is sieved from the lens for greater efficiency.
    let entitiesPlus
        (lens : Lens<'a, World>)
        (sieve : 'a -> 'b)
        (unfold : 'b -> Map<'k, 'c>)
        (mapper : 'k -> Lens<'c, World> -> EntityContent) =
        let lens = Lens.map box lens
        let sieve = fun (a : obj) -> sieve (a :?> 'a) :> obj
        let unfold = fun (b : obj) -> MapGeneralized.make (unfold (b :?> 'b))
        let mapper = fun (key : obj) (c : obj) -> mapper (key :?> 'k) (c :?> Lens<obj, World> |> Lens.map cast<'c>)
        EntitiesFromStream (lens, sieve, unfold, mapper)

    /// Describe entities to be instantiated from a lens.
    let entities
        (lens : Lens<'a, World>)
        (unfold : 'a -> Map<'k, 'b>)
        (mapper : 'k -> Lens<'b, World> -> EntityContent) =
        entitiesPlus lens unfold id mapper

    /// Describe an entity to be conditionally instantiated from a lens.
    let entityOpt (lens : Lens<'a option, World>) (mapper : Lens<'a, World> -> EntityContent) =
        let (sieve : 'a option -> Map<int, 'a>) = fun aOpt -> match aOpt with Some a -> Map.singleton 0 a | None -> Map.empty
        let mapper = fun _ b -> mapper b
        entities lens sieve mapper

    /// Describe entities to be instantiated from a map lens.
    /// Unless the lens is very efficiently producing the map, you may want to use the Content.entities function
    /// instead to cache map creation where possible.
    let entityMap
        (lens : Lens<Map<'k, 'v>, World>)
        (mapper : 'k -> Lens<'v, World> -> EntityContent) =
        entities lens id mapper

    /// Describe entities to be instantiated from a block lens.
    let entityBlock
        (lens : Lens<'a ImmutableArray, World>)
        (mapper : int -> Lens<'a, World> -> EntityContent) =
        entityMap (lens --> fun a -> a |> Seq.indexed |> Map.ofSeq) mapper

    /// Describe optional entities to be instantiated from a block lens.
    let entityOpts
        (lens : Lens<'a option ImmutableArray, World>)
        (mapper : int -> Lens<'a, World> -> EntityContent) =
        entityMap (lens --> fun a -> a |> Seq.indexed |> Seq.filter (snd >> Option.isSome) |> Seq.map (fun (i, aOpt) -> (i, Option.get aOpt)) |> Map.ofSeq) mapper

    /// Describe an entity to be loaded from a file.
    let entityFromFile<'d when 'd :> EntityDispatcher> entityName filePath =
        EntityFromFile (entityName, filePath)

    /// Describe an entity with the given initializers and content.
    let entityWithContent<'d when 'd :> EntityDispatcher> entityName initializers content =
        EntityFromInitializers (typeof<'d>.Name, entityName, initializers, content)

    /// Describe an entity with the given initializers.
    let entity<'d when 'd :> EntityDispatcher> entityName initializers =
        entityWithContent<'d> entityName initializers []

    /// Describe an entity to be conditionally instantiated from a lens.
    let entityWhen lens predicate mapper =
        let mapper = fun _ a -> mapper a
        entities lens (fun a -> if predicate a then Map.singleton 0 a else Map.empty) mapper

    /// Describe an entity to be instantiated when a screen is selected.
    let entityWhenScreenSelected (screen : Screen) (mapper : Lens<unit, World> -> EntityContent) =
        let mapper = (fun lens -> mapper (Lens.map (constant ()) lens))
        entityWhen Simulants.Game.SelectedScreenOpt (fun screenOpt -> screenOpt = Some screen) mapper

    /// Describe an entity with two instantiation paths.
    let entityEither lens mapper =
        entities lens (fun a ->
            let (key, value) = match a with Right r -> (0, Right r) | Left l -> (1, Left l)
            Map.singleton key value)
            (fun _ -> mapper)

    /// Describe an entity with two instantiation paths.
    let entityOfEither lens split mapper =
        entitiesPlus lens split (fun a ->
            let (key, value) = match a with Right r -> (0, Right r) | Left l -> (1, Left l)
            Map.singleton key value)
            (fun _ -> mapper)

    /// Describe an entity with three instantiation paths.
    let entityChoice3 lens mapper =
        entities lens (fun a ->
            let (key, value) =
                match a with
                | Choice1Of3 v -> (0, Choice1Of3 v)
                | Choice2Of3 v -> (1, Choice2Of3 v)
                | Choice3Of3 v -> (2, Choice3Of3 v)
            Map.singleton key value)
            (fun _ -> mapper)

    /// Describe an entity with three instantiation paths.
    let entityOf3 lens split mapper =
        entitiesPlus lens split (fun a ->
            let (key, value) =
                match a with
                | Choice1Of3 v -> (0, Choice1Of3 v)
                | Choice2Of3 v -> (1, Choice2Of3 v)
                | Choice3Of3 v -> (2, Choice3Of3 v)
            Map.singleton key value)
            (fun _ -> mapper)

    /// Describe an entity with four instantiation paths.
    let entityChoice4 lens mapper =
        entities lens (fun a ->
            let (key, value) =
                match a with
                | Choice1Of4 v -> (0, Choice1Of4 v)
                | Choice2Of4 v -> (1, Choice2Of4 v)
                | Choice3Of4 v -> (2, Choice3Of4 v)
                | Choice4Of4 v -> (3, Choice4Of4 v)
            Map.singleton key value)
            (fun _ -> mapper)

    /// Describe an entity with three instantiation paths.
    let entityOf4 lens split mapper =
        entitiesPlus lens split (fun a ->
            let (key, value) =
                match a with
                | Choice1Of4 v -> (0, Choice1Of4 v)
                | Choice2Of4 v -> (1, Choice2Of4 v)
                | Choice3Of4 v -> (2, Choice3Of4 v)
                | Choice4Of4 v -> (3, Choice4Of4 v)
            Map.singleton key value)
            (fun _ -> mapper)

    /// Describe an entity with five instantiation paths.
    let entityChoice5 lens mapper =
        entities lens (fun a ->
            let (key, value) =
                match a with
                | Choice1Of5 v -> (0, Choice1Of5 v)
                | Choice2Of5 v -> (1, Choice2Of5 v)
                | Choice3Of5 v -> (2, Choice3Of5 v)
                | Choice4Of5 v -> (3, Choice4Of5 v)
                | Choice5Of5 v -> (4, Choice5Of5 v)
            Map.singleton key value)
            (fun _ -> mapper)

    /// Describe an entity with five instantiation paths.
    let entityOf5 lens split mapper =
        entitiesPlus lens split (fun a ->
            let (key, value) =
                match a with
                | Choice1Of5 v -> (0, Choice1Of5 v)
                | Choice2Of5 v -> (1, Choice2Of5 v)
                | Choice3Of5 v -> (2, Choice3Of5 v)
                | Choice4Of5 v -> (3, Choice4Of5 v)
                | Choice5Of5 v -> (4, Choice5Of5 v)
            Map.singleton key value)
            (fun _ -> mapper)

    /// Describe an entity with six instantiation paths.
    let entityChoice6 lens mapper =
        entities lens (fun a ->
            let (key, value) =
                match a with
                | Choice1Of6 v -> (0, Choice1Of6 v)
                | Choice2Of6 v -> (1, Choice2Of6 v)
                | Choice3Of6 v -> (2, Choice3Of6 v)
                | Choice4Of6 v -> (3, Choice4Of6 v)
                | Choice5Of6 v -> (4, Choice5Of6 v)
                | Choice6Of6 v -> (5, Choice6Of6 v)
            Map.singleton key value)
            (fun _ -> mapper)

    /// Describe an entity with six instantiation paths.
    let entityOf6 lens split mapper =
        entitiesPlus lens split (fun a ->
            let (key, value) =
                match a with
                | Choice1Of6 v -> (0, Choice1Of6 v)
                | Choice2Of6 v -> (1, Choice2Of6 v)
                | Choice3Of6 v -> (2, Choice3Of6 v)
                | Choice4Of6 v -> (3, Choice4Of6 v)
                | Choice5Of6 v -> (4, Choice5Of6 v)
                | Choice6Of6 v -> (5, Choice6Of6 v)
            Map.singleton key value)
            (fun _ -> mapper)

    /// Describe an entity with seven instantiation paths.
    let entityChoice7 lens mapper =
        entities lens (fun a ->
            let (key, value) =
                match a with
                | Choice1Of7 v -> (0, Choice1Of7 v)
                | Choice2Of7 v -> (1, Choice2Of7 v)
                | Choice3Of7 v -> (2, Choice3Of7 v)
                | Choice4Of7 v -> (3, Choice4Of7 v)
                | Choice5Of7 v -> (4, Choice5Of7 v)
                | Choice6Of7 v -> (5, Choice6Of7 v)
                | Choice7Of7 v -> (6, Choice7Of7 v)
            Map.singleton key value)
            (fun _ -> mapper)

    /// Describe an entity with seven instantiation paths.
    let entityOf7 lens split mapper =
        entitiesPlus lens split (fun a ->
            let (key, value) =
                match a with
                | Choice1Of7 v -> (0, Choice1Of7 v)
                | Choice2Of7 v -> (1, Choice2Of7 v)
                | Choice3Of7 v -> (2, Choice3Of7 v)
                | Choice4Of7 v -> (3, Choice4Of7 v)
                | Choice5Of7 v -> (4, Choice5Of7 v)
                | Choice6Of7 v -> (5, Choice6Of7 v)
                | Choice7Of7 v -> (6, Choice7Of7 v)
            Map.singleton key value)
            (fun _ -> mapper)

    /// Describe an entity to be conditionally instantiated from a lens.
    let entityIf<'d, 'a when 'd :> EntityDispatcher> entityName (lens : Lens<'a, World>) predicate initializers =
        entityWhen lens predicate (fun _ -> entity<'d> entityName initializers)

    /// Describe an entity to be instantiated when a screen is selected.
    let entityIfScreenSelected<'d, 'a when 'd :> EntityDispatcher> entityName screen initializers =
        entityWhenScreenSelected screen (fun _ -> entity<'d> entityName initializers)

    /// Describe an entity with content to be conditionally instantiated from a lens.
    let entityWithContentWhen<'d, 'a when 'd :> EntityDispatcher> entityName predicate (lens : Lens<'a, World>) initializers content =
        entityWhen lens predicate (fun _ -> entityWithContent<'d> entityName initializers content)

    /// Describe an entity with content to be instantiated when a screen is selected.
    let entityWithContentWhenScreenSelected<'d, 'a when 'd :> EntityDispatcher> entityName screen (_ : Lens<'a, World>) initializers content =
        entityWhenScreenSelected screen (fun _ -> entityWithContent<'d> entityName initializers content)

    /// Describe an entity with content to be conditionally instantiated from a lens.
    let entityWithContentIf<'d, 'a when 'd :> EntityDispatcher> entityName (lens : Lens<'a, World>) predicate initializers content =
        entityWhen lens predicate (fun _ -> entityWithContent<'d> entityName initializers content)

    /// Describe an entity with content to be instantiated when a screen is selected.
    let entityWithContentIfScreenSelected<'d, 'a when 'd :> EntityDispatcher> entityName screen initializers content =
        entityWhenScreenSelected screen (fun _ -> entityWithContent<'d> entityName initializers content)

    /// Describe an entity to be conditionally loaded from a file.
    let entityFromFileIf<'d, 'a when 'd :> EntityDispatcher> entityName (lens : Lens<'a, World>) predicate filePath =
        entityWhen lens predicate (fun _ -> entityFromFile entityName filePath)

    /// Describe an entity to be conditionally loaded from a file when a screen is selected.
    let entityFromFileIfScreenSelected<'d, 'a when 'd :> EntityDispatcher> entityName screen filePath =
        entityWhenScreenSelected screen (fun _ -> entityFromFile entityName filePath)

    /// Describe a 2d basic emitter with the given initializers.
    let basicEmitter2d entityName initializers = entity<BasicEmitterDispatcher2d> entityName initializers

    /// Describe a 2d effect with the given initializers.
    let effect2d entityName initializers = entity<EffectDispatcher2d> entityName initializers

    /// Describe a static sprite with the given initializers.
    let staticSprite entityName initializers = entity<StaticSpriteDispatcher> entityName initializers

    /// Describe an animated sprite with the given initializers.
    let animatedSprite entityName initializers = entity<AnimatedSpriteDispatcher> entityName initializers

    /// Describe a button with the given initializers.
    let button entityName initializers = entity<ButtonDispatcher> entityName initializers

    /// Describe a label with the given initializers.
    let label entityName initializers = entity<LabelDispatcher> entityName initializers

    /// Describe a text with the given initializers.
    let text entityName initializers = entity<TextDispatcher> entityName initializers

    /// Describe a toggle button with the given initializers.
    let toggleButton entityName initializers = entity<ToggleButtonDispatcher> entityName initializers

    /// Describe a radio button with the given initializers.
    let radioButton entityName initializers = entity<RadioButtonDispatcher> entityName initializers

    /// Describe an fps gui with the given initializers.
    let fps entityName initializers = entity<FpsDispatcher> entityName initializers

    /// Describe a feeler with the given initializers.
    let feeler entityName initializers = entity<FeelerDispatcher> entityName initializers

    /// Describe a fill bar with the given initializers.
    let fillBar entityName initializers = entity<FillBarDispatcher> entityName initializers

    /// Describe an association of gui entities with the given initializers and content.
    let association entityName initializers content = entityWithContent<GuiDispatcher> entityName initializers content

    /// Describe a conditionally-existent association of gui entities with the given initializers and content.
    let associationIf entityName (lens : Lens<'a, World>) predicate initializers content = entityWithContentIf<GuiDispatcher, 'a> entityName lens predicate initializers content

    /// Describe a panel with the given initializers and content.
    let panel entityName initializers content = entityWithContent<LabelDispatcher> entityName initializers content

    /// Describe a conditionally-existent panel with the given initializers and content.
    let panelIf entityName (lens : Lens<'a, World>) predicate initializers content = entityWithContentIf<LabelDispatcher, 'a> entityName lens predicate initializers content

    /// Describe a 2d block with the given initializers.
    let block2d entityName initializers = entity<BlockDispatcher2d> entityName initializers

    /// Describe a 2d box with the given initializers.
    let box2d entityName initializers = entity<BoxDispatcher2d> entityName initializers

    /// Describe a side-view character with the given initializers.
    let sideViewCharacter entityName initializers = entity<SideViewCharacterDispatcher> entityName initializers

    /// Describe a tile map with the given initializers.
    let tileMap entityName initializers = entity<TileMapDispatcher> entityName initializers

    /// Describe a tmx map with the given initializers.
    let tmxMap entityName initializers = entity<TmxMapDispatcher> entityName initializers

    /// Describe a 3d light with the given initializers.
    let light3d entityName initializers = entity<LightDispatcher3d> entityName initializers

    /// Describe a sky box with the given initializers.
    let skyBox entityName initializers = entity<SkyBoxDispatcher> entityName initializers

    /// Describe a static billboard with the given initializers.
    let staticBillboard entityName initializers = entity<StaticBillboardDispatcher> entityName initializers

    /// Describe a static model with the given initializers.
    let staticModel entityName initializers = entity<StaticModelDispatcher> entityName initializers

    /// Describe a static model surface with the given initializers.
    let staticModelSurface entityName initializers = entity<StaticModelSurfaceDispatcher> entityName initializers

    /// Describe a static scene with the given initializers.
    let staticScene entityName initializers = entity<StaticSceneDispatcher> entityName initializers

[<AutoOpen>]
module ContentOperators =

    /// Bind an event to a signal.
    let inline on (eventAddress : 'a Address) (signal : Signal<'message, 'command>) : PropertyInitializer =
        let eventNameIndex = Address.findIndex (fun name -> name = "Event") eventAddress
        let partialAddress = Address.take (inc eventNameIndex) eventAddress
        EventHandlerDefinition ((fun _ -> signal :> obj), partialAddress)

    /// Bind an event to a signal.
    let inline (==>) (eventAddress : 'a Address) (signal : Signal<'message, 'command>) : PropertyInitializer =
        on eventAddress signal

    /// Bind an event to a signal.
    let inline handle (eventAddress : 'a Address) (handler : Event<'a, 'p> -> Signal<'message, 'command>) : PropertyInitializer =
        let eventNameIndex = Address.findIndex (fun name -> name = "Event") eventAddress
        let partialAddress = Address.take (inc eventNameIndex) eventAddress
        EventHandlerDefinition ((fun evt -> handler (Event.specialize evt) :> obj), partialAddress)

    /// Bind an event to a signal.
    let inline (==|>) (eventAddress : 'a Address) (handler : Event<'a, 'p> -> Signal<'message, 'command>) : PropertyInitializer =
        handle eventAddress handler