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
    let groupsFast
        (lens : Lens<'a, World>)
        (sieve : 'a -> World -> 'b)
        (unfold : 'b -> World -> Map<'k, 'c>)
        (mapper : 'k -> Lens<'c, World> -> World -> GroupContent) =
        let lens = Lens.map box lens
        let sieve = fun (a : obj) w -> sieve (a :?> 'a) w :> obj
        let unfold = fun (b : obj) w -> MapGeneralized.make (unfold (b :?> 'b) w)
        let mapper = fun (key : obj) (c : obj) world -> mapper (key :?> 'k) (c :?> Lens<obj, World> |> Lens.map cast<'c>) world
        GroupsFromStream (lens, sieve, unfold, mapper)
        
    /// Describe groups to be instantiated from a lens.
    let groups
        (lens : Lens<'a, World>)
        (unfold : 'a -> World -> Map<'k, 'b>)
        (mapper : 'k -> Lens<'b, World> -> World -> GroupContent) =
        groupsFast lens unfold constant mapper

    /// Describe a group to be optionally instantiated from a lens.
    let groupIf lens predicate (mapper : Lens<'a, World> -> World -> GroupContent) =
        let mapper = fun _ a world -> mapper a world
        groups lens (fun a _ -> if predicate a then Map.singleton 0 a else Map.empty) mapper

    /// Describe a group to be instantiated when a screen is selected.
    let groupIfScreenSelected (screen : Screen) (mapper : Lens<unit, World> -> World -> GroupContent) =
        let mapper = (fun lens world -> mapper (Lens.map (constant ()) lens) world)
        groupIf Simulants.Game.SelectedScreenOpt (fun screenOpt -> screenOpt = Some screen) mapper

    /// Describe a group to be optionally instantiated from a lens.
    let groupOpt (lens : Lens<'a option, World>) (mapper : Lens<'a, World> -> World -> GroupContent) =
        let (sieve : 'a option -> World -> Map<int, 'a>) = fun aOpt _ -> match aOpt with Some a -> Map.singleton 0 a | None -> Map.empty
        let mapper = fun _ b w -> mapper b w
        groups lens sieve mapper

    /// Describe groups to be instantiated from a map lens.
    /// Unless the lens is very efficiently producing the map, you may want to use the Content.groups function instead
    /// to cache map creation where possible.
    let groupMap
        (lens : Lens<Map<'k, 'v>, World>)
        (mapper : 'k -> Lens<'v, World> -> World -> GroupContent) =
        groups lens constant mapper

    /// Describe groups to be instantiated from a block lens.
    let groupBlock
        (lens : Lens<'a ImmutableArray, World>)
        (mapper : int -> Lens<'a, World> -> World -> GroupContent) =
        groupMap (lens --> fun a -> a |> Seq.indexed |> Map.ofSeq) mapper

    /// Describe groups to be instantiated from a partially-populated block lens.
    let groupOptBlock
        (lens : Lens<'a option ImmutableArray, World>)
        (mapper : int -> Lens<'a, World> -> World -> GroupContent) =
        groupMap (lens --> fun a -> a |> Seq.indexed |> Seq.filter (snd >> Option.isSome) |> Seq.map (fun (i, aOpt) -> (i, Option.get aOpt)) |> Map.ofSeq) mapper

    /// Describe a group to be loaded from a file.
    let groupFromFile<'d when 'd :> GroupDispatcher> groupName filePath =
        GroupFromFile (groupName, filePath)

    /// Describe a group with the given initializers and contained entities.
    let group<'d when 'd :> GroupDispatcher> groupName initializers entities =
        GroupFromInitializers (typeof<'d>.Name, groupName, initializers, entities)

    /// Describe entities to be instantiated from a lens, caching what is sieved from the lens for greater efficiency.
    let entitiesFast
        (lens : Lens<'a, World>)
        (sieve : 'a -> World -> 'b)
        (unfold : 'b -> World -> Map<'k, 'c>)
        (mapper : 'k -> Lens<'c, World> -> World -> EntityContent) =
        let lens = Lens.map box lens
        let sieve = fun (a : obj) w -> sieve (a :?> 'a) w :> obj
        let unfold = fun (b : obj) w -> MapGeneralized.make (unfold (b :?> 'b) w)
        let mapper = fun (key : obj) (c : obj) world -> mapper (key :?> 'k) (c :?> Lens<obj, World> |> Lens.map cast<'c>) world
        EntitiesFromStream (lens, sieve, unfold, mapper)

    /// Describe entities to be instantiated from a lens.
    let entities
        (lens : Lens<'a, World>)
        (unfold : 'a -> World -> Map<'k, 'b>)
        (mapper : 'k -> Lens<'b, World> -> World -> EntityContent) =
        entitiesFast lens unfold constant mapper

    /// Describe an entity to be optionally instantiated from a lens.
    let entityIf lens predicate mapper =
        let mapper = fun _ a world -> mapper a world
        entities lens (fun a _ -> if predicate a then Map.singleton 0 a else Map.empty) mapper

    /// Describe an entity to be instantiated when a screen is selected.
    let entityIfScreenSelected (screen : Screen) (mapper : Lens<unit, World> -> World -> EntityContent) =
        let mapper = (fun lens world -> mapper (Lens.map (constant ()) lens) world)
        entityIf Simulants.Game.SelectedScreenOpt (fun screenOpt -> screenOpt = Some screen) mapper

    /// Describe an entity to be optionally instantiated from a lens.
    let entityOpt (lens : Lens<'a option, World>) (mapper : Lens<'a, World> -> World -> EntityContent) =
        let (sieve : 'a option -> World -> Map<int, 'a>) = fun aOpt _ -> match aOpt with Some a -> Map.singleton 0 a | None -> Map.empty
        let mapper = fun _ b w -> mapper b w
        entities lens sieve mapper

    /// Describe entities to be instantiated from a map lens.
    /// Unless the lens is very efficiently producing the map, you may want to use the Content.entities function
    /// instead to cache map creation where possible.
    let entityMap
        (lens : Lens<Map<'k, 'v>, World>)
        (mapper : 'k -> Lens<'v, World> -> World -> EntityContent) =
        entities lens constant mapper

    /// Describe entities to be instantiated from a block lens.
    let entityBlock
        (lens : Lens<'a ImmutableArray, World>)
        (mapper : int -> Lens<'a, World> -> World -> EntityContent) =
        entityMap (lens --> fun a -> a |> Seq.indexed |> Map.ofSeq) mapper

    /// Describe entities to be instantiated from a block partially-populated lens.
    let entityOptBlock
        (lens : Lens<'a option ImmutableArray, World>)
        (mapper : int -> Lens<'a, World> -> World -> EntityContent) =
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

    /// Describe a 2d basic emitter with the given initializers.
    let basicEmitter2d entityName initializers = entity<BasicEmitter2dDispatcher> entityName initializers

    /// Describe a 2d effect with the given initializers.
    let effect2d entityName initializers = entity<Effect2dDispatcher> entityName initializers

    /// Describe a static sprite with the given initializers.
    let staticSprite entityName initializers = entity<StaticSpriteDispatcher> entityName initializers

    /// Describe an animated sprite with the given initializers.
    let animatedSprite entityName initializers = entity<AnimatedSpriteDispatcher> entityName initializers

    /// Describe a button with the given initializers.
    let button entityName initializers = entity<ButtonDispatcher> entityName initializers

    /// Describe a label with the given initializers.
    let label entityName initializers = entity<LabelDispatcher> entityName initializers

    /// Describe an association of gui entities with the given initializers and content.
    let association entityName initializers content = entityWithContent<GuiDispatcher> entityName initializers content

    /// Describe a panel with the given initializers and content.
    let panel entityName initializers content = entityWithContent<LabelDispatcher> entityName initializers content

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

    /// Describe a 2d block with the given initializers.
    let block2d entityName initializers = entity<Block2dDispatcher> entityName initializers

    /// Describe a 2d box with the given initializers.
    let box2d entityName initializers = entity<Box2dDispatcher> entityName initializers

    /// Describe a side-view character with the given initializers.
    let sideViewCharacter entityName initializers = entity<SideViewCharacterDispatcher> entityName initializers

    /// Describe a tile map with the given initializers.
    let tileMap entityName initializers = entity<TileMapDispatcher> entityName initializers

    /// Describe a tmx map with the given initializers.
    let tmxMap entityName initializers = entity<TmxMapDispatcher> entityName initializers

    /// Describe static scenery with the given initializers.
    let staticScenery entityName initializers = entity<TmxMapDispatcher> entityName initializers

    /// Describe light with the given initializers.
    let light entityName initializers = entity<LightDispatcher> entityName initializers

    /// Describe sky box with the given initializers.
    let skyBox entityName initializers = entity<SkyBoxDispatcher> entityName initializers

    /// Describe static model surface with the given initializers.
    let staticModelSurface entityName initializers = entity<StaticModelSurfaceDispatcher> entityName initializers

    /// Describe static model with the given initializers.
    let staticModel entityName initializers = entity<StaticModelDispatcher> entityName initializers

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