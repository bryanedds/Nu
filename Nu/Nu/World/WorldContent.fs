// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu
open System
open System.Collections.Immutable
open FSharp.Reflection
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
        
    // NOTE: disabled due to usability issues.
    ///// Describe one of multiple groups.
    //let groupMatch<'a> (lens : Lens<'a, World>) mapper =
    //    let cases = (FSharpType.GetUnionCases typeof<'a>)
    //    groups lens (fun a ->
    //        let (key, value) = try (getTag a, a) with _ -> (-1, a)
    //        Map.singleton key value)
    //        (fun key value -> mapper cases.[key].Name value)
    //
    ///// Describe one of multiple groups with explicit union unwrapping.
    //let groupOf<'a, 'b> (lens : Lens<'a, World>) (unwrap : 'a -> 'b) mapper =
    //    let cases = (FSharpType.GetUnionCases typeof<'b>)
    //    groupsPlus lens unwrap (fun a ->
    //        let (key, value) = try (getTag a, a) with _ -> (-1, a)
    //        Map.singleton key value)
    //        (fun key value -> mapper cases.[key].Name value)

    /// Describe a group to be conditionally instantiated from a lens.
    let groupIf lens predicate (mapper : Lens<'a, World> -> GroupContent) =
        let mapper = fun _ a -> mapper a
        groups
            (Lens.narrow (fun world -> predicate (Lens.get lens world)) lens)
            (fun a -> if predicate a then Map.singleton 0 a else Map.empty) mapper

    /// Describe a group to be instantiated when a screen is selected.
    let groupIfScreenSelected (screen : Screen) (mapper : Lens<unit, World> -> GroupContent) =
        let mapper = (fun lens -> mapper (Lens.map (constant ()) lens))
        groupIf Simulants.Game.SelectedScreenOpt (fun screenOpt -> screenOpt = Some screen) mapper

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

    /// Describe a composite entity with the given initializers and content.
    let composite<'d when 'd :> EntityDispatcher> entityName initializers content =
        EntityFromInitializers (typeof<'d>.Name, entityName, initializers, content)

    /// Describe an entity with the given initializers.
    let entity<'d when 'd :> EntityDispatcher> entityName initializers =
        composite<'d> entityName initializers []

    /// Describe an entity to be conditionally instantiated from a lens.
    let entityIf lens predicate mapper =
        let mapper = fun _ a -> mapper a
        entities
            (Lens.narrow (fun world -> predicate (Lens.get lens world)) lens)
            (fun a -> if predicate a then Map.singleton 0 a else Map.empty) mapper

    // NOTE: disabled due to usability issues.
    ///// Describe one of multiple entities.
    //let entityMatch<'a> (lens : Lens<'a, World>) mapper =
    //    let cases = (FSharpType.GetUnionCases typeof<'a>)
    //    entities lens (fun a ->
    //        let (key, value) = try (getTag a, a) with _ -> (-1, a)
    //        Map.singleton key value)
    //        (fun key value -> mapper cases.[key].Name value)
    //
    ///// Describe one of multiple entities with explicit union unwrapping.
    //let entityOf<'a, 'b> (lens : Lens<'a, World>) (unwrap : 'a -> 'b) mapper =
    //    let cases = (FSharpType.GetUnionCases typeof<'b>)
    //    entitiesPlus lens unwrap (fun a ->
    //        let (key, value) = try (getTag a, a) with _ -> (-1, a)
    //        Map.singleton key value)
    //        (fun key value -> mapper cases.[key].Name value)

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
    let association entityName initializers content = composite<GuiDispatcher> entityName initializers content

    /// Describe a panel with the given initializers and content.
    let panel entityName initializers content = composite<LabelDispatcher> entityName initializers content

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