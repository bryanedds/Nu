// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu
open System
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
    let screenFromLayerFile<'d when 'd :> ScreenDispatcher> screenName behavior filePath =
        ScreenFromLayerFile (screenName, behavior, typeof<'d>, filePath)

    /// Describe a screen with the given initializers and contained layers.
    let screen<'d when 'd :> ScreenDispatcher> screenName behavior initializers layers =
        ScreenFromInitializers (typeof<'d>.Name, screenName, behavior, initializers, layers)

    /// Describe layers to be instantiated from a lens.
    /// Allows the separation of sieve and unfold for efficiency.
    /// Allows for indexing for insertion and removal in arbitrary order.
    let layersPlus
        (lens : Lens<'a, World>)
        (sieve : 'a -> 'b)
        (unfold : 'b -> World -> 'c list)
        (indexOpt : ('c -> int) option)
        (mapper : int -> Lens<'c, World> -> World -> LayerContent) =
        let lens = lens.Map box
        let sieve = fun (a : obj) -> sieve (a :?> 'a) :> obj
        let unfold = fun (b : obj) w -> unfold (b :?> 'b) w |> Reflection.objToObjSeq
        let indexOpt =
            match indexOpt with
            | Some indexOpt -> Some (fun (o : obj) -> indexOpt (o :?> 'c))
            | None -> None
        let mapper = fun i (c : obj) world -> mapper i (c :?> Lens<obj, World> --> cast<'c>) world
        LayersFromStream (lens, sieve, unfold, indexOpt, mapper)

    /// Describe layers to be instantiated from a lens.
    /// Allows the separation of sieve and unfold for efficiency.
    let layers lens sieve unfold mapper =
        layersPlus lens sieve unfold None mapper

    /// Describe layers to be instantiated from a lens.
    /// Allows the separation of sieve and unfold for efficiency.
    /// Allows for indexing for insertion and removal in arbitrary order.
    let layersIndexedBy lens sieve unfold index mapper =
        layersPlus lens sieve unfold (Some index) mapper

    /// Describe layers to be instantiated from a lens indexed by fst.
    /// Allows the separation of sieve and unfold for efficiency.
    /// Allows for indexing by fst for insertion and removal in arbitrary order.
    let layersIndexedByFst lens sieve unfold mapper =
        let mapper = (fun i lens world -> mapper i (lens --> snd) world)
        layersPlus lens sieve unfold (Some (fun c -> fst c)) mapper

    /// Describe a layer to be optionally instantiated from a lens.
    let layerIf lens predicate mapper =
        layersPlus lens (fun a _ -> if predicate a then [a] else []) id None (constant mapper)

    /// Describe a layer to be instantiated when a screen is selected.
    let layerIfScreenSelected (screen : Screen) (mapper : Lens<unit, World> -> World -> LayerContent) =
        let mapper = (fun lens world -> mapper (Lens.map (constant ()) lens) world)
        layerIf Simulants.Game.SelectedScreenOpt (fun screenOpt -> screenOpt = Some screen) mapper

    /// Describe a layer to be optionally instantiated from a lens.
    let layerOpt lens sieve mapper =
        let mapper = (fun _ lens world -> mapper (lens --> Option.get) world)
        layersPlus lens sieve (fun a _ -> if Option.isSome a then [a] else []) None mapper

    /// Describe a layer to be loaded from a file.
    let layerFromFile<'d when 'd :> LayerDispatcher> layerName filePath =
        LayerFromFile (layerName, filePath)

    /// Describe a layer with the given initializers and contained entities.
    let layer<'d when 'd :> LayerDispatcher> layerName initializers entities =
        LayerFromInitializers (typeof<'d>.Name, layerName, initializers, entities)

    /// Describe entities to be instantiated from a lens.
    /// Allows the separation of sieve and unfold for efficiency.
    /// Allows for indexing for insertion and removal in arbitrary order.
    let entitiesPlus
        (lens : Lens<'a, World>)
        (sieve : 'a -> 'b)
        (unfold : 'b -> World -> 'c list)
        (indexOpt : ('c -> int) option)
        (mapper : int -> Lens<'c, World> -> World -> EntityContent) =
        let lens = lens.Map box
        let sieve = fun (a : obj) -> sieve (a :?> 'a) :> obj
        let unfold = fun (b : obj) w -> unfold (b :?> 'b) w |> Reflection.objToObjSeq
        let indexOpt =
            match indexOpt with
            | Some indexOpt -> Some (fun (o : obj) -> indexOpt (o :?> 'c))
            | None -> None
        let mapper = fun i (c : obj) world -> mapper i (c :?> Lens<obj, World> --> cast<'c>) world
        EntitiesFromStream (lens, sieve, unfold, indexOpt, mapper)

    /// Describe entities to be instantiated from a lens.
    /// Allows the separation of sieve and unfold for efficiency.
    let entities lens sieve unfold mapper =
        entitiesPlus lens sieve unfold None mapper

    /// Describe entities to be instantiated from a lens.
    /// Allows the separation of sieve and unfold for efficiency.
    /// Allows for indexing for insertion and removal in arbitrary order.
    let entitiesIndexedBy lens sieve unfold index mapper =
        entitiesPlus lens sieve unfold (Some index) mapper

    /// Describe entities to be instantiated from a lens indexed by fst.
    /// Allows the separation of sieve and unfold for efficiency.
    /// Allows for indexing by fst for insertion and removal in arbitrary order.
    let entitiesIndexedByFst lens sieve unfold mapper =
        let mapper = (fun i lens world -> mapper i (lens --> snd) world)
        entitiesPlus lens sieve unfold (Some (fun c -> fst c)) mapper

    /// Describe an entity to be optionally instantiated from a lens.
    let entityIf lens predicate mapper =
        entitiesPlus lens (fun a _ -> if predicate a then [a] else []) id None (constant mapper)

    /// Describe an entity to be instantiated when a screen is selected.
    let entityIfScreenSelected (screen : Screen) (mapper : Lens<unit, World> -> World -> EntityContent) =
        let mapper = (fun lens world -> mapper (Lens.map (constant ()) lens) world)
        entityIf Simulants.Game.SelectedScreenOpt (fun screenOpt -> screenOpt = Some screen) mapper

    /// Describe an entity to be optionally instantiated from a lens.
    let entityOpt lens sieve mapper =
        let mapper = (fun _ lens world -> mapper (lens --> Option.get) world)
        entitiesPlus lens sieve (fun a _ -> if Option.isSome a then [a] else []) None mapper

    /// Describe an entity to be loaded from a file.
    let entityFromFile<'d when 'd :> EntityDispatcher> entityName filePath =
        EntityFromFile (entityName, filePath)

    /// Describe an entity with the given initializers and content.
    let entityWithContent<'d when 'd :> EntityDispatcher> entityName initializers content =
        EntityFromInitializers (typeof<'d>.Name, entityName, initializers, content)

    /// Describe an entity with the given initializers.
    let entity<'d when 'd :> EntityDispatcher> entityName initializers =
        entityWithContent<'d> entityName initializers []

    /// Describe an effect with the given initializers.
    let effect entityName initializers = entity<EffectDispatcher> entityName initializers

    /// Describe a static sprite with the given initializers.
    let staticSprite entityName initializers = entity<StaticSpriteDispatcher> entityName initializers

    /// Describe an animated sprite with the given initializers.
    let animatedSprite entityName initializers = entity<AnimatedSpriteDispatcher> entityName initializers

    /// Describe a node with the given initializers.
    let node entityName initializers = entity<NodeDispatcher> entityName initializers

    /// Describe a button with the given initializers.
    let button entityName initializers = entity<ButtonDispatcher> entityName initializers

    /// Describe a label with the given initializers.
    let label entityName initializers = entity<LabelDispatcher> entityName initializers

    /// Describe a panel with the given initializers and content.
    let panel entityName initializers content = entityWithContent<LabelDispatcher> entityName initializers content

    /// Describe a text with the given initializers.
    let text entityName initializers = entity<TextDispatcher> entityName initializers

    /// Describe a toggle with the given initializers.
    let toggle entityName initializers = entity<ToggleDispatcher> entityName initializers

    /// Describe an fps gui with the given initializers.
    let fps entityName initializers = entity<FpsDispatcher> entityName initializers

    /// Describe a feeler with the given initializers.
    let feeler entityName initializers = entity<FeelerDispatcher> entityName initializers

    /// Describe a fill bar with the given initializers.
    let fillBar entityName initializers = entity<FillBarDispatcher> entityName initializers

    /// Describe a block with the given initializers.
    let block entityName initializers = entity<BlockDispatcher> entityName initializers

    /// Describe a box with the given initializers.
    let box entityName initializers = entity<BoxDispatcher> entityName initializers

    /// Describe a character with the given initializers.
    let character entityName initializers = entity<CharacterDispatcher> entityName initializers

    /// Describe a tile map with the given initializers.
    let tileMap entityName initializers = entity<TileMapDispatcher> entityName initializers

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