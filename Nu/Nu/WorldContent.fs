// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2018.

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
        GameFromDefinitions (typeof<'d>.Name, initializers, screens)

    /// Describe a screen to be loaded from a file.
    let screenFromFile<'d when 'd :> ScreenDispatcher> screenName behavior filePath =
        ScreenFromFile (screenName, behavior, filePath)

    /// Describe a screen to be loaded from a file.
    let screenFromLayerFile<'d when 'd :> ScreenDispatcher> screenName behavior filePath =
        ScreenFromLayerFile (screenName, behavior, typeof<'d>, filePath)

    /// Describe a screen with the given initializers and contained layers.
    let screen<'d when 'd :> ScreenDispatcher> screenName behavior initializers layers =
        ScreenFromDefinitions (typeof<'d>.Name, screenName, behavior, initializers, layers)

    /// Describe indexed layers to be streamed from a lens.
    let layersIndexed (lens : Lens<'a list, World>) (indexer : 'a -> int) (mapper : int -> Lens<'a, World> -> Screen -> World -> LayerContent) =
        let mapper = fun i (a : obj) screen world -> mapper i (a :?> Lens<obj, World> |> Lens.map (cast<'a>)) screen world
        LayersFromStream (lens.Map box, Some (fun (o : obj) -> indexer (o :?> 'a)), mapper)

    /// Describe layers to be streamed from a lens.
    let layers (lens : Lens<'a list, World>) (mapper : int -> Lens<'a, World> -> Screen -> World -> LayerContent) =
        let mapper = fun i (a : obj) world -> mapper i (a :?> Lens<obj, World> |> Lens.map (cast<'a>)) world
        LayersFromStream (lens.Map box, None, mapper)

    /// Describe a layer to be optionally streamed from a lens.
    let layerOpt (lens : Lens<'a option, World>) (mapper : Lens<'a, World> -> Screen -> World -> LayerContent) =
        layers (lens --> function Some a -> [a] | None -> []) (fun _ -> mapper)

    /// Describe a layer to be optionally streamed from a lens.
    let layerIf (lens : Lens<bool, World>) (mapper : Lens<unit, World> -> Screen -> World -> LayerContent) =
        layers (lens --> function true -> [()] | false -> []) (fun _ -> mapper)

    /// Describe a layer to be streamed when a screen is selected.
    let layerIfScreenSelected (screen : Screen) (mapper : Lens<unit, World> -> Screen -> World -> LayerContent) =
        layerIf (Default.Game.SelectedScreenOpt --> fun screenOpt -> screenOpt = Some screen) mapper

    /// Describe a layer to be loaded from a file.
    let layerFromFile<'d when 'd :> LayerDispatcher> layerName filePath =
        LayerFromFile (layerName, filePath)

    /// Describe a layer with the given initializers and contained entities.
    let layer<'d when 'd :> LayerDispatcher> layerName initializers entities =
        LayerFromDefinitions (typeof<'d>.Name, layerName, initializers, entities)

    /// Describe indexed entities to be streamed from a lens.
    let entitiesIndexed (lens : Lens<'a list, World>) (indexer : 'a -> int) (mapper : int -> Lens<'a, World> -> Layer -> World -> EntityContent) =
        let mapper = fun i (a : obj) layer world -> mapper i (a :?> Lens<obj, World> |> Lens.map (cast<'a>)) layer world
        EntitiesFromStream (lens.Map box, Some (fun (o : obj) -> indexer (o :?> 'a)), mapper)

    /// Describe entities to be streamed from a lens.
    let entities (lens : Lens<'a list, World>) (mapper : int -> Lens<'a, World> -> Layer -> World -> EntityContent) =
        let mapper = fun i (a : obj) layer world -> mapper i (a :?> Lens<obj, World> |> Lens.map (cast<'a>)) layer world
        EntitiesFromStream (lens.Map box, None, mapper)

    /// Describe an entity to be optionally streamed from a lens.
    let entityOpt (lens : Lens<'a option, World>) (mapper : Lens<'a, World> -> Layer -> World -> EntityContent) =
        entities (lens --> function Some a -> [a] | None -> []) (fun _ -> mapper)

    /// Describe an entity to be optionally streamed from a lens.
    let entityIf (lens : Lens<bool, World>) (mapper : Lens<unit, World> -> Layer -> World -> EntityContent) =
        entities (lens --> function true -> [()] | false -> []) (fun _ -> mapper)

    /// Describe an entity to be streamed when a screen is selected.
    let entityIfScreenSelected (screen : Screen) (mapper : Lens<unit, World> -> Layer -> World -> EntityContent) =
        entityIf (Default.Game.SelectedScreenOpt --> fun screenOpt -> screenOpt = Some screen) mapper

    /// Describe an entity to be loaded from a file.
    let entityFromFile<'d when 'd :> EntityDispatcher> entityName filePath =
        EntityFromFile (entityName, filePath)

    /// Describe an entity with the given initializers and content.
    let entityWithContent<'d when 'd :> EntityDispatcher> entityName initializers content =
        EntityFromDefinitions (typeof<'d>.Name, entityName, initializers, content)

    /// Describe an entity with the given initializers.
    let entity<'d when 'd :> EntityDispatcher> entityName initializers =
        entityWithContent<'d> entityName initializers []

    /// Describe an effect with the given initializers.
    let effect entityName initializers = entity<EffectDispatcher> entityName initializers

    /// Describe a node with the given initializers.
    let node entityName initializers = entity<NodeDispatcher> entityName initializers

    /// Describe a button with the given initializers.
    let button entityName initializers = entity<ButtonDispatcher> entityName initializers

    /// Describe a label with the given initializers.
    let label entityName initializers = entity<LabelDispatcher> entityName initializers

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
        EventHandlerDefinition ((fun _ -> signal :> obj), atooa eventAddress)

    /// Bind an event to a signal.
    let inline (==>) (eventAddress : 'a Address) (signal : Signal<'message, 'command>) : PropertyInitializer =
        on eventAddress signal

    /// Bind an event to a signal.
    let inline handle (eventAddress : 'a Address) (handler : Event<'a, 'p> -> Signal<'message, 'command>) : PropertyInitializer =
        EventHandlerDefinition ((fun evt -> handler (Event.specialize evt) :> obj), atooa eventAddress)

    /// Bind an event to a signal.
    let inline (==|>) (eventAddress : 'a Address) (handler : Event<'a, 'p> -> Signal<'message, 'command>) : PropertyInitializer =
        handle eventAddress handler