// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2018.

namespace Nu
open System
open Prime
open Nu

/// Contains primitives for describing simulant content.    
module Content =

    /// Describe a game to be loaded from a file.
    let gameFromFile<'d when 'd :> GameDispatcher> filePath =
        GameFromFile filePath

    /// Describe a game with the given definitions and contained screens.
    let game<'d when 'd :> GameDispatcher> definitions children =
        GameFromDefinitions (typeof<'d>.Name, definitions, children)

    /// Describe a screen to be loaded from a file.
    let screenFromFile<'d when 'd :> ScreenDispatcher> screenName behavior filePath =
        ScreenFromFile (screenName, behavior, filePath)

    /// Describe a screen to be loaded from a file.
    let screenFromLayerFile<'d when 'd :> ScreenDispatcher> screenName behavior filePath =
        ScreenFromLayerFile (screenName, behavior, typeof<'d>, filePath)

    /// Describe a screen with the given definitions and contained layers.
    let screen<'d when 'd :> ScreenDispatcher> screenName behavior definitions children =
        ScreenFromDefinitions (typeof<'d>.Name, screenName, behavior, definitions, children)

    /// Describe indexed layers to be streamed from a lens.
    let layersIndexed (lens : Lens<'a list, World>) (indexer : 'a -> int) (mapper : int -> Lens<'a, World> -> Screen -> World -> LayerContent) =
        let mapper = fun i (a : obj) screen world -> mapper i (a :?> Lens<obj, World> |> Lens.mapOut (cast<'a>)) screen world
        LayersFromStream (lens.MapOut box, Some (fun (o : obj) -> indexer (o :?> 'a)), mapper)

    /// Describe layers to be streamed from a lens.
    let layers (lens : Lens<'a list, World>) (mapper : int -> Lens<'a, World> -> Screen -> World -> LayerContent) =
        let mapper = fun i (a : obj) world -> mapper i (a :?> Lens<obj, World> |> Lens.mapOut (cast<'a>)) world
        LayersFromStream (lens.MapOut box, None, mapper)

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

    /// Describe a layer with the given definitions and contained entities.
    let layer<'d when 'd :> LayerDispatcher> layerName definitions children =
        LayerFromDefinitions (typeof<'d>.Name, layerName, definitions, children)

    /// Describe indexed entities to be streamed from a lens.
    let entitiesIndexed (lens : Lens<'a list, World>) (indexer : 'a -> int) (mapper : int -> Lens<'a, World> -> Layer -> World -> EntityContent) =
        let mapper = fun i (a : obj) layer world -> mapper i (a :?> Lens<obj, World> |> Lens.mapOut (cast<'a>)) layer world
        EntitiesFromStream (lens.MapOut box, Some (fun (o : obj) -> indexer (o :?> 'a)), mapper)

    /// Describe entities to be streamed from a lens.
    let entities (lens : Lens<'a list, World>) (mapper : int -> Lens<'a, World> -> Layer -> World -> EntityContent) =
        let mapper = fun i (a : obj) layer world -> mapper i (a :?> Lens<obj, World> |> Lens.mapOut (cast<'a>)) layer world
        EntitiesFromStream (lens.MapOut box, None, mapper)

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

    /// Describe an entity with the given definitions and content.
    let entityWithContent<'d when 'd :> EntityDispatcher> entityName definitions content =
        EntityFromDefinitions (typeof<'d>.Name, entityName, definitions, content)

    /// Describe an entity with the given definitions.
    let entity<'d when 'd :> EntityDispatcher> entityName definitions =
        entityWithContent<'d> entityName definitions []

    /// Describe an effect with the given definitions.
    let effect entityName definitions = entity<EffectDispatcher> entityName definitions

    /// Describe a node with the given definitions.
    let node entityName definitions = entity<NodeDispatcher> entityName definitions

    /// Describe a button with the given definitions.
    let button entityName definitions = entity<ButtonDispatcher> entityName definitions

    /// Describe a label with the given definitions.
    let label entityName definitions = entity<LabelDispatcher> entityName definitions

    /// Describe a text with the given definitions.
    let text entityName definitions = entity<TextDispatcher> entityName definitions

    /// Describe a toggle with the given definitions.
    let toggle entityName definitions = entity<ToggleDispatcher> entityName definitions

    /// Describe an fps gui with the given definitions.
    let fps entityName definitions = entity<FpsDispatcher> entityName definitions

    /// Describe a feeler with the given definitions.
    let feeler entityName definitions = entity<FeelerDispatcher> entityName definitions

    /// Describe a fill bar with the given definitions.
    let fillBar entityName definitions = entity<FillBarDispatcher> entityName definitions

    /// Describe a block with the given definitions.
    let block entityName definitions = entity<BlockDispatcher> entityName definitions

    /// Describe a box with the given definitions.
    let box entityName definitions = entity<BoxDispatcher> entityName definitions

    /// Describe a character with the given definitions.
    let character entityName definitions = entity<CharacterDispatcher> entityName definitions

    /// Describe a tile map with the given definitions.
    let tileMap entityName definitions = entity<TileMapDispatcher> entityName definitions

[<AutoOpen>]
module ContentOperators =

    /// Bind an event to a signal.
    let inline (>=>) (eventAddress : 'a Address) (signal : Signal<_, 'm>) : PropertyInitializer =
        EventHandlerDefinition ((fun _ -> signal :> obj), atooa eventAddress, World.deriveFromEvent eventAddress)

    /// Bind an event to a signal.
    let inline (>|>) (eventAddress : 'a Address) (handler : Event<'a, 'p> -> Signal<_, 'm>) : PropertyInitializer =
        EventHandlerDefinition ((fun evt -> handler (Event.specialize evt) :> obj), atooa eventAddress, World.deriveFromEvent eventAddress)