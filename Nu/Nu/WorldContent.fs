// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu
open System
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
    let screenFromLayerFile<'d when 'd :> ScreenDispatcher> screenName behavior filePath =
        ScreenFromLayerFile (screenName, behavior, typeof<'d>, filePath)

    /// Describe a screen with the given initializers and contained layers.
    let screen<'d when 'd :> ScreenDispatcher> screenName behavior initializers layers =
        ScreenFromInitializers (typeof<'d>.Name, screenName, behavior, initializers, layers)

    /// Describe layers to be streamed from a lens indexed by the given mapper.
    let layersIndexedBy (lens : Lens<'a list, World>) (indexer : 'a -> int) (mapper : int -> Lens<'a, World> -> World -> LayerContent) =
        let mapper = fun i (a : obj) world -> mapper i (a :?> Lens<obj, World> |> Lens.map (cast<'a>)) world
        LayersFromStream (lens.Map box, Some (fun (o : obj) -> indexer (o :?> 'a)), mapper)

    /// Describe layers to be streamed from a lens indexed by fst.
    let layersIndexedByFst (lens : Lens<(int * 'a) list, World>) (mapper : int -> Lens<(int * 'a), World> -> World -> LayerContent) =
        layersIndexedBy lens fst mapper

    /// Describe layers to be streamed from a lens indexed by snd.
    let layersIndexedBySnd (lens : Lens<('a * int) list, World>) (mapper : int -> Lens<('a * int), World> -> World -> LayerContent) =
        layersIndexedBy lens snd mapper

    /// Describe layers to be streamed from a lens indexed by discriminated union tag.
    let layersIndexedByArray (lens : Lens<'a list, World>) (arr : 'a array) (mapper : int -> Lens<'a, World> -> World -> LayerContent) =
        let indexer a = Array.findIndex ((=) a) arr
        layersIndexedBy lens indexer mapper

    /// Describe layers to be streamed from a lens indexed by its union tag.
    let layersIndexedByTag (lens : Lens<'a list, World>) (mapper : int -> Lens<'a, World> -> World -> LayerContent) =
        let indexer (a : 'a) =
            let (unionCaseInfo, _) = FSharpValue.GetUnionFields (a :> obj, typeof<'a>)
            unionCaseInfo.Tag
        layersIndexedBy lens indexer mapper

    /// Describe layers to be streamed from a lens.
    let layers (lens : Lens<'a list, World>) (mapper : int -> Lens<'a, World> -> World -> LayerContent) =
        let mapper = fun i (a : obj) world -> mapper i (a :?> Lens<obj, World> |> Lens.map (cast<'a>)) world
        LayersFromStream (lens.Map box, None, mapper)

    /// Describe a layer to be optionally streamed from a lens.
    let layerOpt (lens : Lens<'a option, World>) (mapper : Lens<'a, World> -> World -> LayerContent) =
        layers (lens --> function Some a -> [a] | None -> []) (fun _ -> mapper)

    /// Describe a layer to be optionally streamed from a lens.
    let layerIf (lens : Lens<bool, World>) (mapper : Lens<unit, World> -> World -> LayerContent) =
        layers (lens --> function true -> [()] | false -> []) (fun _ -> mapper)

    /// Describe a layer to be streamed when a screen is selected.
    let layerIfScreenSelected (screen : Screen) (mapper : Lens<unit, World> -> World -> LayerContent) =
        layerIf (Default.Game.SelectedScreenOpt --> fun screenOpt -> screenOpt = Some screen) mapper

    /// Describe a layer to be loaded from a file.
    let layerFromFile<'d when 'd :> LayerDispatcher> layerName filePath =
        LayerFromFile (layerName, filePath)

    /// Describe a layer with the given initializers and contained entities.
    let layer<'d when 'd :> LayerDispatcher> layerName initializers entities =
        LayerFromInitializers (typeof<'d>.Name, layerName, initializers, entities)

    /// Describe entities to be streamed from a lens indexed by the given indexer.
    let entitiesIndexedBy (lens : Lens<'a list, World>) (indexer : 'a -> int) (mapper : int -> Lens<'a, World> -> World -> EntityContent) =
        let mapper = fun i (a : obj) world -> mapper i (a :?> Lens<obj, World> |> Lens.map (cast<'a>)) world
        EntitiesFromStream (lens.Map box, Some (fun (o : obj) -> indexer (o :?> 'a)), mapper)

    /// Describe entities to be streamed from a lens indexed by fst.
    let entitiesIndexedByFst (lens : Lens<(int * 'a) list, World>) (mapper : int -> Lens<(int * 'a), World> -> World -> EntityContent) =
        entitiesIndexedBy lens fst mapper

    /// Describe entities to be streamed from a lens indexed by snd.
    let entitiesIndexedBySnd (lens : Lens<('a * int) list, World>) (mapper : int -> Lens<('a * int), World> -> World -> EntityContent) =
        entitiesIndexedBy lens snd mapper

    /// Describe entities to be streamed from a lens indexed by discriminated union tag.
    let entitiesIndexedByArray (lens : Lens<'a list, World>) (arr : 'a array) (mapper : int -> Lens<'a, World> -> World -> EntityContent) =
        let indexer a = Array.findIndex ((=) a) arr
        entitiesIndexedBy lens indexer mapper

    /// Describe entities to be streamed from a lens indexed by its union tag.
    let entitiesIndexedByTag (lens : Lens<'a list, World>) (mapper : int -> Lens<'a, World> -> World -> EntityContent) =
        let indexer (a : 'a) =
            let (unionCaseInfo, _) = FSharpValue.GetUnionFields (a :> obj, typeof<'a>)
            unionCaseInfo.Tag
        entitiesIndexedBy lens indexer mapper

    /// Describe entities to be streamed from a lens.
    let entities (lens : Lens<'a list, World>) (mapper : int -> Lens<'a, World> -> World -> EntityContent) =
        let mapper = fun i (a : obj) world -> mapper i (a :?> Lens<obj, World> |> Lens.map (cast<'a>)) world
        EntitiesFromStream (lens.Map box, None, mapper)

    /// Describe an entity to be optionally streamed from a lens.
    let entityOpt (lens : Lens<'a option, World>) (mapper : Lens<'a, World> -> World -> EntityContent) =
        entities (lens --> function Some a -> [a] | None -> []) (fun _ -> mapper)

    /// Describe an entity to be optionally streamed from a lens.
    let entityIf (lens : Lens<bool, World>) (mapper : Lens<unit, World> -> World -> EntityContent) =
        entities (lens --> function true -> [()] | false -> []) (fun _ -> mapper)

    /// Describe an entity to be streamed when a screen is selected.
    let entityIfScreenSelected (screen : Screen) (mapper : Lens<unit, World> -> World -> EntityContent) =
        entityIf (Default.Game.SelectedScreenOpt --> fun screenOpt -> screenOpt = Some screen) mapper

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