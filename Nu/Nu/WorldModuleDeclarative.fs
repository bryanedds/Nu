// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2018.

namespace Nu
open System
open Prime
open Nu

/// A marker interface for simulant descriptors.
type SimulantDescriptor =
    interface
        abstract Children : SimulantDescriptor list
        end

/// Describes a game value independent of the engine.
type [<NoComparison>] GameDescriptor =
    { GameDispatcherName : string
      GameProperties : Map<string, Symbol>
      Screens : ScreenDescriptor list }

    /// The empty game descriptor.
    static member empty =
        { GameDispatcherName = String.Empty
          GameProperties = Map.empty
          Screens = [] }

    interface SimulantDescriptor with
        member this.Children =
            this.Screens |> enumerable<SimulantDescriptor> |> List.ofSeq

/// Describes a screen value independent of the engine.
and [<NoComparison>] ScreenDescriptor =
    { ScreenDispatcherName : string
      ScreenProperties : Map<string, Symbol>
      Layers : LayerDescriptor list }

    /// The empty screen descriptor.
    static member empty =
        { ScreenDispatcherName = String.Empty
          ScreenProperties = Map.empty
          Layers = [] }

    /// Derive a name from the dispatcher.
    static member getNameOpt dispatcher =
        dispatcher.ScreenProperties |>
        Map.tryFind (Property? Name) |>
        Option.map symbolToValue<string>
          
    interface SimulantDescriptor with
        member this.Children =
            this.Layers |> enumerable<SimulantDescriptor> |> List.ofSeq

/// Describes a layer value independent of the engine.
and [<NoComparison>] LayerDescriptor =
    { LayerDispatcherName : string
      LayerProperties : Map<string, Symbol>
      Entities : EntityDescriptor list }

    /// The empty layer descriptor.
    static member empty =
        { LayerDispatcherName = String.Empty
          LayerProperties = Map.empty
          Entities = [] }

    /// Derive a name from the dispatcher.
    static member getNameOpt dispatcher =
        dispatcher.LayerProperties |>
        Map.tryFind (Property? Name) |>
        Option.map symbolToValue<string>

    interface SimulantDescriptor with
        member this.Children =
            this.Entities |> enumerable<SimulantDescriptor> |> List.ofSeq

/// Describes an entity value independent of the engine.
and [<NoComparison>] EntityDescriptor =
    { EntityDispatcherName : string
      EntityProperties : Map<string, Symbol> }

    /// The empty entity descriptor.
    static member empty =
        { EntityDispatcherName = String.Empty
          EntityProperties = Map.empty }

    /// Derive a name from the dispatcher.
    static member getNameOpt dispatcher =
        dispatcher.EntityProperties |>
        Map.tryFind (Property? Name) |>
        Option.map symbolToValue<string>

    interface SimulantDescriptor with
        member this.Children = []

type [<NoEquality; NoComparison>] DeclarativeDefinition =
    | PropertyDefinition of PropertyDefinition
    | Equate of string * World Lens * bool

/// Contains primitives for describing simulants.
module Describe =

    /// Describe a game with the given definitions and contained screens.
    let game3 dispatcherName (definitions : DeclarativeDefinition seq) (screens : ScreenDescriptor seq) (game : Game) world =
        let properties =
            definitions |>
            Seq.map (fun def -> match def with PropertyDefinition def -> Some (def.PropertyName, def.PropertyExpr) | Equate _ -> None) |>
            Seq.definitize |>
            Seq.map (mapSnd (function DefineExpr value -> value | VariableExpr fn -> fn world)) |>
            Seq.map (mapSnd valueToSymbol) |>
            Map.ofSeq
        let equations =
            definitions |>
            Seq.map (fun def -> match def with Equate (leftName, right, breaking) -> Some (leftName, game :> Simulant, right, breaking) | PropertyDefinition _ -> None) |>
            Seq.definitize |>
            Seq.toList
        let descriptor =
            { GameDispatcherName = dispatcherName
              GameProperties = properties
              Screens = List.ofSeq screens }
        (descriptor, equations)

    /// Describe a game with the given definitions and contained screens.
    let game<'d when 'd :> GameDispatcher> properties screens game world =
        game3 typeof<'d>.Name properties screens game world

    /// Describe a screen with the given definitions and contained layers.
    let screen3 dispatcherName (definitions : DeclarativeDefinition seq) (layers : LayerDescriptor seq) (screen : Screen) world =
        let properties =
            definitions |>
            Seq.map (fun def -> match def with PropertyDefinition def -> Some (def.PropertyName, def.PropertyExpr) | Equate _ -> None) |>
            Seq.definitize |>
            Seq.map (mapSnd (function DefineExpr value -> value | VariableExpr fn -> fn world)) |>
            Seq.map (mapSnd valueToSymbol) |>
            Map.ofSeq
        let equations =
            definitions |>
            Seq.map (fun def -> match def with Equate (leftName, right, breaking) -> Some (leftName, screen :> Simulant, right, breaking) | PropertyDefinition _ -> None) |>
            Seq.definitize |>
            Seq.toList
        let descriptor =
            { ScreenDispatcherName = dispatcherName
              ScreenProperties = properties
              Layers = List.ofSeq layers }
        (descriptor, equations)

    /// Describe a screen with the given definitions and contained layers.
    let screen<'d when 'd :> ScreenDispatcher> definitions layers screen world =
        screen3 typeof<'d>.Name definitions layers screen world

    /// Describe a layer with the given definitions and contained entities.
    let layer3 dispatcherName (definitions : DeclarativeDefinition seq) (entities : EntityDescriptor seq) (layer : Layer) world =
        let properties =
            definitions |>
            Seq.map (fun def -> match def with PropertyDefinition def -> Some (def.PropertyName, def.PropertyExpr) | Equate _ -> None) |>
            Seq.definitize |>
            Seq.map (mapSnd (function DefineExpr value -> value | VariableExpr fn -> fn world)) |>
            Seq.map (mapSnd valueToSymbol) |>
            Map.ofSeq
        let equations =
            definitions |>
            Seq.map (fun def -> match def with Equate (leftName, right, breaking) -> Some (leftName, layer :> Simulant, right, breaking) | PropertyDefinition _ -> None) |>
            Seq.definitize |>
            Seq.toList
        let descriptor =
            { LayerDispatcherName = dispatcherName
              LayerProperties = properties
              Entities = List.ofSeq entities }
        (descriptor, equations)

    /// Describe a layer with the given definitions and contained entities.
    let layer<'d when 'd :> LayerDispatcher> definitions entities world =
        layer3 typeof<'d>.Name definitions entities world

    /// Describe an entity with the given definitions.
    let entity2 dispatcherName (definitions : DeclarativeDefinition seq) (entity : Entity) world =
        let properties =
            definitions |>
            Seq.map (fun def -> match def with PropertyDefinition def -> Some (def.PropertyName, def.PropertyExpr) | Equate _ -> None) |>
            Seq.definitize |>
            Seq.map (mapSnd (function DefineExpr value -> value | VariableExpr fn -> fn world)) |>
            Seq.map (mapSnd valueToSymbol) |>
            Map.ofSeq
        let equations =
            definitions |>
            Seq.map (fun def -> match def with Equate (leftName, right, breaking) -> Some (leftName, entity :> Simulant, right, breaking) | PropertyDefinition _ -> None) |>
            Seq.definitize |>
            Seq.toList
        let descriptor =
            { EntityDispatcherName = dispatcherName
              EntityProperties = properties }
        (descriptor, equations)

    /// Describe an entity with the given definitions.
    let entity<'d when 'd :> EntityDispatcher> definitions world =
        entity2 typeof<'d>.Name definitions world

/// Describes the behavior of a screen.
type [<NoComparison>] ScreenBehavior =
    | Vanilla
    | OmniScreen
    | Dissolve of DissolveData
    | Splash of DissolveData * SplashData * Screen

/// Describes the layout of a simulant
type SimulantLayout = interface end

/// Describes the layout of an entity.
type [<NoEquality; NoComparison>] EntityLayout =
    | EntityFromDefinitions of string * string * DeclarativeDefinition list
    | EntityFromFile of string * string
    interface SimulantLayout

    /// Expand an entity layout to its constituent parts.
    static member expand nameOpt layout layer world =
        match layout with
        | EntityFromDefinitions (dispatcherName, name, definitions) ->
            let name = Option.getOrDefault name nameOpt
            let (descriptor, definitions) = Describe.entity2 dispatcherName definitions (layer => name) world
            Left (name, descriptor, definitions)
        | EntityFromFile (name, filePath) ->
            let name = Option.getOrDefault name nameOpt
            Right (name, filePath)

/// Describes the layout of a layer.
type [<NoEquality; NoComparison>] LayerLayout =
    | LayerFromDefinitions of string * string * DeclarativeDefinition list * EntityLayout list
    | LayerFromFile of string * string
    interface SimulantLayout
    
    /// Expand a layer layout to its constituent parts.
    static member expand layout screen world =
        match layout with
        | LayerFromDefinitions (dispatcherName, name, definitions, entities) ->
            let layer = screen => name
            let descriptorsPlusPlus = List.map (fun layout -> EntityLayout.expand None layout layer world) entities
            let descriptorsPlus = Either.getLeftValues descriptorsPlusPlus
            let descriptors = descriptorsPlus |> List.map (fun (entityName, descriptor, _) -> { descriptor with EntityProperties = Map.add (Property? Name) (valueToSymbol entityName) descriptor.EntityProperties })
            let equations = descriptorsPlus |> List.map __c |> List.concat
            let filePaths = Either.getRightValues descriptorsPlusPlus |> List.map (fun (entityName, path) -> (name, entityName, path))
            let (descriptor, equationsLayer) = Describe.layer3 dispatcherName definitions descriptors layer world
            let equationsAll = equations @ equationsLayer
            Left (name, descriptor, equationsAll, filePaths)
        | LayerFromFile (name, filePath) -> Right (name, filePath)

/// Describes the layout of a screen.
type [<NoEquality; NoComparison>] ScreenLayout =
    | ScreenFromDefinitions of string * string * ScreenBehavior * DeclarativeDefinition list * LayerLayout list
    | ScreenFromLayerFile of string * ScreenBehavior * Type * string
    | ScreenFromFile of string * ScreenBehavior * string
    interface SimulantLayout

    /// Expand a screen layout to its constituent parts.
    static member expand layout (_ : Game) world =
        match layout with
        | ScreenFromDefinitions (dispatcherName, name, behavior, definitions, layers) ->
            let screen = Screen name
            let descriptorsPlusPlus = List.map (fun layout -> LayerLayout.expand layout screen world) layers
            let descriptorsPlus = Either.getLeftValues descriptorsPlusPlus
            let descriptors = descriptorsPlus |> List.map (fun (layerName, descriptor, _, _) -> { descriptor with LayerProperties = Map.add (Property? Name) (valueToSymbol layerName) descriptor.LayerProperties })
            let equations = descriptorsPlus |> List.map (fun (_, _, equations, _) -> equations) |> List.concat
            let layerFilePaths = Either.getRightValues descriptorsPlusPlus |> List.map (fun (layerName, filePath) -> (name, layerName, filePath))
            let entityFilePaths = List.map (fun (_, _, _, filePaths) -> List.map (fun (layerName, entityName, filePath) -> (name, layerName, entityName, filePath)) filePaths) descriptorsPlus |> List.concat
            let (descriptor, equationsScreen) = Describe.screen3 dispatcherName definitions descriptors screen world
            let equationsAll = equations @ equationsScreen
            Left (name, descriptor, equationsAll, behavior, layerFilePaths, entityFilePaths)
        | ScreenFromLayerFile (name, behavior, ty, filePath) -> Right (name, behavior, Some ty, filePath)
        | ScreenFromFile (name, behavior, filePath) -> Right (name, behavior, None, filePath)

/// Describes the layout of a game.
type [<NoEquality; NoComparison>] GameLayout =
    | GameFromDefinitions of string * DeclarativeDefinition list * ScreenLayout list
    | GameFromFile of string
    interface SimulantLayout

    /// Expand a game layout to its constituent parts.
    static member expand layout world =
        match layout with
        | GameFromDefinitions (dispatcherName, definitions, screens) ->
            let game = Game ()
            let descriptorsPlusPlus = List.map (fun layout -> ScreenLayout.expand layout game world) screens
            let descriptorsPlus = Either.getLeftValues descriptorsPlusPlus
            let descriptors = List.map (fun (screenName, descriptor, _, _, _, _) -> { descriptor with ScreenProperties = Map.add (Property? Name) (valueToSymbol screenName) descriptor.ScreenProperties }) descriptorsPlus
            let equations = List.map (fun (_, _, equations, _, _, _) -> equations) descriptorsPlus |> List.concat
            let screenBehaviors = List.map (fun (screenName, _,  _, behavior, _, _) -> (screenName, behavior)) descriptorsPlus |> Map.ofList
            let screenFilePaths = Either.getRightValues descriptorsPlusPlus
            let layerFilePaths = List.map (fun (_, _, _, _, layerFilePaths, _) -> layerFilePaths) descriptorsPlus |> List.concat
            let entityFilePaths = List.map (fun (_, _, _, _, _, entityFilePaths) -> entityFilePaths) descriptorsPlus |> List.concat
            let (descriptor, equationsGame) = Describe.game3 dispatcherName definitions descriptors game world
            let equationsAll = equations @ equationsGame
            Left (descriptor, equationsAll, screenBehaviors, screenFilePaths, layerFilePaths, entityFilePaths)
        | GameFromFile filePath -> Right filePath

type [<NoEquality; NoComparison>] View =
    | Render of RenderDescriptor
    | PlaySound of single * Audio AssetTag
    | PlaySong of int * single * Audio AssetTag
    | FadeOutSong of int
    | StopSong
    | Effect of (World -> World)

[<AutoOpen>]
module DeclarativeOperators =

    /// Pair an empty list of commands with a model.
    let inline just model = (model, [])

    /// Declare an instruction to set a property.
    let init lens value = PropertyDefinition (define lens value)

    /// Declare an instruction to equate two properties.
    let equate (left : Lens<'a, World>) (right : Lens<'a, World>) breaking =
        if right.This :> obj |> isNull
        then failwith "Equate expects an authentic Lens where its This is not null."
        else Equate (left.Name, right, breaking)

    /// Declare an instruction to set a property.
    let inline (==) left right = init left right

    /// Declare an instruction to equate two properties.
    let inline (=|=) left right = equate left right false

    /// Declare an instruction to equate two properties.
    let inline (=/=) left right = equate left right true

module Declarative =
    let Game = Game.Prop
    let Screen = Screen.Prop
    let Layer = Layer.Prop
    let Entity = Entity.Prop