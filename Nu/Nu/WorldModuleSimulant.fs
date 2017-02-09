// Nu Game Engine.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Nu
open System
open Prime
open Nu

[<AutoOpen; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module SimulantOperators =

    /// Derive a screen from a name string.
    let (!>) screenNameStr = Screen (ntoa !!screenNameStr)

    /// Derivce a screen from a name.
    let ntos screenName = Screen (ntoa screenName)

    /// Derive an entity from its layer.
    let ltoe (layer : Layer) entityName = Entity (atoa<Layer, Entity> layer.LayerAddress ->- ntoa entityName)

    /// Derive layer from its screen.
    let stol (screen : Screen) layerName = Layer (atoa<Screen, Layer> screen.ScreenAddress ->- ntoa layerName)

    /// Derive an entity from its layer.
    let etol (entity : Entity) = !< entity

    /// Derive a screen from one of its layers.
    let ltos (layer : Layer) = Screen (Address.take<Layer, Screen> 1 layer.LayerAddress)

module Simulants =

    /// The game. Always exists.
    let Game = Game Address.empty

    /// The default screen - may or may not exist.
    let DefaultScreen = !> Constants.Engine.DefaultScreenName
    
    /// The default layer - may or may not exist.
    let DefaultLayer = DefaultScreen => Constants.Engine.DefaultLayerName
    
    /// The default entity - may or may not exist.
    let DefaultEntity = DefaultLayer => Constants.Engine.DefaultEntityName

[<AutoOpen>]
module WorldModuleSimulant =

    type World with

        static member simulantExists (simulant : Simulant) (world : World) =
            (world :> EventWorld<Game, World>).ParticipantExists simulant

        static member tryDeriveSimulant address =
            match Address.getNames address with
            | [] -> Some (Game Address.empty :> Simulant)
            | [_] -> Some (Screen (Address.changeType<Simulant, Screen> address) :> Simulant)
            | [_; _] -> Some (Layer (Address.changeType<Simulant, Layer> address) :> Simulant)
            | [_; _; _] -> Some (Entity (Address.changeType<Simulant, Entity> address) :> Simulant)
            | _ -> None
        
        static member deriveSimulant address =
            match World.tryDeriveSimulant address with
            | Some simulant -> simulant
            | None -> failwithf "Could not proxy simulant using address '%s'." (scstring address)

        static member internal tryGetSimulantProperty name (simulant : Simulant) world =
            match simulant with
            | :? Game -> World.tryGetGameProperty name world
            | :? Screen as screen -> World.tryGetScreenProperty name screen world
            | :? Layer as layer -> World.tryGetLayerProperty name layer world
            | :? Entity as entity -> World.tryGetEntityProperty name entity world
            | _ -> None

        static member internal getSimulantProperty name (simulant : Simulant) world =
            match simulant with
            | :? Game -> World.getGameProperty name world
            | :? Screen as screen -> World.getScreenProperty name screen world
            | :? Layer as layer -> World.getLayerProperty name layer world
            | :? Entity as entity -> World.getEntityProperty name entity world
            | _ -> failwithumf ()

        static member internal trySetSimulantProperty name property (simulant : Simulant) world =
            match simulant with
            | :? Game -> World.trySetGameProperty name property world
            | :? Screen as screen -> World.trySetScreenProperty name property screen world
            | :? Layer as layer -> World.trySetLayerProperty name property layer world
            | :? Entity as entity -> World.trySetEntityProperty name property entity world
            | _ -> (false, world)

        static member internal setSimulantProperty name property (simulant : Simulant) world =
            match simulant with
            | :? Game -> World.setGameProperty name property world
            | :? Screen as screen -> World.setScreenProperty name property screen world
            | :? Layer as layer -> World.setLayerProperty name property layer world
            | :? Entity as entity -> World.setEntityProperty name property entity world
            | _ -> failwithumf ()

        static member internal tryGetSimulantScriptFrame (simulant : Simulant) world =
            match simulant with
            | :? Game -> Some (World.getGameScriptFrameNp world)
            | :? Screen as screen -> Some (World.getScreenScriptFrameNp screen world)
            | :? Layer as layer -> Some (World.getLayerScriptFrameNp layer world)
            | :? Entity as entity ->
                match World.tryGetEntityProperty Property? ScriptFrameNp entity world with
                | Some scriptFrameProperty -> Some (fst scriptFrameProperty :?> Scripting.DeclarationFrame)
                | None -> None
            | _ -> failwithumf ()