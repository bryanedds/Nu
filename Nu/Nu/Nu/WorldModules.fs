// Nu Game Engine.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Nu
open System
open OpenTK
open Prime
open Nu

[<AutoOpen; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module SimulantOperators =

    /// Convert a name string to a screen's proxy.
    let (!>) screenNameStr = Screen.proxy ^ ntoa !!screenNameStr

    /// Convert a name to a screen's proxy.
    let ntos screenName = Screen.proxy ^ ntoa screenName

    /// Convert a group's proxy to an entity's by appending the entity's name at the end.
    let gtoe (group : Group) entityName = Entity.proxy ^ atoa<Group, Entity> group.GroupAddress ->- ntoa entityName

    /// Convert a screen's proxy to a group's by appending the group's name at the end.
    let stog (screen : Screen) groupName = Group.proxy ^ atoa<Screen, Group> screen.ScreenAddress ->- ntoa groupName

    /// Convert an entity's proxy to a group's by removing the entity's name from the end.
    let etog (entity : Entity) = !< entity

    /// Convert a group's proxy to a screen's by removing the group's name from the end.
    let gtos group = Screen.proxy ^ Address.take<Group, Screen> 1 group.GroupAddress

module Simulants =

    /// The game. Always exists.
    let Game = { GameAddress = Address.empty }

    /// The default screen - may or may not exist.
    let DefaultScreen = !> Constants.Engine.DefaultScreenName
    
    /// The default group - may or may not exist.
    let DefaultGroup = DefaultScreen => Constants.Engine.DefaultGroupName
    
    /// The default entity - may or may not exist.
    let DefaultEntity = DefaultGroup => Constants.Engine.DefaultEntityName

module Descriptors =

    /// Describe a game with the given properties values and contained screens.
    let Game<'d when 'd :> GameDispatcher> properties screens =
        { GameDispatcher = typeof<'d>.Name
          GameProperties = Map.ofSeq properties
          Screens = List.ofSeq screens }

    /// Describe a screen with the given properties values and contained groups.
    let Screen<'d when 'd :> ScreenDispatcher> properties groups =
        { ScreenDispatcher = typeof<'d>.Name
          ScreenProperties = Map.ofSeq properties
          Groups = List.ofSeq groups }

    /// Describe a group with the given properties values and contained entities.
    let Group<'d when 'd :> GroupDispatcher> properties entities =
        { GroupDispatcher = typeof<'d>.Name
          GroupProperties = Map.ofSeq properties
          Entities = List.ofSeq entities }

    /// Describe an entity with the given properties values.
    let Entity<'d when 'd :> EntityDispatcher> properties =
        { EntityDispatcher = typeof<'d>.Name
          EntityProperties = Map.ofSeq properties }

[<AutoOpen>]
module WorldEventHandlerModule =

    type World with

        /// Ignore all handled events.
        static member handleAsPass<'a, 's when 's :> Simulant> (_ : Event<'a, 's>) (world : World) =
            (Cascade, world)

        /// Swallow all handled events.
        static member handleAsSwallow<'a, 's when 's :> Simulant> (_ : Event<'a, 's>) (world : World) =
            (Resolve, world)
            
        /// Handle event by exiting app.
        static member handleAsExit<'a, 's when 's :> Simulant> (_ : Event<'a, 's>) (world : World) =
            (Resolve, World.exit world)