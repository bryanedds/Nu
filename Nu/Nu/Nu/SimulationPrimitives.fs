// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2015.

namespace Nu
open System
open OpenTK
open Prime
open Nu
open Nu.Constants

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module TransitionDescriptor =

    /// Make a screen transition descriptor.
    let make transitionType =
        { TransitionType = transitionType
          TransitionLifetime = 0L
          OptDissolveImage = None }

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module GameState =

    /// Make a game state value.
    let make dispatcher =
        { Id = Core.makeId ()
          OptSelectedScreen = None
          PublishChanges = true
          CreationTimeStampNp = Core.getTimeStamp ()
          DispatcherNp = dispatcher
          Xtension = { XFields = Map.empty; CanDefault = false; Sealed = true }}

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module ScreenState =

    /// Make a screen state value.
    let make dispatcher optName =
        let id = Core.makeId ()
        { Id = id
          Name = match optName with Some name -> name | None -> acstring id
          TransitionStateNp = IdlingState
          TransitionTicksNp = 0L // TODO: roll this field into Incoming/OutcomingState values
          Incoming = TransitionDescriptor.make Incoming
          Outgoing = TransitionDescriptor.make Outgoing
          PublishChanges = true
          Persistent = true
          CreationTimeStampNp = Core.getTimeStamp ()
          DispatcherNp = dispatcher
          Xtension = { XFields = Map.empty; CanDefault = false; Sealed = true }}
      
[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module GroupState =

    /// Make a group state value.
    let make dispatcher optName =
        let id = Core.makeId ()
        { GroupState.Id = id
          Name = match optName with Some name -> name | None -> acstring id
          PublishChanges = true
          Persistent = true
          CreationTimeStampNp = Core.getTimeStamp ()
          DispatcherNp = dispatcher
          Xtension = { XFields = Map.empty; CanDefault = false; Sealed = true }}
      
[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module EntityState =

    /// Make an entity state value.
    let make dispatcher optOverlayName optName =
        let id = Core.makeId ()
        { Id = id
          Name = match optName with Some name -> name | None -> acstring id
          Position = Vector2.Zero
          Depth = 0.0f
          Size = DefaultEntitySize
          Rotation = 0.0f
          Visible = true
          ViewType = Relative
          PublishChanges = true
          Persistent = true
          CreationTimeStampNp = Core.getTimeStamp ()
          DispatcherNp = dispatcher
          FacetNames = []
          FacetsNp = []
          OptOverlayName = optOverlayName
          Xtension = { XFields = Map.empty; CanDefault = false; Sealed = true }}

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Game =

    /// Create a Game proxy from an address.
    let proxy address = { GameAddress = address }

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Screen =

    /// Create a Screen proxy from an address.
    let proxy address = { ScreenAddress = address }

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Group =

    /// Create a Group proxy from an address.
    let proxy address = { GroupAddress = address }

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Entity =

    /// Create an Entity proxy from an address.
    let proxy address = { EntityAddress = address }

[<AutoOpen>]
module SimulationOperators =

    /// Convert any type of address to a simulant's address.
    let atoua address = Address.changeType<'a, Simulant> address

    /// Convert any type of address to a screen's address.
    let atosa address = Address.changeType<'a, Screen> address

    /// Convert any type of address to a group's address.
    let atoga address = Address.changeType<'a, Group> address

    /// Convert any type of address to an entity's address.
    let atoea address = Address.changeType<'a, Entity> address

    /// Convert a group's address to an entity's by appending the entity's name at the end.
    let gatoea groupAddress entityName = Address.changeType<Group, Entity> groupAddress ->- ntoa entityName

    /// Convert a screen's address to a group's by appending the group's name at the end.
    let satoga screenAddress groupName = Address.changeType<Screen, Group> screenAddress ->- ntoa groupName

    /// Convert a screen's address to an entity's by appending the group and entity's names at the end.
    let satoea screenAddress groupName entityName = gatoea (satoga screenAddress groupName) entityName

    /// Convert an entity's address to a group's by removing the entity's name from the end.
    let eatoga entityAddress = Address.take<Entity, Group> 2 entityAddress

    /// Convert a group's address to a screen's by removing the group's name from the end.
    let gatosa groupAddress = Address.take<Group, Screen> 1 groupAddress

    /// Convert a entity's address to a screen's by removing the group and entity's names from the end.
    let eatosa entityAddress = Address.take<Entity, Screen> 1 entityAddress