// Nu Game Engine.
// Required Notice:
// Copyright (C) Bryan Edds.
// Nu Game Engine is licensed under the Nu Game Engine Noncommercial License.
// See https://github.com/bryanedds/Nu/blob/master/License.md.

namespace Nu.Tests
open System
open NUnit.Framework
open Prime
open Nu
open Nu.Tests

/// A facet whose event handler is found by name.
type NamedHandlerTestFacet () =
    inherit Facet (false, false, false)

    static let handleTestEvent (evt : Event<int, Entity>) (_ : World) =
        NamedHandlerTestFacet.Handled <- NamedHandlerTestFacet.Handled + evt.Data
        Cascade

    static member val Handled = 0 with get, set
    static member HandleTestEventName = nameof handleTestEvent

/// An entity dispatcher whose event handlers are found by name.
type NamedHandlerTestDispatcher () =
    inherit Entity2dDispatcher (false, false, false)

    static let handleTestEvent (evt : Event<int, Entity>) (_ : World) =
        NamedHandlerTestDispatcher.Handled <- NamedHandlerTestDispatcher.Handled + evt.Data
        Cascade

    static member val Handled = 0 with get, set
    static member HandleTestEventName = nameof handleTestEvent

    static member HandleTestEventPublicly (evt : Event<int, Entity>, _ : World) =
        NamedHandlerTestDispatcher.Handled <- NamedHandlerTestDispatcher.Handled + evt.Data * 100
        Cascade

    static member HandleTestEventGenerically<'a> (_ : Event<'a, Entity>) (_ : World) =
        Cascade

/// An entity dispatcher that inherits its event handlers.
type NamedHandlerTestDerivedDispatcher () =
    inherit NamedHandlerTestDispatcher ()

/// An entity dispatcher with a same-named event handler that does something different.
type NamedHandlerTestOtherDispatcher () =
    inherit Entity2dDispatcher (false, false, false)

    static let handleTestEvent (evt : Event<int, Entity>) (_ : World) =
        NamedHandlerTestDispatcher.Handled <- NamedHandlerTestDispatcher.Handled + evt.Data * 10
        Cascade

    static member HandleTestEventName = nameof handleTestEvent

module NamedSubscriptionTests =

    let private TestEventAddress (entity : Entity) =
        rtoa<int> [|"Test"; "Event"|] --> entity.EntityAddress

    let private publishTestEvent data (entity : Entity) world =
        World.publishPlus data (TestEventAddress entity) EventTrace.empty entity false false world

    let private makeWorldWithGroup () =
        Nu.init ()
        let world = World.makeStub (constant None) { WorldConfig.defaultConfig with Accompanied = true } (TestPlugin ())
        let screen = World.createScreen<ScreenDispatcher> (Some "Screen") world
        let group = World.createGroup<GroupDispatcher> (Some "Group") screen world
        (group, world)

    let [<Test>] ``Named subscription finds static let handler on subscriber's dispatcher.`` () =
        let (group, world) = makeWorldWithGroup ()
        let entity = World.createEntity<NamedHandlerTestDispatcher> None DefaultOverlay (Some [|"Entity"|]) group world
        NamedHandlerTestDispatcher.Handled <- 0
        let unsubscribe = World.subscribePlus Gen.id64 NamedHandlerTestDispatcher.HandleTestEventName (TestEventAddress entity) entity world
        publishTestEvent 2 entity world
        publishTestEvent 3 entity world
        Assert.Equal (5, NamedHandlerTestDispatcher.Handled)
        unsubscribe world
        publishTestEvent 7 entity world
        Assert.Equal (5, NamedHandlerTestDispatcher.Handled)
        World.cleanUp world

    let [<Test>] ``Named subscription finds public static handler, including on a base dispatcher.`` () =
        let (group, world) = makeWorldWithGroup ()
        let entity = World.createEntity<NamedHandlerTestDerivedDispatcher> None DefaultOverlay (Some [|"Entity"|]) group world
        NamedHandlerTestDispatcher.Handled <- 0
        World.subscribe (nameof NamedHandlerTestDispatcher.HandleTestEventPublicly) (TestEventAddress entity) entity world
        World.subscribe NamedHandlerTestDispatcher.HandleTestEventName (TestEventAddress entity) entity world
        publishTestEvent 1 entity world
        Assert.Equal (101, NamedHandlerTestDispatcher.Handled)
        World.cleanUp world

    let [<Test>] ``Named subscription to a missing or generic handler cascades without handling.`` () =
        let (group, world) = makeWorldWithGroup ()
        let entity = World.createEntity<NamedHandlerTestDispatcher> None DefaultOverlay (Some [|"Entity"|]) group world
        NamedHandlerTestDispatcher.Handled <- 0
        World.subscribe "handleNothing" (TestEventAddress entity) entity world
        World.subscribe (nameof NamedHandlerTestDispatcher.HandleTestEventGenerically) (TestEventAddress entity) entity world
        publishTestEvent 1 entity world
        Assert.Equal (0, NamedHandlerTestDispatcher.Handled)
        World.cleanUp world

    let [<Test>] ``Named subscription follows its subscriber's dispatcher when it changes.`` () =
        let (group, world) = makeWorldWithGroup ()
        let entity = World.createEntity<NamedHandlerTestDispatcher> None DefaultOverlay (Some [|"Entity"|]) group world
        NamedHandlerTestDispatcher.Handled <- 0
        World.subscribe NamedHandlerTestDispatcher.HandleTestEventName (TestEventAddress entity) entity world
        publishTestEvent 1 entity world
        Assert.Equal (1, NamedHandlerTestDispatcher.Handled)
        World.changeEntityDispatcher (nameof NamedHandlerTestOtherDispatcher) entity world
        publishTestEvent 1 entity world
        Assert.Equal (11, NamedHandlerTestDispatcher.Handled)
        World.cleanUp world

    let [<Test>] ``Named monitor stops handling when its subscriber is destroyed.`` () =
        let (group, world) = makeWorldWithGroup ()
        let entity = World.createEntity<NamedHandlerTestDispatcher> None DefaultOverlay (Some [|"Entity"|]) group world
        let publisher = World.createEntity<Entity2dDispatcher> None DefaultOverlay (Some [|"Publisher"|]) group world
        NamedHandlerTestDispatcher.Handled <- 0
        World.monitor NamedHandlerTestDispatcher.HandleTestEventName (TestEventAddress publisher) entity world
        publishTestEvent 2 publisher world
        Assert.Equal (2, NamedHandlerTestDispatcher.Handled)
        World.destroyEntityImmediate entity world
        publishTestEvent 3 publisher world
        Assert.Equal (2, NamedHandlerTestDispatcher.Handled)
        World.cleanUp world

    let [<Test>] ``Named sense finds handler on the facet and stops handling when the facet is removed.`` () =
        let (group, world) = makeWorldWithGroup ()
        let entity = World.createEntity<Entity2dDispatcher> None DefaultOverlay (Some [|"Entity"|]) group world
        entity.SetFacetNames (Set.singleton (nameof NamedHandlerTestFacet)) world
        NamedHandlerTestFacet.Handled <- 0
        World.sense NamedHandlerTestFacet.HandleTestEventName (TestEventAddress entity) entity (nameof NamedHandlerTestFacet) world
        publishTestEvent 2 entity world
        Assert.Equal (2, NamedHandlerTestFacet.Handled)
        entity.SetFacetNames Set.empty world
        publishTestEvent 3 entity world
        Assert.Equal (2, NamedHandlerTestFacet.Handled)
        World.cleanUp world