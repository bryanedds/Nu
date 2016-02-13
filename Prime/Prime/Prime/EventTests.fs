// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Prime.Tests
open System
open System.IO
open System.Xml
open Xunit
open Prime
module EventTests =

    type [<StructuralEquality; NoComparison>] TestParticipant =
        { TestAddress : TestParticipant Address }
        interface Participant with
            member this.ParticipantAddress = atoa<TestParticipant, Participant> this.TestAddress
            member this.GetPublishingPriority _ _ = 0.0f
            end

    type [<ReferenceEquality>] TestWorld =
        { TestState : int
          TestEventSystem : TestWorld EventSystem }
        interface TestWorld Eventable with
            member this.GetLiveness () = Running
            member this.GetEventSystem () = this.TestEventSystem
            member this.UpdateEventSystem updater = { this with TestEventSystem = updater this.TestEventSystem }
            member this.ContainsParticipant _ = true
            member this.PublishEvent (participant : Participant) publisher eventData eventAddress eventTrace subscription world = 
                Eventable.publishEvent<'a, 'p, TestParticipant, TestWorld> participant publisher eventData eventAddress eventTrace subscription world
        static member make () =
            { TestState = 0
              TestEventSystem = EventSystem.make () }

    [<RequireQualifiedAccess>]
    module Participants =
        let TestParticipant = { TestAddress = Address.empty<TestParticipant> }

    let UnitEventAddress = ntoa<unit> !!"Unit"
    let StringEventAddress = ntoa<string> !!"String"
    let TestFilePath = "TestFile.xml"
    let incTestStateAndCascade (_ : Event<unit, TestParticipant>) world = (Cascade, { world with TestState = inc world.TestState })
    let incTestStateAndResolve (_ : Event<unit, TestParticipant>) world = (Resolve, { world with TestState = inc world.TestState })

    let [<Fact>] subscribeWorks () =
        let world = TestWorld.make ()
        let world = Eventable.subscribe incTestStateAndCascade UnitEventAddress Participants.TestParticipant world
        let world = Eventable.publish () UnitEventAddress ["Test"] Participants.TestParticipant world
        Assert.Equal (1, world.TestState)

    let [<Fact>] subscribeAndPublishTwiceWorks () =
        let world = TestWorld.make ()
        let world = Eventable.subscribe incTestStateAndCascade UnitEventAddress Participants.TestParticipant world
        let world = Eventable.publish () UnitEventAddress ["Test"] Participants.TestParticipant world
        let world = Eventable.publish () UnitEventAddress ["Test"] Participants.TestParticipant world
        Assert.Equal (2, world.TestState)

    let [<Fact>] subscribeTwiceAndPublishWorks () =
        let world = TestWorld.make ()
        let world = Eventable.subscribe incTestStateAndCascade UnitEventAddress Participants.TestParticipant world
        let world = Eventable.subscribe incTestStateAndCascade UnitEventAddress Participants.TestParticipant world
        let world = Eventable.publish () UnitEventAddress ["Test"] Participants.TestParticipant world
        Assert.Equal (2, world.TestState)

    let [<Fact>] subscribeWithResolutionWorks () =
        let world = TestWorld.make ()
        let world = Eventable.subscribe incTestStateAndCascade UnitEventAddress Participants.TestParticipant world
        let world = Eventable.subscribe incTestStateAndResolve UnitEventAddress Participants.TestParticipant world
        let world = Eventable.publish () UnitEventAddress ["Test"] Participants.TestParticipant world
        Assert.Equal (1, world.TestState)

    let [<Fact>] unsubscribeWorks () =
        let key = makeGuid ()
        let world = TestWorld.make ()
        let world = Eventable.subscribe5 key incTestStateAndResolve UnitEventAddress Participants.TestParticipant world
        let world = Eventable.unsubscribe key world
        let world = Eventable.publish () UnitEventAddress ["Test"] Participants.TestParticipant world
        Assert.Equal (0, world.TestState)