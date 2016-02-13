// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Prime.Tests
open System
open System.IO
open System.Xml
open Xunit
open Prime
open Prime.Observation
open Prime.Chain
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
            member this.PublishEvent
                (participant : Participant) publisher eventData eventAddress eventTrace subscription world = 
                Eventable.publishEvent<'a, 'p, TestParticipant, TestWorld> participant publisher eventData eventAddress eventTrace subscription world
        static member incTestState this = { this with TestState = inc this.TestState }
        static member make () = { TestState = 0; TestEventSystem = EventSystem.make () }

    [<RequireQualifiedAccess>]
    module Participants =
        let TestParticipant = { TestAddress = Address.empty<TestParticipant> }

    let IntEventAddress = ntoa<int> !!"Int"
    let UnitEventAddress = ntoa<unit> !!"Unit"
    let StringEventAddress = ntoa<string> !!"String"
    let incTestState _ world = TestWorld.incTestState world
    let incTestStateNoEvent world = TestWorld.incTestState world
    let incTestStateTwice _ world = TestWorld.incTestState ^ TestWorld.incTestState world
    let incTestStateTwiceNoEvent world = TestWorld.incTestState ^ TestWorld.incTestState world
    let incTestStateAndCascade (_ : Event<unit, TestParticipant>) world = (Cascade, TestWorld.incTestState world)
    let incTestStateAndResolve (_ : Event<unit, TestParticipant>) world = (Resolve, TestWorld.incTestState world)

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
        
    let [<Fact>] chainWorks () =
        
        // build everything
        let world = TestWorld.make ()
        let chain =
            chain {
                let! e = next
                do! update ^ incTestState e
                do! react incTestStateNoEvent
                do! reactE incTestState
                do! pass
                do! loop 0 inc (fun i _ -> i < 2) (fun _ -> update incTestStateTwiceNoEvent) }
        let observation = observe IntEventAddress Participants.TestParticipant
        let world = Chain.runAssumingCascade chain observation world |> snd
        Assert.Equal (0, world.TestState)

        // assert the first publish executes the first chained operation
        let world = Eventable.publish 1 IntEventAddress ["Test"] Participants.TestParticipant world
        Assert.Equal (1, world.TestState)

        // assert the second publish executes the second chained operation
        let world = Eventable.publish 2 IntEventAddress ["Test"] Participants.TestParticipant world
        Assert.Equal (2, world.TestState)
        
        // and so on...
        let world = Eventable.publish 3 IntEventAddress ["Test"] Participants.TestParticipant world
        Assert.Equal (3, world.TestState)
        
        // and so on...
        let world = Eventable.publish 4 IntEventAddress ["Test"] Participants.TestParticipant world
        Assert.Equal (7, world.TestState)
        
        // assert no garbage is left over after chained computation is concluded
        Assert.True ^ Vmap.isEmpty ^ Eventable.getSubscriptions world