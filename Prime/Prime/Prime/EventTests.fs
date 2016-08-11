// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Prime.Tests
open System
open System.IO
open Xunit
open Prime
open Prime.Stream
open Prime.Chain
module EventTests =

    type NullParticipant () =
        interface Participant with
            member this.ParticipantAddress = Address.makeFromName !!"Null"
            member this.GetPublishingPriority _ _ = 0.0f
            end

    type [<StructuralEquality; NoComparison>] TestParticipant =
        { TestAddress : TestParticipant Address }
        interface Participant with
            member this.ParticipantAddress = atoa<TestParticipant, Participant> this.TestAddress
            member this.GetPublishingPriority _ _ = 0.0f
            end

    type [<ReferenceEquality>] TestWorld =
        { TestState : int
          TestEventSystem : TestWorld EventSystem }
        interface TestWorld EventWorld with
            member this.GetLiveness () = Running
            member this.GetEventSystem () = this.TestEventSystem
            member this.GetNullParticipant () = NullParticipant () :> Participant
            member this.GetGlobalParticipant () = { TestAddress = Address.empty } :> Participant
            member this.UpdateEventSystem updater = { this with TestEventSystem = updater this.TestEventSystem }
            member this.ContainsParticipant _ = true
            member this.PublishEvent (participant : Participant) publisher eventData eventAddress eventTrace subscription world =
                match participant with
                | :? TestParticipant -> EventWorld.publishEvent<'a, 'p, TestParticipant, TestWorld> participant publisher eventData eventAddress eventTrace subscription world
                | _ -> EventWorld.publishEvent<'a, 'p, Participant, TestWorld> participant publisher eventData eventAddress eventTrace subscription world
        static member incTestState this =
            { this with TestState = inc this.TestState }
        static member make eventTracer eventTracing eventFilter =
            { TestState = 0; TestEventSystem = EventSystem.make eventTracer eventTracing eventFilter }

    let TestEvent = ntoa<int> !!"Inc"
    let TestParticipant = { TestAddress = Address.empty<TestParticipant> }
    let incTestState _ world = TestWorld.incTestState world
    let incTestStateNoEvent world = TestWorld.incTestState world
    let incTestStateTwice _ world = TestWorld.incTestState ^ TestWorld.incTestState world
    let incTestStateTwiceNoEvent world = TestWorld.incTestState ^ TestWorld.incTestState world
    let incTestStateAndCascade (_ : Event<int, TestParticipant>) world = (Cascade, TestWorld.incTestState world)
    let incTestStateAndResolve (_ : Event<int, TestParticipant>) world = (Resolve, TestWorld.incTestState world)

    let [<Fact>] subscribeWorks () =
        let world = TestWorld.make ignore false EventFilter.Empty
        let world = EventWorld.subscribe incTestStateAndCascade TestEvent TestParticipant world
        let world = EventWorld.publish 0 TestEvent EventTrace.empty TestParticipant world
        Assert.Equal (1, world.TestState)

    let [<Fact>] subscribeAndPublishTwiceWorks () =
        let world = TestWorld.make ignore false EventFilter.Empty
        let world = EventWorld.subscribe incTestStateAndCascade TestEvent TestParticipant world
        let world = EventWorld.publish 0 TestEvent EventTrace.empty TestParticipant world
        let world = EventWorld.publish 0 TestEvent EventTrace.empty TestParticipant world
        Assert.Equal (2, world.TestState)

    let [<Fact>] subscribeTwiceAndPublishWorks () =
        let world = TestWorld.make ignore false EventFilter.Empty
        let world = EventWorld.subscribe incTestStateAndCascade TestEvent TestParticipant world
        let world = EventWorld.subscribe incTestStateAndCascade TestEvent TestParticipant world
        let world = EventWorld.publish 0 TestEvent EventTrace.empty TestParticipant world
        Assert.Equal (2, world.TestState)

    let [<Fact>] subscribeWithResolutionWorks () =
        let world = TestWorld.make ignore false EventFilter.Empty
        let world = EventWorld.subscribe incTestStateAndCascade TestEvent TestParticipant world
        let world = EventWorld.subscribe incTestStateAndResolve TestEvent TestParticipant world
        let world = EventWorld.publish 0 TestEvent EventTrace.empty TestParticipant world
        Assert.Equal (1, world.TestState)

    let [<Fact>] unsubscribeWorks () =
        let key = makeGuid ()
        let world = TestWorld.make ignore false EventFilter.Empty
        let world = EventWorld.subscribe5 key incTestStateAndResolve TestEvent TestParticipant world
        let world = EventWorld.unsubscribe key world
        let world = EventWorld.publish 0 TestEvent EventTrace.empty TestParticipant world
        Assert.Equal (0, world.TestState)

    let [<Fact>] streamWorks () =
        let world = TestWorld.make ignore false EventFilter.Empty
        let world = stream TestEvent |> subscribe incTestStateAndCascade TestParticipant <| world
        let world = EventWorld.publish 0 TestEvent EventTrace.empty TestParticipant world
        Assert.Equal (1, world.TestState)

    let [<Fact>] streamSubscribeTwiceUnsubscribeOnceWorks () =
        let world = TestWorld.make ignore false EventFilter.Empty
        let stream = stream TestEvent
        let world = subscribe incTestStateAndCascade TestParticipant stream world
        let (unsubscribe, world) = subscribePlus incTestStateAndCascade TestParticipant stream world
        let world = unsubscribe world
        let world = EventWorld.publish 0 TestEvent EventTrace.empty TestParticipant world
        Assert.Equal (1, world.TestState)

    let [<Fact>] streamUnsubscribeWorks () =
        let world = TestWorld.make ignore false EventFilter.Empty
        let (unsubscribe, world) = stream TestEvent |> subscribePlus incTestStateAndCascade TestParticipant <| world
        let world = unsubscribe world
        let world = EventWorld.publish 0 TestEvent EventTrace.empty TestParticipant world
        Assert.True ^ Vmap.isEmpty ^ EventWorld.getSubscriptions world
        Assert.Equal (0, world.TestState)

    let [<Fact>] filterWorks () =
        let world = TestWorld.make ignore false EventFilter.Empty
        let world =
            stream TestEvent |>
            filter (fun _ world -> world.TestState = 0) |>
            subscribe incTestStateAndCascade TestParticipant <|
            world
        let world = EventWorld.publish 0 TestEvent EventTrace.empty TestParticipant world
        let world = EventWorld.publish 0 TestEvent EventTrace.empty TestParticipant world
        Assert.Equal (1, world.TestState)

    let [<Fact>] mapWorks () =
        let world = TestWorld.make ignore false EventFilter.Empty
        let world =
            stream TestEvent |>
            map (fun evt _ -> evt.Data * 2) |>
            subscribe (fun evt world -> (Cascade, { world with TestState = evt.Data })) TestParticipant <|
            world
        let world = EventWorld.publish 1 TestEvent EventTrace.empty TestParticipant world
        Assert.Equal (2, world.TestState)

    let [<Fact>] productWorks () =
        let world = TestWorld.make ignore false EventFilter.Empty
        let world =
            stream TestEvent |>
            product TestEvent |>
            subscribe (fun evt world -> (Cascade, { world with TestState = fst evt.Data + snd evt.Data })) TestParticipant <|
            world
        let world = EventWorld.publish 1 TestEvent EventTrace.empty TestParticipant world
        Assert.Equal (2, world.TestState)

    let [<Fact>] scanWorks () =
        let world = TestWorld.make ignore false EventFilter.Empty
        let world =
            stream TestEvent |>
            scan (fun acc evt _ -> acc + evt.Data) 0 |>
            subscribe (fun evt world -> (Cascade, { world with TestState = evt.Data })) TestParticipant <|
            world
        let world = EventWorld.publish 1 TestEvent EventTrace.empty TestParticipant world
        let world = EventWorld.publish 2 TestEvent EventTrace.empty TestParticipant world
        Assert.Equal (3, world.TestState)

    let [<Fact>] scanDoesntLeaveGarbage () =
        let world = TestWorld.make ignore false EventFilter.Empty
        let (unsubscribe, world) =
            stream TestEvent |>
            scan2 (fun a _ _ -> a) |>
            subscribePlus incTestStateAndCascade TestParticipant <|
            world
        let world = EventWorld.publish 0 TestEvent EventTrace.empty TestParticipant world
        let world = unsubscribe world
        Assert.True ^ Vmap.isEmpty ^ EventWorld.getSubscriptions world

    let [<Fact>] chainWorks () =
        
        // build everything
        let world = TestWorld.make ignore false EventFilter.Empty
        let chain =
            chain {
                let! e = next
                do! update ^ incTestState e
                do! react incTestStateNoEvent
                do! reactE incTestState
                do! pass
                do! loop 0 inc (fun i _ -> i < 2) (fun _ -> update incTestStateTwiceNoEvent) }
        let stream = stream TestEvent
        let world = Chain.runAssumingCascade chain stream world |> snd
        Assert.Equal (0, world.TestState)

        // assert the first publish executes the first chained operation
        let world = EventWorld.publish 1 TestEvent EventTrace.empty TestParticipant world
        Assert.Equal (1, world.TestState)

        // assert the second publish executes the second chained operation
        let world = EventWorld.publish 2 TestEvent EventTrace.empty TestParticipant world
        Assert.Equal (2, world.TestState)
        
        // and so on...
        let world = EventWorld.publish 3 TestEvent EventTrace.empty TestParticipant world
        Assert.Equal (3, world.TestState)
        
        // and so on...
        let world = EventWorld.publish 4 TestEvent EventTrace.empty TestParticipant world
        Assert.Equal (7, world.TestState)
        
        // assert no garbage is left over after chained computation is concluded
        Assert.True ^ Vmap.isEmpty ^ EventWorld.getSubscriptions world