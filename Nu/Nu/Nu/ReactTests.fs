namespace Nu
open System
open System.IO
open System.Xml
open SDL2
open Xunit
open Prime
open Nu
open Nu.Constants
open Nu.React
module ReactTests =

    let TestEventAddress = !* "Test"
    let incUserStateAndPropagate _ world = (Propagate, World.transformUserState incI world)
    let incUserStateAndResolve _ world = (Resolved, World.transformUserState incI world)

    let [<Fact>] subscribeWorks () =
        World.init ()
        let world = World.makeEmpty 0
        let world = subscribeUpon TestEventAddress world ^^ using Address.empty incUserStateAndPropagate
        let world = World.publish4 TestEventAddress Address.empty (NoData ()) world
        Assert.Equal (1, World.getUserState world)

    let [<Fact>] unsubscribeWorks () =
        World.init ()
        let world = World.makeEmpty 0
        let reactor = upon TestEventAddress ^^ using Address.empty incUserStateAndPropagate
        let world = subscribe world reactor
        let world = unsubscribe world reactor
        let world = World.publish4 TestEventAddress Address.empty (NoData ()) world
        Assert.True <| Map.isEmpty world.Callbacks.Subscriptions
        Assert.Equal (0, World.getUserState world)

    let [<Fact>] filterWorks () =
        World.init ()
        let world = World.makeEmpty 0
        let reactor =
            upon TestEventAddress ^^
            filter (fun _ world -> World.getUserState world = 0) ^^
            using Address.empty incUserStateAndPropagate
        let world = subscribe world reactor
        let world = subscribe world reactor
        let world = World.publish4 TestEventAddress Address.empty (NoData ()) world
        Assert.Equal (1, World.getUserState world)

    let [<Fact>] mapWorks () =
        World.init ()
        let world = World.makeEmpty 0
        let world =
            subscribeUpon TestEventAddress world ^^
            map unwrapV ^^
            using Address.empty (fun a world -> (Propagate, World.setUserState a world))
        let world = World.publish4 TestEventAddress Address.empty (UserData { UserValue = 0 }) world
        Assert.Equal (0, World.getUserState world)

    let [<Fact>] scanWorks () =
        World.init ()
        let world = World.makeEmpty 0
        let world =
            subscribeUpon TestEventAddress world ^^
            map unwrapV ^^
            scan (fun b a _ -> b + a) 0 ^^
            using Address.empty (fun a world -> (Propagate, World.setUserState a world))
        let world = World.publish4 TestEventAddress Address.empty (UserData { UserValue = 1 }) world
        let world = World.publish4 TestEventAddress Address.empty (UserData { UserValue = 2 }) world
        Assert.Equal (3, World.getUserState world)

    let [<Fact>] scanDoesntLeaveGarbage () =
        World.init ()
        let world = World.makeEmpty 0
        let reactor =
            upon TestEventAddress ^^
            scan2 (fun a _ _ -> a) ^^
            using Address.empty (fun _ world -> (Propagate, world))
        let world = subscribe world reactor
        let world = World.publish4 TestEventAddress Address.empty (NoData ()) world
        let world = unsubscribe world reactor
        Assert.True <| Map.isEmpty world.Callbacks.CallbackStates