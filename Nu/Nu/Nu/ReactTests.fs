namespace Nu
open System
open System.IO
open System.Xml
open SDL2
open Xunit
open Prime
open Nu
open Nu.Constants
module ReactTests =

    let TestEventAddress = !* "Test"
    let incUserStateAndPropagate _ world = (Propagate, World.transformUserState incI world)
    let incUserStateAndResolve _ world = (Resolved, World.transformUserState incI world)

    let [<Fact>] subscribeWorks () =
        World.init ()
        let world = World.makeEmpty 0
        let reactor = React.subscribe TestEventAddress ^^ React.unto Address.empty incUserStateAndPropagate world
        let world = World.publish4 TestEventAddress Address.empty (NoData ()) reactor.World
        Assert.Equal (1, World.getUserState world)

    let [<Fact>] unsubscribeWorks () =
        World.init ()
        let world = World.makeEmpty 0
        let reactor = React.subscribe TestEventAddress ^^ React.unto Address.empty incUserStateAndPropagate world
        let world = reactor.Unsubscribe reactor.World
        let world = World.publish4 TestEventAddress Address.empty (NoData ()) world
        Assert.True <| Map.isEmpty world.Callbacks.Subscriptions
        Assert.Equal (0, World.getUserState world)

    let [<Fact>] filterWorks () =
        World.init ()
        let world = World.makeEmpty 0
        let reactor =
            React.filter (fun _ world -> World.getUserState world = 0) ^^
            React.unto Address.empty incUserStateAndPropagate world
        let reactor = React.subscribe TestEventAddress reactor
        let reactor = React.subscribe TestEventAddress reactor
        let world = World.publish4 TestEventAddress Address.empty (NoData ()) reactor.World
        Assert.Equal (1, World.getUserState world)

    let [<Fact>] mapWorks () =
        World.init ()
        let world = World.makeEmpty 0
        let reactor =
            React.subscribe TestEventAddress ^^
            React.map React.unwrapV ^^
            React.unto Address.empty (fun a world -> (Propagate, World.setUserState a world)) world
        let world = World.publish4 TestEventAddress Address.empty (UserData { UserValue = 0 }) reactor.World
        Assert.Equal (0, World.getUserState world)

    let [<Fact>] scanWorks () =
        World.init ()
        let world = World.makeEmpty 0
        let reactor =
            React.subscribe TestEventAddress ^^
            React.map React.unwrapV ^^
            React.scan (fun b a _ -> b + a) 0 ^^
            React.unto Address.empty (fun a world -> (Propagate, World.setUserState a world)) world
        let world = World.publish4 TestEventAddress Address.empty (UserData { UserValue = 1 }) reactor.World
        let world = World.publish4 TestEventAddress Address.empty (UserData { UserValue = 2 }) world
        Assert.Equal (3, World.getUserState world)

    let [<Fact>] scanDoesntLeaveGarbage () =
        World.init ()
        let world = World.makeEmpty 0
        let reactor =
            React.subscribe TestEventAddress ^^
            React.scan2 (fun a _ _ -> a) ^^
            React.unto Address.empty (fun _ world -> (Propagate, world)) world
        let world = World.publish4 TestEventAddress Address.empty (NoData ()) reactor.World
        let world = reactor.Unsubscribe world
        Assert.True <| Map.isEmpty world.Callbacks.CallbackStates