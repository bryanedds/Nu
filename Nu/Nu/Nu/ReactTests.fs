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
    let incUserStateAndCascade _ world = (Cascade, World.transformUserState inc world)
    let incUserStateAndResolve _ world = (Resolve, World.transformUserState inc world)

    let [<Fact>] subscribeWorks () =
        World.init ()
        let world = World.makeEmpty 0
        let world = from<unit> TestEventAddress |> using incUserStateAndCascade |> subscribe world |> snd
        let world = World.publish4 TestEventAddress Address.empty () world
        Assert.Equal (1, World.getUserState world)

    let [<Fact>] subscribeTwiceUnsubscribeOnceWorks () =
        World.init ()
        let world = World.makeEmpty 0
        let observable = from<unit> TestEventAddress |> using incUserStateAndCascade
        let world = snd <| subscribe world observable
        let (unsubscribe, world) = subscribe world observable
        let world = unsubscribe world
        let world = World.publish4 TestEventAddress Address.empty () world
        Assert.Equal (1, World.getUserState world)

    let [<Fact>] unsubscribeWorks () =
        World.init ()
        let world = World.makeEmpty 0
        let (unsubscribe, world) = from<unit> TestEventAddress |> using incUserStateAndCascade |> subscribe world
        let world = unsubscribe world
        let world = World.publish4 TestEventAddress Address.empty () world
        Assert.True <| Map.isEmpty world.Callbacks.Subscriptions
        Assert.Equal (0, World.getUserState world)

    let [<Fact>] filterWorks () =
        World.init ()
        let world = World.makeEmpty 0
        let world =
            from<unit> TestEventAddress |>
            filter (fun _ world -> World.getUserState world = 0) |>
            using incUserStateAndCascade |>
            subscribe world |>
            snd
        let world = World.publish4 TestEventAddress Address.empty () world
        let world = World.publish4 TestEventAddress Address.empty () world
        Assert.Equal (1, World.getUserState world)

    let [<Fact>] mapWorks () =
        World.init ()
        let world = World.makeEmpty 0
        let world =
            from<int> TestEventAddress |>
            map (fun event _ -> event.Data * 2) |>
            using (fun event world -> (Cascade, World.setUserState event.Data world)) |>
            subscribe world |>
            snd
        let world = World.publish4 TestEventAddress Address.empty 1 world
        Assert.Equal (2, World.getUserState world)

    let [<Fact>] scanWorks () =
        World.init ()
        let world = World.makeEmpty 0
        let world =
            from<int> TestEventAddress |>
            scan (fun acc event _ -> acc + event.Data) 0 |>
            using (fun event world -> (Cascade, World.setUserState event.Data world)) |>
            subscribe world |>
            snd
        let world = World.publish4 TestEventAddress Address.empty 1 world
        let world = World.publish4 TestEventAddress Address.empty 2 world
        Assert.Equal (3, World.getUserState world)

    let [<Fact>] scanDoesntLeaveGarbage () =
        World.init ()
        let world = World.makeEmpty 0
        let (unsubscribe, world) = from<int> TestEventAddress |> scan2 (fun a _ _ -> a) |> subscribe world
        let world = World.publish4 TestEventAddress Address.empty 0 world
        let world = unsubscribe world
        Assert.True <| Map.isEmpty world.Callbacks.CallbackStates