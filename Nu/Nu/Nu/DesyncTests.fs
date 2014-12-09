namespace Nu
open System
open Xunit
open Prime
open Nu
open Nu.Constants
open Nu.WorldConstants
open Nu.Observer
open Nu.Desync
module DesyncTests =

    let IntEventAddress = stoa<int> "Test"
    let incUserState _ world = World.transformUserState inc world
    let incUserStateNoEvent world = World.transformUserState inc world
    let incUserStateTwice _ world = World.transformUserState (inc >> inc) world
    let incUserStateTwiceNoEvent world = World.transformUserState (inc >> inc) world

    let [<Fact>] desyncWorks () =
        
        // build everything
        World.init ()
        let world = World.makeEmpty 0
        let proc =
            desync {
                do! waitE (fun e -> call <| incUserState e)
                do! wait <| call incUserStateNoEvent
                do! reactE incUserState
                do! react incUserStateNoEvent
                do! wait <| loop 0 inc (fun i -> i < 2) (fun _ -> (call incUserStateTwiceNoEvent)) }
        let obs = observe IntEventAddress GameAddress
        let world = snd <| Desync.runDesyncAssumingCascade obs proc world
        
        // assert the first publish executes the first desync'd operation
        let world = World.publish4 0 IntEventAddress GameAddress world
        Assert.Equal (0, World.getUserState world)

        // assert the second publish executes the second desync'd operation
        let world = World.publish4 1 IntEventAddress GameAddress world
        Assert.Equal (1, World.getUserState world)

        // assert the second publish executes the second desync'd operation
        let world = World.publish4 2 IntEventAddress GameAddress world
        Assert.Equal (2, World.getUserState world)

        // assert the second publish executes the second desync'd operation
        let world = World.publish4 3 IntEventAddress GameAddress world
        Assert.Equal (3, World.getUserState world)

        // and so on...
        let world = World.publish4 4 IntEventAddress GameAddress world
        Assert.Equal (4, World.getUserState world)
        
        // and so on...
        let world = World.publish4 5 IntEventAddress GameAddress world
        Assert.Equal (8, World.getUserState world)
        
        // assert no garbage is left over after desync'd computation is concluded
        Assert.True <| Map.isEmpty world.Callbacks.CallbackStates