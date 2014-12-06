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

    let UnitEventAddress = stoa<unit> "Test"
    let incUserState _ world = (Cascade, World.transformUserState inc world)
    let incUserStateTwice _ world = (Cascade, World.transformUserState (inc >> inc) world)

    let [<Fact>] desyncWorks () =
        
        // build everything
        World.init ()
        let world = World.makeEmpty 0
        let proc =
            desync {
                do! call incUserState
                do! loop [0 .. 1] (fun i ->
                    if i = 0 then call incUserState
                    else pass ())
                //if 1 = 2 then pass () else skip () // WHY CAN'T I GET THIS TO COMPILE (other than Delay missing)?
                return! call incUserStateTwice }
        let obs = observe UnitEventAddress (atooa UnitEventAddress)
        let world = snd <| Desync.runDesyncReferencingEventsSpecifyingHandling tautology obs proc world
        
        // assert the first publish executes the first desync'd operation
        let world = World.publish4 () UnitEventAddress GameAddress world
        Assert.Equal (1, World.getUserState world)

        // assert the second publish executes the second desync'd operation
        let world = World.publish4 () UnitEventAddress GameAddress world
        Assert.Equal (2, World.getUserState world)
        
        // and so on...
        let world = World.publish4 () UnitEventAddress GameAddress world
        Assert.Equal (2, World.getUserState world)
        
        // and so on...
        let world = World.publish4 () UnitEventAddress GameAddress world
        Assert.Equal (4, World.getUserState world)
        
        // assert no garbage is left over after desync'd computation is concluded
        Assert.True <| Map.isEmpty world.Callbacks.CallbackStates