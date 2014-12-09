namespace Nu
open System
open Xunit
open Prime
open Nu
open Nu.Constants
open Nu.WorldConstants
open Nu.Observer
open Nu.Unsync
module DesyncTests =

    let UnitEventAddress = stoa<unit> "Test"
    let incUserState _ world = (Cascade, World.transformUserState inc world)
    let incUserStateTwice _ world = (Cascade, World.transformUserState (inc >> inc) world)

    let [<Fact>] desyncWorks () =
        
        // build everything
        World.init ()
        let world = World.makeEmpty 0
        let proc =
            unsync {

                do! wait <|
                    unsync {
                        let! world = get
                        let world = World.transformUserState inc world
                        do! put world }
                
                do! wait <|
                    unsync {
                        let! world = get
                        let world = World.transformUserState inc world
                        do! put world }
                
                do! wait <|
                    unsync {
                        let! world = get
                        let world = World.transformUserState (inc >> inc) world
                        do! put world }}

        let obs = observe UnitEventAddress GameAddress
        let world = snd <| Unsync.runDesyncAssumingCascade obs proc world
        
        // assert the first publish executes the first desync'd operation
        let world = World.publish4 () UnitEventAddress GameAddress world
        Assert.Equal (0, World.getUserState world)

        // assert the second publish executes the second desync'd operation
        let world = World.publish4 () UnitEventAddress GameAddress world
        Assert.Equal (1, World.getUserState world)

        // and so on...
        let world = World.publish4 () UnitEventAddress GameAddress world
        Assert.Equal (2, World.getUserState world)
        
        // and so on...
        let world = World.publish4 () UnitEventAddress GameAddress world
        Assert.Equal (4, World.getUserState world)
        
        // assert no garbage is left over after desync'd computation is concluded
        Assert.True <| Map.isEmpty world.Callbacks.CallbackStates