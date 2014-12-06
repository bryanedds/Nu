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
        World.init ()
        let world = World.makeEmpty 0
        let obs = observe UnitEventAddress (atooa UnitEventAddress)
        
        let proc =
            desync {
                do! call incUserState
                do! loop [0 .. 1] (fun i ->
                    if i = 0 then call incUserState
                    else pass ())
                return incUserStateTwice }

        let world = snd <| Desync.runWithEventSpecifyingHandling tautology obs proc world
        let world = World.publish4 () UnitEventAddress GameAddress world
        Assert.Equal (1, World.getUserState world)
        let world = World.publish4 () UnitEventAddress GameAddress world
        Assert.Equal (2, World.getUserState world)
        let world = World.publish4 () UnitEventAddress GameAddress world
        Assert.Equal (2, World.getUserState world)
        let world = World.publish4 () UnitEventAddress GameAddress world
        Assert.Equal (4, World.getUserState world)
        Assert.True <| Map.isEmpty world.Callbacks.CallbackStates