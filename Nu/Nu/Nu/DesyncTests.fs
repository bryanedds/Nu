namespace Nu
open System
open Xunit
open Prime
open Nu
open Nu.Constants
open Nu.WorldConstants
open Nu.Observer
module DesyncTests =

    let UnitEventAddress = stoa<unit> "Test"
    let incUserState (_, world) = (Cascade, World.transformUserState inc world)
    let incUserStateTwice (_, world) = (Cascade, World.transformUserState (inc >> inc) world)

    let [<Fact>] desyncWorks () =
        World.init ()
        let world = World.makeEmpty 0
        let obs = observe UnitEventAddress (atooa UnitEventAddress)
        
        let proc2 =
            desync.Bind (call incUserState, fun () ->
                desync.For ([0 .. 1], fun _ ->
                    desync.Bind (pass (), fun () -> desync.Return incUserState)))
        ignore proc2
        
        let proc =
            desync {
                do! call incUserState
                //for i in [0 .. 1] do (fun i -> pass ())
                do! forM [0 .. 1] (fun i -> if i = 0 then pass () else call incUserState)
                return incUserStateTwice }
        
        let world = snd <| Desync.run tautology obs (proc ()) world
        let world = World.publish4 () UnitEventAddress GameAddress world
        Assert.Equal (1, World.getUserState world)
        let world = World.publish4 () UnitEventAddress GameAddress world
        Assert.Equal (1, World.getUserState world)
        let world = World.publish4 () UnitEventAddress GameAddress world
        Assert.Equal (2, World.getUserState world)
        let world = World.publish4 () UnitEventAddress GameAddress world
        Assert.Equal (4, World.getUserState world)
        Assert.True <| Map.isEmpty world.Callbacks.CallbackStates