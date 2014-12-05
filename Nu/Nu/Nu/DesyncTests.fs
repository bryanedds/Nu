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
        #if DEBUG
        let proc =
            desync {
                let! _ = call incUserState
                return! call incUserStateTwice }
        #else
        let proc = bind { Operation = incUserState; OptNext = None } (fun _ -> returnM incUserStateTwice)
        #endif
        let world = snd <| Desync.run tautology obs proc world
        let world = World.publish4 () UnitEventAddress GameAddress world
        Assert.Equal (2, World.getUserState world)
        let world = World.publish4 () UnitEventAddress GameAddress world
        Assert.Equal (3, World.getUserState world)
        Assert.True <| Map.isEmpty world.Callbacks.CallbackStates