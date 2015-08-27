// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2015.

namespace Nu.Tests
open System
open Xunit
open Prime
open Nu
open Nu.Observation
open Nu.Chain
module ChainTests =

    let IntEventAddress = ntoa<int> "Test"
    let incUserState _ world = World.updateUserState inc world
    let incUserStateNoEvent world = World.updateUserState inc world
    let incUserStateTwice _ world = World.updateUserState (inc >> inc) world
    let incUserStateTwiceNoEvent world = World.updateUserState (inc >> inc) world

    let [<Fact>] chainWorks () =
        
        // build everything
        World.init ()
        let world = World.makeEmpty 0
        let chain =
            chain {
                let! e = next
                do! update ^ incUserState e
                do! react incUserStateNoEvent
                do! reactE incUserState
                do! pass
                do! loop 0 inc (fun i _ -> i < 2) (fun _ -> update incUserStateTwiceNoEvent) }
        let observation = observe IntEventAddress Simulants.Game
        let world = snd ^ Chain.runAssumingCascade chain observation world
        Assert.Equal (0, World.getUserState world)

        // assert the first publish executes the first chained operation
        let world = World.publish4 1 IntEventAddress Simulants.Game world
        Assert.Equal (1, World.getUserState world)

        // assert the second publish executes the second chained operation
        let world = World.publish4 2 IntEventAddress Simulants.Game world
        Assert.Equal (2, World.getUserState world)
        
        // and so on...
        let world = World.publish4 3 IntEventAddress Simulants.Game world
        Assert.Equal (3, World.getUserState world)
        
        // and so on...
        let world = World.publish4 4 IntEventAddress Simulants.Game world
        Assert.Equal (7, World.getUserState world)
        
        // assert no garbage is left over after chained computation is concluded
        Assert.True ^ Map.isEmpty world.Callbacks.CallbackStates