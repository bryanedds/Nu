﻿namespace BlazeVector
open System
open System.Numerics
open Prime
open Nu
open BlazeVector

type GameplayState =
    | Playing
    | Quit

// this extends the Screen API to expose the Gameplay model as well as the Quit event.
[<AutoOpen>]
module GameplayExtensions =
    type Screen with
        member this.GetGameplayState world : GameplayState = this.Get (nameof Screen.GameplayState) world
        member this.SetGameplayState (value : GameplayState) world = this.Set (nameof Screen.GameplayState) value world
        member this.GameplayState = lens (nameof Screen.GameplayState) this this.GetGameplayState this.SetGameplayState
        member this.GetScore world : int = this.Get (nameof Screen.Score) world
        member this.SetScore (value : int) world = this.Set (nameof Screen.Score) value world
        member this.Score = lens (nameof Screen.Score) this this.GetScore this.SetScore

// this is the dispatcher that defines the behavior of the screen where gameplay takes place.
type GameplayDispatcher () =
    inherit ScreenDispatcherImSim ()

    // here we define default property values
    static member Properties =
        [define Screen.GameplayState Quit
         define Screen.Score 0]

    // here we define the behavior of our gameplay
    override this.Process (selectionResults, screen, world) =

        // process initialization
        let initializing = FQueue.contains Select selectionResults
        if initializing then

            // reset score
            Simulants.Gameplay.SetScore 0 world

            // create stage sections from random section files
            for sectionIndex in 0 .. dec Constants.Gameplay.SectionCount do

                // load a random section from file (except the first section which is always 0)
                let section = Simulants.GameplaySection sectionIndex
                let sectionFilePath = if sectionIndex = 0 then Assets.Gameplay.SectionFilePaths.[0] else Gen.randomItem Assets.Gameplay.SectionFilePaths
                World.readGroupFromFile sectionFilePath (Some section.Name) section.Screen world |> ignore<Group>

                // shift all entities in the loaded section so that they go after the previously loaded section
                let sectionXShift = 1024.0f * single sectionIndex
                for sectionEntity in World.getEntities section world do
                    sectionEntity.SetPosition (sectionEntity.GetPosition world + v3 sectionXShift 0.0f 0.0f) world

        // process clean-up
        if FQueue.contains Deselecting selectionResults then

            // destroy stage sections that were created from section files
            for sectionIndex in 0 .. dec Constants.Gameplay.SectionCount do
                let section = Simulants.GameplaySection sectionIndex
                World.destroyGroup section world

        // process gameplay when selected
        if screen.GetSelected world then

            // declare scene group
            World.beginGroup Simulants.Gameplay.Name [] world

            // declare player
            World.doEntity<PlayerDispatcher> "Player"
                [if initializing then Entity.Position @= v3 -390.0f -50.0f 0.0f
                 Entity.Elevation .= 1.0f]
                world
            let player = world.DeclaredEntity

            // process scoring
            for section in 0 .. dec Constants.Gameplay.SectionCount do
                let deaths = World.doSubscription "Deaths" (Events.DeathEvent --> Simulants.GameplaySection section --> Address.Wildcard) world
                screen.Score.Map (fun score -> score + deaths.Length * 100) world

            // process player death
            let deaths = World.doSubscription "Deaths" player.DeathEvent world
            if screen.GetGameplayState world = Playing && FQueue.notEmpty deaths then
                screen.SetGameplayState Quit world
                World.playSound Constants.Audio.SoundVolumeDefault Assets.Gameplay.DeathSound world
        
            // process eye look
            if world.Advancing then
                let playerPosition = player.GetPosition world
                let playerSize = player.GetSize world
                let eyeCenter = world.Eye2dCenter
                let eyeSize = world.Eye2dSize
                let eyeCenter = v2 (playerPosition.X + playerSize.X * 0.5f + eyeSize.X * 0.33f) eyeCenter.Y
                World.setEye2dCenter eyeCenter world

            // declare score text
            World.doText "Score" [Entity.Position .= v3 260.0f 155.0f 0.0f; Entity.Elevation .= 10.0f; Entity.Text @= "Score: " + string (screen.GetScore world)] world

            // declare quit button
            let quit = World.doButton "Quit" [Entity.Position .= v3 232.0f -144.0f 0.0f; Entity.Elevation .= 10.0f; Entity.Text .= "Quit"] world
            if quit then screen.SetGameplayState Quit world

            // end scene declaration
            World.endGroup world