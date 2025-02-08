﻿namespace BlazeVector
open System
open System.Numerics
open Prime
open Nu
open BlazeVector

// this is our top-level MMCC model type. It determines what state the game is in. To learn about MMCC in Nu, see -
// https://github.com/bryanedds/Nu/wiki/Model-View-Update-for-Games-via-MMCC
type BlazeVector =
    | Splash
    | Title
    | Credits
    | Gameplay
    
// this is our top-level MMCC message type.
type BlazeVectorMessage =
    | ShowTitle
    | ShowCredits
    | ShowGameplay
    interface Message
    
// this is our top-level MMCC command type. Commands are used instead of messages when the world is to be transformed.
type BlazeVectorCommand =
    | Exit
    interface Command
    
// this extends the Game API to expose the above MMCC model as a property.
[<AutoOpen>]
module BlazeVectorExtensions =
    type Game with
        member this.GetBlazeVector world = this.GetModelGeneric<BlazeVector> world
        member this.SetBlazeVector value world = this.SetModelGeneric<BlazeVector> value world
        member this.BlazeVector = this.ModelGeneric<BlazeVector> ()

// this is the dispatcher that customizes the top-level behavior of our game. In here, we create screens as content and
// bind them up with events and properties.
type BlazeVectorDispatcher () =
    inherit GameDispatcher<BlazeVector, BlazeVectorMessage, BlazeVectorCommand> (Splash)

    // here we define the game's properties and event handling
    override this.Definitions (blazeVector, _) =
        [Game.DesiredScreen :=
            match blazeVector with
            | Splash -> Desire Simulants.Splash
            | Title -> Desire Simulants.Title
            | Credits -> Desire Simulants.Credits
            | Gameplay -> Desire Simulants.Gameplay
         if blazeVector = Splash then Simulants.Splash.DeselectingEvent => ShowTitle
         Simulants.TitleCredits.ClickEvent => ShowCredits
         Simulants.TitlePlay.ClickEvent => ShowGameplay
         Simulants.TitleExit.ClickEvent => Exit
         Simulants.CreditsBack.ClickEvent => ShowTitle
         Simulants.Gameplay.QuitEvent => ShowTitle]

    // here we handle the above messages
    override this.Message (_, message, _, _) =
        match message with
        | ShowTitle -> just Title
        | ShowCredits -> just Credits
        | ShowGameplay -> just Gameplay

    // here we handle the above commands
    override this.Command (_, command, _, world) =
        match command with
        | Exit ->
            if world.Unaccompanied
            then just (World.exit world)
            else just world

    // here we describe the content of the game, including all of its screens.
    override this.Content (_, _) =
        [Content.screen Simulants.Splash.Name (Slide (Constants.Dissolve.Default, Constants.Slide.Default, None, Simulants.Title)) [] []
         Content.screenWithGroupFromFile Simulants.Title.Name (Dissolve (Constants.Dissolve.Default, Some Assets.Gui.MachinerySong)) Assets.Gui.TitleGroupFilePath [] []
         Content.screenWithGroupFromFile Simulants.Credits.Name (Dissolve (Constants.Dissolve.Default, Some Assets.Gui.MachinerySong)) Assets.Gui.CreditsGroupFilePath [] []
         Content.screen<GameplayDispatcher> Simulants.Gameplay.Name (Dissolve (Constants.Dissolve.Default, Some Assets.Gameplay.DeadBlazeSong)) [] []]