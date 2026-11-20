// Zombie Game.
// Copyright (C) Bryan Edds.

namespace ZombieGame
open System
open System.Numerics
open Prime
open Nu

/// Whether a save prompt has yes highlighted, beside the intent it's given - and saving chosen at it, raised for the
/// gameplay screen to do.
[<AutoOpen>]
module SavePromptDispatcherExtensions =
    type Entity with
        member this.GetYesHighlighted world : bool = this.Get (nameof this.YesHighlighted) world
        member this.SetYesHighlighted (value : bool) world = this.Set (nameof this.YesHighlighted) value world
        member this.YesHighlighted = lens (nameof this.YesHighlighted) this this.GetYesHighlighted this.SetYesHighlighted
        member this.SaveEvent = Events.SaveEvent --> this

/// The typewriter asking whether to save - yes or no, one highlighted.
type SavePromptDispatcher () =
    inherit GuiDispatcherImSim ()

    /// Answer the player's intent - choosing yes or no, raising saving on yes and closing on no.
    static let answer (intent : Intent) (entity : Entity) world =
        if intent.Move.X <> 0 then
            entity.SetYesHighlighted (intent.Move.X < 0) world
            World.playSound 0.0f 0.0f 0.35f Assets.Gameplay.MenuMoveSound world
        elif intent.Back then
            World.publish () entity.CloseEvent entity world
        elif intent.Act then
            if entity.GetYesHighlighted world
            then World.publish () entity.SaveEvent entity world
            else World.publish () entity.CloseEvent entity world

    static member Properties =
        [define Entity.YesHighlighted true
         define Entity.Intent Intent.empty]

    override this.Process (entity, world) =

        // answer the player's intent
        if world.TimeAdvancing then answer (entity.GetIntent world) entity world

        // the question, and yes and no
        let yes = entity.GetYesHighlighted world
        World.doText "PromptBox"
            [Entity.Position .= v3 0.0f -130.0f 0.0f
             Entity.Size .= v3 600.0f 56.0f 0.0f
             Entity.Text .= "Save your progress?"
             Entity.TextColor .= Color.White
             Entity.BackdropImageOpt .= Some Assets.Default.Black
             Entity.Color .= Color.White.WithA 0.78f] world
        World.doText "PromptYes"
            [Entity.Position .= v3 -50.0f -148.0f 0.0f
             Entity.Size .= v3 60.0f 16.0f 0.0f
             Entity.Text .= "Yes"
             Entity.TextColor @= (if yes then Constants.Gui.BrightColor else Constants.Gui.DimColor)] world
        World.doText "PromptNo"
            [Entity.Position .= v3 50.0f -148.0f 0.0f
             Entity.Size .= v3 60.0f 16.0f 0.0f
             Entity.Text .= "No"
             Entity.TextColor @= (if not yes then Constants.Gui.BrightColor else Constants.Gui.DimColor)] world
