// Zombie Game.
// Copyright (C) Bryan Edds.

namespace ZombieGame
open System
open System.Numerics
open Prime
open Nu

/// What the credits show - the heading over them and the line after them, as well as the menu's heading - how long
/// they've rolled, and their finishing, raised for the game.
[<AutoOpen>]
module CreditsDispatcherExtensions =
    type Entity with
        member this.GetClosing world : string = this.Get (nameof this.Closing) world
        member this.SetClosing (value : string) world = this.Set (nameof this.Closing) value world
        member this.Closing = lens (nameof this.Closing) this this.GetClosing this.SetClosing
        member this.GetRollTime world : single = this.Get (nameof this.RollTime) world
        member this.SetRollTime (value : single) world = this.Set (nameof this.RollTime) value world
        member this.RollTime = lens (nameof this.RollTime) this this.GetRollTime this.SetRollTime
        member this.FinishedEvent = Events.FinishedEvent --> this

/// The credits - a heading, each role with the names credited with it, and a closing line, rolling up the screen over
/// time from below it - finishing once they've rolled off its top, or once they're skipped.
type CreditsDispatcher () =
    inherit GuiDispatcherImSim ()

    /// The credits' lines in order from the top - each its text, its color, its font sizing, and how far it stands below
    /// the heading.
    static let getLines heading closing =
        let roles =
            CreditData.group Data.Credits |>
            List.collect (fun (role, names) ->
                (role, Constants.Gui.DimColor, Constants.Gui.SmallFontSizing) ::
                [for name in names -> (name, Constants.Gui.BrightColor, Some 10.0f)] @
                [("", Constants.Gui.DimColor, None)]) // a gap after each role
        let lines = [(heading, Color (0.55f, 0.75f, 0.7f, 1.0f), Some 28.0f); ("", Constants.Gui.DimColor, None); ("", Constants.Gui.DimColor, None)] @ roles @ [("", Constants.Gui.DimColor, None); (closing, Constants.Gui.DimColor, Some 10.0f)]
        List.mapi (fun i (text, color, sizing) -> (text, color, sizing, single i * Constants.Gui.CreditsSpacing)) lines

    static member Properties =
        [define Entity.Heading ""
         define Entity.Closing ""
         define Entity.RollTime 0.0f]

    override this.Process (entity, world) =

        // roll on, finishing once rolled off the top of the screen, or once skipped
        let lines = getLines (entity.GetHeading world) (entity.GetClosing world)
        let (_, _, _, last) = List.last lines
        let start = -world.Eye2dSize.Y * 0.5f - Constants.Gui.CreditsSpacing // the heading's height to begin, just below the screen
        let rolled = entity.GetRollTime world * Constants.Gui.CreditsSpeed
        if world.TimeAdvancing then
            let delta = if entity.GetTimeScaled world then world.ScaledDelta else world.UnscaledDelta
            entity.RollTime.Map ((+) (single delta.SecondsF)) world
            if  start + rolled - last > world.Eye2dSize.Y * 0.5f + Constants.Gui.CreditsSpacing ||
                World.isKeyboardKeyPressed KeyboardKey.Space world ||
                World.isKeyboardKeyPressed KeyboardKey.Enter world ||
                World.isKeyboardKeyPressed KeyboardKey.Escape world then
                World.publish () entity.FinishedEvent entity world

        // the lines, where they've rolled to, and the hint
        for (i, (text, color, sizing, below)) in List.indexed lines do
            if text <> "" then
                World.doText ("Line+" + string i)
                    [Entity.Position @= v3 0.0f (start + rolled - below) 0.0f
                     Entity.Size .= v3 600.0f 18.0f 0.0f
                     Entity.Text .= text
                     Entity.TextColor .= color
                     Entity.FontSizing .= sizing] world
        World.doText "Hint"
            [Entity.Position .= v3 0.0f -170.0f 0.0f
             Entity.Size .= v3 600.0f 12.0f 0.0f
             Entity.Text .= "Enter - Skip"
             Entity.TextColor .= Color (0.5f, 0.48f, 0.45f, 1.0f)
             Entity.FontSizing .= Constants.Gui.SmallFontSizing] world
