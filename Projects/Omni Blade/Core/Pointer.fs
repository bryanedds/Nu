// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace OmniBlade
open System
open System.Numerics
open Prime
open Nu

/// The pointing hand that shows what a menu has selected for the gamepad or keyboard to choose.
[<RequireQualifiedAccess>]
module Pointer =

    let private Size = v3 60.0f 39.0f 0.0f // three times the image's 20 by 13 pixels, as the game's pixel art is drawn
    let private Gap = 4.0f // how far left of what it points at the fingertip rests
    let private Inset = 16.0f // how far into a gui entity from its left the point the hand points at is
    let private BobDistance = 3.0f
    let private BobPeriod = 40L
    let private Elevation = 100.0f // above all of the field's and battle's gui, including the buttons around it

    /// Draw the hand with its fingertip at a point of a gui entity's, bobbing toward it, unless the mouse is in use.
    let renderAt (point : Vector3) (entity : Entity) (time : int64) world =
        if not (Game.GetIntentState world).PointerHidden && entity.GetExists world && entity.GetVisible world then
            let bob = if time % BobPeriod < BobPeriod / 2L then 0.0f else BobDistance
            let min = v3 (point.X - Size.X - Gap + bob) (point.Y - Size.Y * 0.5f) 0.0f
            World.renderGuiSprite
                (entity.GetAbsolute world) (box3 min Size) Assets.Gui.PointerImage Constants.Render.PipelineNameDefault v3Zero
                (max Elevation (entity.GetElevation world + 1.0f)) false Color.One UserDefinedSettings.defaultSettings world

    /// Draw the hand pointing at a gui entity from its left, a little in from its edge, bobbing toward it, unless
    /// the mouse is in use.
    let render (entity : Entity) (time : int64) world =
        if entity.GetExists world then
            let perimeter = entity.GetPerimeter world
            renderAt (v3 (perimeter.Min.X + Inset) perimeter.Center.Y 0.0f) entity time world

/// The pair of arrows either side of a ring menu's selected item, showing the ring turns left and right.
[<RequireQualifiedAccess>]
module RingArrows =

    let private Size = v3 27.0f 48.0f 0.0f // three times the image's pixels, as the ring's items are drawn
    let private Gap = 3.0f // how far from the item each arrow rests
    let private BobDistance = 3.0f
    let private BobPeriod = 40L
    let private Elevation = 100.0f // above all of the battle's gui, as the pointing hand is

    /// Draw the arrows either side of a ring item, bobbing outward, unless the mouse is in use.
    let render (item : Entity) (time : int64) world =
        if not (Game.GetIntentState world).PointerHidden && item.GetExists world && item.GetVisible world then
            let perimeter = item.GetPerimeter world
            let bob = if time % BobPeriod < BobPeriod / 2L then 0.0f else BobDistance
            let y = perimeter.Center.Y - Size.Y * 0.5f
            let elevation = max Elevation (item.GetElevation world + 1.0f)
            let absolute = item.GetAbsolute world
            World.renderGuiSprite
                absolute (box3 (v3 (perimeter.Min.X - Size.X - Gap - bob) y 0.0f) Size) Assets.Battle.RingLeftImage
                Constants.Render.PipelineNameDefault v3Zero elevation false Color.One UserDefinedSettings.defaultSettings world
            World.renderGuiSprite
                absolute (box3 (v3 (perimeter.Max.X + Gap + bob) y 0.0f) Size) Assets.Battle.RingRightImage
                Constants.Render.PipelineNameDefault v3Zero elevation false Color.One UserDefinedSettings.defaultSettings world

/// Moving a selection through a menu's rows and columns.
[<RequireQualifiedAccess>]
module Selection =

    /// Step an index through a count, wrapping at the ends.
    let wrap delta count index =
        if count <= 0 then 0 else ((index + delta) % count + count) % count

    /// The vertical step a direction makes, down being forward through a list.
    let rowDelta direction =
        match direction with
        | Upward -> -1
        | Downward -> 1
        | Leftward | Rightward -> 0

    /// The horizontal step a direction makes.
    let columnDelta direction =
        match direction with
        | Leftward -> -1
        | Rightward -> 1
        | Upward | Downward -> 0

    /// Play the sound of choosing something.
    let playAffirm world =
        World.playSound 0.0f 0.0f Constants.Audio.SoundVolumeDefault Assets.Gui.AffirmSound world

    /// Play the sound of choosing something that can't be chosen.
    let playMistake world =
        World.playSound 0.0f 0.0f Constants.Audio.SoundVolumeDefault Assets.Gui.MistakeSound world