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

    /// Draw the hand with its fingertip at a point of a gui entity's, bobbing toward it, if shown, as it's not while
    /// the mouse is in use.
    let renderAt shown (point : Vector3) (entity : Entity) (time : int64) world =
        if shown && entity.GetExists world && entity.GetVisible world then
            let period = Constants.Gui.PointerBobPeriod
            let bob = if time % period < period / 2L then 0.0f else Constants.Gui.PointerBobDistance
            let size = Constants.Gui.PointerSize
            let min = v3 (point.X - size.X - Constants.Gui.PointerGap + bob) (point.Y - size.Y * 0.5f) 0.0f
            let elevation = max Constants.Gui.PointerElevation (entity.GetElevation world + 1.0f)
            World.renderGuiSprite
                (entity.GetAbsolute world) (box3 min size) Assets.Gui.PointerImage Constants.Render.PipelineNameDefault v3Zero
                elevation false Color.One UserDefinedSettings.defaultSettings world

    /// Draw the hand pointing at a gui entity from its left, a little in from its edge, bobbing toward it, if shown,
    /// as it's not while the mouse is in use.
    let render shown (entity : Entity) (time : int64) world =
        if entity.GetExists world then
            let perimeter = entity.GetPerimeter world
            renderAt shown (v3 (perimeter.Min.X + Constants.Gui.PointerInset) perimeter.Center.Y 0.0f) entity time world

/// The pair of arrows either side of a ring menu's selected item, showing the ring turns left and right.
[<RequireQualifiedAccess>]
module RingArrows =

    /// Draw the arrows either side of a ring item, bobbing outward, if shown, as they're not while the mouse is in use.
    let render shown (item : Entity) (time : int64) world =
        if shown && item.GetExists world && item.GetVisible world then
            let perimeter = item.GetPerimeter world
            let period = Constants.Battle.RingArrowsBobPeriod
            let bob = if time % period < period / 2L then 0.0f else Constants.Battle.RingArrowsBobDistance
            let size = Constants.Battle.RingArrowsSize
            let gap = Constants.Battle.RingArrowsGap
            let y = perimeter.Center.Y - size.Y * 0.5f
            let elevation = max Constants.Battle.RingArrowsElevation (item.GetElevation world + 1.0f)
            let absolute = item.GetAbsolute world
            World.renderGuiSprite
                absolute (box3 (v3 (perimeter.Min.X - size.X - gap - bob) y 0.0f) size) Assets.Battle.RingLeftImage
                Constants.Render.PipelineNameDefault v3Zero elevation false Color.One UserDefinedSettings.defaultSettings world
            World.renderGuiSprite
                absolute (box3 (v3 (perimeter.Max.X + gap + bob) y 0.0f) size) Assets.Battle.RingRightImage
                Constants.Render.PipelineNameDefault v3Zero elevation false Color.One UserDefinedSettings.defaultSettings world