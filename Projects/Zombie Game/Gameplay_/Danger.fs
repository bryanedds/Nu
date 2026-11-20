// Zombie Game.
// Copyright (C) Bryan Edds.

namespace ZombieGame
open System
open System.Numerics
open Prime
open Nu

/// The dark pulsing over play while the player's in danger.
type DangerDispatcher () =
    inherit GuiDispatcherImSim ()

    override this.Process (entity, world) =
        let gameTime = if entity.GetTimeScaled world then world.ScaledTime else world.UnscaledTime
        World.doText "Danger"
            [Entity.Position .= v3 0.0f 0.0f 0.0f
             Entity.Size .= v3 640.0f 360.0f 0.0f
             Entity.Text .= ""
             Entity.TextColor .= Color.White
             Entity.FontSizing .= None
             Entity.BackdropImageOpt .= Some Assets.Default.Black
             Entity.Color @= Color.White.WithA (0.12f + 0.08f * abs (sin (gameTime.SecondsF * 3.0f)))] world
