// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace OmniBlade
open System
open System.Collections.Generic
open System.Numerics
open Prime
open Nu

type [<Sealed>] OmniBladePlugin () =
    inherit NuPlugin ()

    override this.EditModes =
        Map.ofList
            [("Splash", fun world -> Game.SetOmniBlade Splash world)
             ("Title", fun world -> Game.SetOmniBlade Title world)
             ("Credits", fun world -> Game.SetOmniBlade Credits world)
             ("Pick", fun world -> Game.SetOmniBlade Pick world)
             ("Intro", fun world -> Game.SetOmniBlade (Intro Slot1) world)
             ("Gameplay", fun world ->
                let field = Field.initial world.UpdateTime Slot1
                Simulants.Field.SetField field world
                Simulants.Field.Signal (WarpAvatar field.Avatar.Perimeter.Bottom) world
                Game.SetOmniBlade Field world)
             ("Slot1", fun world ->
                let field = Field.loadOrInitial world.UpdateTime Slot1
                Simulants.Field.SetField field world
                Simulants.Field.Signal (WarpAvatar field.Avatar.Perimeter.Bottom) world
                Game.SetOmniBlade Field world)
             ("Slot2", fun world ->
                let field = Field.loadOrInitial world.UpdateTime Slot2
                Simulants.Field.SetField field world
                Simulants.Field.Signal (WarpAvatar field.Avatar.Perimeter.Bottom) world
                Game.SetOmniBlade Field world)
             ("Slot3", fun world ->
                let field = Field.loadOrInitial world.UpdateTime Slot3
                Simulants.Field.SetField field world
                Simulants.Field.Signal (WarpAvatar field.Avatar.Perimeter.Bottom) world
                Game.SetOmniBlade Field world)
             ("FieldDebug", fun world ->
                let field = Field.debug world.UpdateTime
                Simulants.Field.SetField field world
                Simulants.Field.Signal (WarpAvatar field.Avatar.Perimeter.Bottom) world
                Game.SetOmniBlade Field world)]

    override this.InitialPackages =
        [Assets.Gui.PackageName]

    override this.MakePhysicsEngine2d () =
        Box2dNetPhysicsEngine.make (Constants.Physics.GravityDefault * Constants.Engine.Meter2d)

    override this.MakePhysicsEngine2dRenderContext segments circles eyeBounds =
        { new Box2dNetPhysicsEngineRenderContext with
            override this.DrawLine (start : Vector2, stop : Vector2, color) =
                match segments.TryGetValue color with
                | (true, segmentList) -> segmentList.Add (start, stop)
                | (false, _) -> segments.Add (color, List [struct (start, stop)])
            override this.DrawCircle (center : Vector2, radius, color) =
                match circles.TryGetValue struct (color, radius) with
                | (true, circleList) -> circleList.Add center
                | (false, _) -> circles.Add (struct (color, radius), List [center])
            override _.EyeBounds = eyeBounds }