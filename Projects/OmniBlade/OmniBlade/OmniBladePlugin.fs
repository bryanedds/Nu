// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace OmniBlade
open System
open Prime
open Nu

type OmniBladePlugin () =
    inherit NuPlugin ()

    override this.EditModes =
        Map.ofSeq
            [("Splash", fun world -> Game.SetOmniBlade Splash world)
             ("Title", fun world -> Game.SetOmniBlade Title world)
             ("Credits", fun world -> Game.SetOmniBlade Credits world)
             ("Pick", fun world -> Game.SetOmniBlade Pick world)
             ("Gameplay", fun world ->
                let field = Field.initial world.UpdateTime (World.getViewBounds2dAbsolute world) Slot1
                let world = Game.SetOmniBlade Field world
                let world = Simulants.Field.SetField field world
                let world = Simulants.Field.Signal (WarpAvatar field.Avatar.Bottom) world
                world)
             ("Slot1", fun world ->
                let field = Field.loadOrInitial world.UpdateTime (World.getViewBounds2dAbsolute world) Slot1
                let world = Game.SetOmniBlade Field world
                let world = Simulants.Field.SetField field world
                let world = Simulants.Field.Signal (WarpAvatar field.Avatar.Bottom) world
                world)
             ("Slot2", fun world ->
                let field = Field.loadOrInitial world.UpdateTime (World.getViewBounds2dAbsolute world) Slot2
                let world = Game.SetOmniBlade Field world
                let world = Simulants.Field.SetField field world
                let world = Simulants.Field.Signal (WarpAvatar field.Avatar.Bottom) world
                world)
             ("Slot3", fun world ->
                let field = Field.loadOrInitial world.UpdateTime (World.getViewBounds2dAbsolute world) Slot3
                let world = Game.SetOmniBlade Field world
                let world = Simulants.Field.SetField field world
                let world = Simulants.Field.Signal (WarpAvatar field.Avatar.Bottom) world
                world)
             ("FieldDebug", fun world ->
                let field = Field.debug world.UpdateTime (World.getViewBounds2dAbsolute world)
                let world = Game.SetOmniBlade Field world
                let world = Simulants.Field.SetField field world
                let world = Simulants.Field.Signal (WarpAvatar field.Avatar.Bottom) world
                world)]