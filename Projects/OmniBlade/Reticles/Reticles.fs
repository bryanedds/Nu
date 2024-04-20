// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace OmniBlade
open System
open System.Numerics
open Prime
open Nu

type Reticles =
    Map<CharacterIndex, Vector3>

type ReticlesCommand =
    | TargetCancel
    | TargetSelect of CharacterIndex
    interface Command

[<AutoOpen>]
module ReticlesDispatcher =
    type Entity with
        member this.GetReticles world = this.GetModelGeneric<Reticles> world
        member this.SetReticles value world = this.SetModelGeneric<Reticles> value world
        member this.Reticles = this.ModelGeneric<Reticles> ()
        member this.TargetSelectEvent = Events.TargetSelectEvent --> this

type ReticlesDispatcher () =
    inherit GuiDispatcher<Reticles, Message, ReticlesCommand> (Map.empty)

    override this.Command (_, command, entity, world) =
        match command with
        | TargetCancel -> just (World.publish () entity.CancelEvent entity world)
        | TargetSelect index -> just (World.publish index entity.TargetSelectEvent entity world)

    override this.Content (reticles, _) =
        [Content.button "Cancel"
            [Entity.PositionLocal == Constants.Battle.CancelPosition
             Entity.Size == v3 48.0f 48.0f 0.0f
             Entity.UpImage == asset Assets.Battle.PackageName "CancelUp"
             Entity.DownImage == asset Assets.Battle.PackageName "CancelDown"
             Entity.ClickEvent => TargetCancel]
         for (index, center) in reticles.Pairs do
            Content.button (CharacterIndex.toEntityName index)
                [Entity.MountOpt == None
                 Entity.Size == v3 96.0f 96.0f 0.0f
                 Entity.PerimeterCenter := center
                 Entity.UpImage == asset Assets.Battle.PackageName "ReticleUp"
                 Entity.DownImage == asset Assets.Battle.PackageName "ReticleDown"
                 Entity.ClickEvent => TargetSelect index]]