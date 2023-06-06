// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace OmniBlade
open System
open System.Numerics
open Prime
open Nu
open Nu.Declarative
open OmniBlade

[<AutoOpen>]
module ReticlesDispatcher =

    type Reticles =
        Map<CharacterIndex, Vector3>

    type ReticlesCommand =
        | TargetCancel
        | TargetSelect of CharacterIndex
        interface Command

    type Entity with
        member this.GetReticles world = this.GetModelGeneric<Reticles> world
        member this.SetReticles value world = this.SetModelGeneric<Reticles> value world
        member this.Reticles = this.ModelGeneric<Reticles> ()
        member this.TargetSelectEvent = Events.TargetSelect --> this

    type ReticlesDispatcher () =
        inherit GuiDispatcher<Reticles, Message, ReticlesCommand> (Map.empty)

        override this.Command (_, command, entity, world) =
            match command with
            | TargetCancel -> just (World.publishPlus () entity.CancelEvent [] entity true false world)
            | TargetSelect index -> just (World.publishPlus index entity.TargetSelectEvent [] entity true false world)

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
                     Entity.Center := center
                     Entity.UpImage == asset Assets.Battle.PackageName "ReticleUp"
                     Entity.DownImage == asset Assets.Battle.PackageName "ReticleDown"
                     Entity.ClickEvent => TargetSelect index]]