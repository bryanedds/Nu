namespace OmniBlade
open System
open Prime
open Nu
open Nu.Declarative
open OmniBlade

[<AutoOpen>]
module OmniReticles =

    type [<NoComparison>] ReticleCommand =
        | Cancel
        | TargetSelect of CharacterIndex

    type Entity with

        member this.GetReticleModel = this.GetModel<ReticleModel>
        member this.SetReticleModel = this.SetModel<ReticleModel>
        member this.ReticleModel = this.Model<ReticleModel> ()
        member this.TargetSelectEvent = Events.TargetSelect --> this

    type ReticlesDispatcher () =
        inherit GuiDispatcher<ReticleModel, unit, ReticleCommand> ({ Characters = Map.empty; AimType = NoAim })

        static member Properties =
            [define Entity.SwallowMouseLeft false
             define Entity.Visible false]

        override this.Command (_, command, rets, world) =
            match command with
            | Cancel -> just (World.publish () rets.CancelEvent [] rets world)
            | TargetSelect index -> just (World.publish index rets.TargetSelectEvent [] rets world)

        override this.Content (model, rets, _) =
            [Content.button (rets.Name + "+" + "Cancel")
                [Entity.PositionLocal == v2 0.0f -80.0f
                 Entity.Size == v2 64.0f 64.0f
                 Entity.Depth <== rets.Depth + 1.0f
                 Entity.ViewType == Relative
                 Entity.Persistent == false
                 Entity.UpImage == asset Assets.BattlePackage "CancelUp"
                 Entity.DownImage == asset Assets.BattlePackage "CancelDown"
                 Entity.ClickEvent ==> cmd TargetSelect]
             Content.entities (model --> fun model -> CharacterModels.getTargets model.AimType model.Characters) $ fun index character _ world ->
                Content.button (rets.Name + "+" + "Reticle" + "+" + scstring index)
                    [Entity.Center <== character --> fun character -> character.Center
                     Entity.Size == v2 64.0f 64.0f
                     Entity.Depth <== rets.Depth + 1.0f
                     Entity.ViewType == Relative
                     Entity.Persistent == false
                     Entity.UpImage == asset Assets.BattlePackage "ReticleUp"
                     Entity.DownImage == asset Assets.BattlePackage "ReticleDown"
                     Entity.ClickEvent ==> cmd (TargetSelect (character.Get world).CharacterState.CharacterIndex)]]