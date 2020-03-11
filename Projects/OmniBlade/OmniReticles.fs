namespace OmniBlade
open System
open Prime
open Nu
open OmniBlade

[<AutoOpen>]
module OmniReticles =

    type [<NoComparison>] ReticlesCommand =
        | TargetCancel
        | TargetSelect of CharacterIndex

    type Entity with

        member this.GetReticlesModel = this.GetModel<ReticlesModel>
        member this.SetReticlesModel = this.SetModel<ReticlesModel>
        member this.ReticlesModel = this.Model<ReticlesModel> ()
        member this.TargetSelectEvent = Events.TargetSelect --> this

    type ReticlesDispatcher () =
        inherit GuiDispatcher<ReticlesModel, unit, ReticlesCommand> ({ Characters = Map.empty; AimType = NoAim })

        static member Properties =
            [define Entity.Lens.SwallowMouseLeft false
             define Entity.Lens.Visible false]

        override this.Command (_, command, rets, world) =
            match command with
            | TargetCancel -> just (World.publish () rets.CancelEvent [] rets world)
            | TargetSelect index -> just (World.publish index rets.TargetSelectEvent [] rets world)

        override this.Content (model, rets, _) =
            let buttonName = rets.Name + "+" + "Cancel"
            let button = rets.Parent / buttonName
            [Content.button buttonName
                [button.PositionLocal == v2 -32.0f -96.0f
                 button.Size == v2 64.0f 64.0f
                 button.ViewType == Relative
                 button.Persistent == false
                 button.UpImage == asset Assets.BattlePackage "CancelUp"
                 button.DownImage == asset Assets.BattlePackage "CancelDown"
                 button.ParentNodeOpt == Some (relate button rets)
                 button.ClickEvent ==> cmd TargetCancel]
             Content.entities (model --> fun model -> CharacterModels.getTargets model.AimType model.Characters) $ fun index character _ world ->
                let buttonName = rets.Name + "+" + "Reticle" + "+" + scstring index
                let button = rets.Parent / buttonName
                Content.button buttonName
                    [button.Center <== character --> fun character -> character.Center
                     button.Size == v2 128.0f 128.0f
                     button.ViewType == Relative
                     button.Persistent == false
                     button.UpImage == asset Assets.BattlePackage "ReticleUp"
                     button.DownImage == asset Assets.BattlePackage "ReticleDown"
                     button.ParentNodeOpt == Some (relate button rets)
                     button.ClickEvent ==> cmd (TargetSelect (character.Get world).CharacterState.CharacterIndex)]]