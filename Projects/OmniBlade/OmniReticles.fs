namespace OmniBlade
open Prime
open Nu
open Nu.Declarative
open OmniBlade

[<AutoOpen>]
module OmniReticles =

    type [<StructuralEquality; NoComparison>] ReticlesModel =
        { BattleModel : BattleModel
          AimType : AimType }

    type ReticlesCommand =
        | TargetCancel
        | TargetSelect of CharacterIndex

    type Entity with

        member this.GetReticlesModel = this.GetModel<ReticlesModel>
        member this.SetReticlesModel = this.SetModel<ReticlesModel>
        member this.ReticlesModel = this.Model<ReticlesModel> ()
        member this.TargetSelectEvent = Events.TargetSelect --> this

    type ReticlesDispatcher () =
        inherit GuiDispatcher<ReticlesModel, unit, ReticlesCommand> ({ BattleModel = BattleModel.empty; AimType = EnemyAim true })

        static member Properties =
            [define Entity.SwallowMouseLeft false
             define Entity.Visible false]

        override this.Command (_, command, rets, world) =
            match command with
            | TargetCancel -> just (World.publish () rets.CancelEvent [] rets world)
            | TargetSelect index -> just (World.publish index rets.TargetSelectEvent [] rets world)

        override this.Content (model, rets) =
            let buttonName = rets.Name + "+" + "Cancel"
            let button = rets.Parent / buttonName
            [Content.button button.Name
                [Entity.ParentNodeOpt == None
                 Entity.Visible <== rets.Visible
                 Entity.Size == v2 64.0f 64.0f
                 Entity.Position == Constants.Battle.CancelPosition
                 Entity.UpImage == asset Assets.BattlePackageName "CancelUp"
                 Entity.DownImage == asset Assets.BattlePackageName "CancelDown"
                 Entity.ClickEvent ==> cmd TargetCancel]
             Content.entities model
                (fun model -> (model.AimType, model.BattleModel))
                (fun (aimType, battleModel) _ -> BattleModel.getTargets aimType battleModel) $ fun index character world ->
                let buttonName = rets.Name + "+" + "Reticle" + "+" + scstringm index
                let button = rets.Parent / buttonName
                Content.button button.Name
                    [Entity.ParentNodeOpt == None
                     Entity.Size == v2 128.0f 128.0f
                     Entity.Center <== character --> fun (character : CharacterModel) -> character.CenterOffset
                     Entity.UpImage == asset Assets.BattlePackageName "ReticleUp"
                     Entity.DownImage == asset Assets.BattlePackageName "ReticleDown"
                     Entity.ClickEvent ==> cmd (TargetSelect (character.Get world).CharacterIndex)]]