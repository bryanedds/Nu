namespace OmniBlade
open System
open Prime
open Nu
open Nu.Declarative
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
            [define Entity.SwallowMouseLeft false
             define Entity.Visible false]

        override this.Command (_, command, rets, world) =
            match command with
            | TargetCancel -> just (World.publish () rets.CancelEvent [] rets world)
            | TargetSelect index -> just (World.publish index rets.TargetSelectEvent [] rets world)

        override this.Content (model, rets, _) =
            let buttonName = rets.Name + "+" + "Cancel"
            let button = rets.Parent / buttonName
            [Content.button buttonName
                [Entity.Size == v2 64.0f 64.0f
                 Entity.ViewType == Relative
                 Entity.Persistent == false
                 Entity.UpImage == asset Assets.BattlePackage "CancelUp"
                 Entity.DownImage == asset Assets.BattlePackage "CancelDown"
                 Entity.ParentNodeOpt == Some (relate button rets)
                 Entity.ClickEvent ==> cmd TargetSelect]
             Content.entities (model --> fun model -> CharacterModels.getTargets model.AimType model.Characters) $ fun index character _ world ->
                Content.button (rets.Name + "+" + "Reticle" + "+" + scstring index)
                    [Entity.PositionLocal <== character --> fun character -> character.Position
                     Entity.Size == v2 128.0f 128.0f
                     Entity.ViewType == Relative
                     Entity.Persistent == false
                     Entity.UpImage == asset Assets.BattlePackage "ReticleUp"
                     Entity.DownImage == asset Assets.BattlePackage "ReticleDown"
                     Entity.ParentNodeOpt == Some (relate button rets)
                     Entity.ClickEvent ==> cmd (TargetSelect (character.Get world).CharacterState.CharacterIndex)]]