namespace OmniBlade
open Prime
open Nu
open Nu.Declarative
open OmniBlade

[<AutoOpen>]
module OmniReticles =

    type [<ReferenceEquality; NoComparison>] Reticles =
        { Battle : Battle // TODO: let's see if we can make this reference something smaller.
          AimType : AimType }

    type ReticlesCommand =
        | TargetCancel
        | TargetSelect of CharacterIndex

    type Entity with

        member this.GetReticles = this.GetModel<Reticles>
        member this.SetReticles = this.SetModel<Reticles>
        member this.Reticles = this.Model<Reticles> ()
        member this.TargetSelectEvent = Events.TargetSelect --> this

    type ReticlesDispatcher () =
        inherit GuiDispatcher<Reticles, unit, ReticlesCommand> ({ Battle = Battle.empty; AimType = EnemyAim true })

        static member Properties =
            [define Entity.SwallowMouseLeft false
             define Entity.Visible false]

        override this.Command (_, command, rets, world) =
            match command with
            | TargetCancel -> just (World.publish () rets.CancelEvent [] rets true world)
            | TargetSelect index -> just (World.publish index rets.TargetSelectEvent [] rets true world)

        override this.Content (reticles, rets) =
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
             Content.entities reticles
                (fun reticles -> (reticles.AimType, reticles.Battle))
                (fun (aimType, battle) _ -> Battle.getTargets aimType battle) $ fun index character world ->
                let buttonName = rets.Name + "+" + "Reticle" + "+" + scstring index
                let button = rets.Parent / buttonName
                Content.button button.Name
                    [Entity.ParentNodeOpt == None
                     Entity.Size == v2 128.0f 128.0f
                     Entity.Center <== character --> fun (character : Character) -> character.CenterOffset
                     Entity.UpImage == asset Assets.BattlePackageName "ReticleUp"
                     Entity.DownImage == asset Assets.BattlePackageName "ReticleDown"
                     Entity.ClickEvent ==> cmd (TargetSelect (character.Get world).CharacterIndex)]]