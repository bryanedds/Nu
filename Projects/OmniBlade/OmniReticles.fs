namespace OmniBlade
open System
open FSharpx.Collections
open Prime
open Nu
open Nu.Declarative
open OmniBlade

[<AutoOpen>]
module OmniReticles =

    type Entity with
        
        member this.GetAimType world : AimType = this.Get Property? AimType world
        member this.SetAimType (value : AimType) world = this.Set Property? AimType value world
        member this.AimType = Lens.make Property? AimType this.GetAimType this.SetAimType this
        member this.GetReticleButtonsNp world : Entity list = this.Get Property? ReticleButtonsNp world
        member this.SetReticleButtonsNp (value : Entity list) world = this.Set Property? ReticleButtonsNp value world
        member this.ReticleButtonsNp = Lens.make Property? ReticleButtonsNp this.GetReticleButtonsNp this.SetReticleButtonsNp this
        member this.GetReticleCancelButtonOptNp world : Entity option = this.Get Property? ReticleCancelButtonOptNp world
        member this.SetReticleCancelButtonOptNp (value : Entity option) world = this.Set Property? ReticleCancelButtonOptNp value world
        member this.ReticleCancelButtonOptNp = Lens.make Property? ReticleCancelButtonOptNp this.GetReticleCancelButtonOptNp this.SetReticleCancelButtonOptNp this
        member this.TargetSelectEvent = Events.TargetSelect --> this

    type ReticlesDispatcher () =
        inherit GuiDispatcher ()

        static let CancelButtonOffset = Vector2 (0.0f, -80.0f)

        static let getTargets (rets : Entity) world =
            match rets.GetAimType world with
            | EnemyAim -> World.getEnemies Simulants.Scene world
            | AllyAim healthy -> (if healthy then World.getHealthyAllies else World.getWoundedAllies) Simulants.Scene world
            | AnyAim -> World.getCharacters Simulants.Scene world
            | NoAim -> []

        static let destroyButton button world =
            World.destroyEntityImmediate button world

        static let createButton name (rets : Entity) world =
            let (button, world) = World.createEntity5 typeof<ButtonDispatcher>.Name (Some (rets.Name + "+" + name)) DefaultOverlay (etol rets) world
            let world = button.SetViewType Relative world
            let world = button.SetPersistent false world
            let world = button.SetDepth (rets.GetDepth world + 1.0f) world
            let world = button.SetParentNodeOptWithAdjustment (Some (Relation.unresolve button.EntityAddress rets.EntityAddress)) world
            (button, world)

        static let createReticleButton index (target : Entity) (rets : Entity) world =
            let (button, world) = createButton ("Reticle" + "+" + scstring index) rets world
            let world = button.SetUpImage (asset Assets.BattlePackage "ReticleUp") world
            let world = button.SetDownImage (asset Assets.BattlePackage "ReticleDown") world
            let world = button.QuickSize world
            let world = button.SetCenter (target.GetCenter world) world
            let world = button.AttachProperty Property? Target false true { PropertyType = typeof<Entity>; PropertyValue = target } world
            let world = World.monitor (fun _ world -> World.publish target rets.TargetSelectEvent [] rets world) button.ClickEvent button world
            (button, world)

        static let createCancelButton cancelStr  (menu : Entity) world =
            let (button, world) = createButton cancelStr menu world
            let world = button.SetUpImage (asset Assets.BattlePackage (cancelStr + "Up")) world
            let world = button.SetDownImage (asset Assets.BattlePackage (cancelStr + "Down")) world
            let world = button.SetPositionLocal CancelButtonOffset world
            let world = World.monitor (fun _ world -> World.publish () menu.CancelEvent [] menu world) button.ClickEvent button world
            (button, world)

        static let destroyButtons (rets : Entity) world =

            // destroy reticle buttons
            let world = List.fold (flip destroyButton) world (rets.GetReticleButtonsNp world)
            let world = rets.SetReticleButtonsNp [] world

            // destroy cancel button
            match rets.GetItemCancelOpt world with
            | Some cancelStr ->
                let (cancelButton, world) = createCancelButton cancelStr rets world
                rets.SetReticleCancelButtonOptNp (Some cancelButton) world
            | None -> world

        static let createButtons (rets : Entity) world =

            // create reticle buttons
            let (buttons, world) =
                Seq.foldi (fun index (buttons, world) target ->
                    let (button, world) = createReticleButton index target rets world
                    (button :: buttons, world))
                    ([], world) (getTargets rets world)
            let world = rets.SetReticleButtonsNp buttons world

            // create cancel button
            let (cancelButton, world) = createCancelButton "Cancel" rets world
            rets.SetReticleCancelButtonOptNp (Some cancelButton) world

        static let updateButtons (rets : Entity) world =
            Seq.fold (fun world (button : Entity) ->
                let target = button.Get<Entity> Property? Target world
                if target.GetExists world && (target.GetCharacterState world).IsHealthy then
                    let world = button.SetDepth (rets.GetDepth world + 1.0f) world
                    button.SetCenter (target.GetCenter world) world
                else
                    let world = rets.ReticleButtonsNp.Update (List.remove ((=) button)) world
                    World.destroyEntity button world)
                world (rets.GetReticleButtonsNp world)
        
        static member Properties =
            [define Entity.AimType EnemyAim
             define Entity.ReticleButtonsNp []
             define Entity.ReticleCancelButtonOptNp None
             define Entity.SwallowMouseLeft false]
        
        override dispatcher.Register (rets, world) =
            createButtons rets world

        override dispatcher.Unregister (rets, world) =
            destroyButtons rets world

        override dispatcher.Update (rets, world) =
            updateButtons rets world