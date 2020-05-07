namespace OmniBlade
open System
open FSharpx.Collections
open Prime
open Nu
open Nu.Declarative
open OmniBlade

[<AutoOpen>]
module OmniCharacter =

    type Entity with
    
        member this.GetCharacterModel = this.GetModel<CharacterModel>
        member this.SetCharacterModel = this.SetModel<CharacterModel>
        member this.CharacterModel = this.Model<CharacterModel> ()

    type CharacterDispatcher () =
        inherit EntityDispatcher<CharacterModel, unit, unit>
            (CharacterModel.make
                { PartyIndex = 0; CharacterType = Ally Jinn; ActionTime = 0; ExpPoints = 0; HitPoints = 20; SpecialPoints = 1; Defending = false; Charging = false; PowerBuff = 1.0f; ShieldBuff = 1.0f; MagicBuff = 1.0f; CounterBuff = 1.0f; Specials = Set.empty; Statuses = Set.empty; WeaponOpt = Some "WoodenSword"; ArmorOpt = None; Accessories = []; AutoBattleOpt = None }
                { TimeStart = 0L; AnimationSheet = Assets.JinnAnimationSheet; AnimationCycle = ReadyCycle; Direction = Rightward }
                NoInput
                (Math.makeBounds v2Zero v2One))

        static let [<Literal>] CelSize =
            160.0f

        static let getSpriteInset (character : Entity) world =
            let rom = Simulants.Game.GetModel<Rom> world
            let model = character.GetCharacterModel world
            let index = CharacterModel.getAnimationIndex rom (World.getTickTime world) model
            let offset = v2 (single index.X * CelSize) (single index.Y * CelSize)
            let inset = Vector4 (offset.X, offset.Y, offset.X + CelSize, offset.Y + CelSize)
            inset

        static let getSpriteColor (character : Entity) world =
            let rom = Simulants.Game.GetModel<Rom> world
            let model = character.GetCharacterModel world
            let color =
                if model.AnimationCycle = CharacterAnimationCycle.WoundCycle && model.IsEnemy then
                    match CharacterModel.getAnimationProgressOpt rom (World.getTickTime world) model with
                    | Some progress -> Vector4 (1.0f,0.5f,1.0f,1.0f-progress) // purple
                    | None -> failwithumf ()
                else Vector4.One
            color

        static let getSpriteGlow (character : Entity) world =
            let pulseTime = World.getTickTime world % Constants.Battle.CharacterPulseLength
            let pulseProgress = single pulseTime / single Constants.Battle.CharacterPulseLength
            let pulseIntensity = sin (pulseProgress * single Math.PI)
            let model = character.GetCharacterModel world
            let statuses = model.Statuses
            if CharacterModel.runningSpecialAutoBattle model then Vector4 (1.0f,0.0f,0.0f,pulseIntensity) // red
            elif Set.contains PoisonStatus statuses then Vector4 (0.0f,1.0f,0.0f,pulseIntensity) // green
            elif Set.contains MuteStatus statuses then Vector4 (0.1f,1.0f,0.0f,pulseIntensity) // orange
            elif Set.contains SleepStatus statuses then Vector4 (0.0f,0.0f,1.0f,pulseIntensity) // blue
            else Vector4.Zero

        static member Properties =
            [define Entity.Omnipresent true]

        override this.Initializers (model, _, _) =
            [Entity.Bounds <== model --> fun (model : CharacterModel) -> model.Bounds]

        override this.Actualize (character, world) =
            if character.GetInView world then
                let model = character.GetCharacterModel world
                World.enqueueRenderMessage
                    (RenderDescriptorMessage
                        (LayerableDescriptor
                            { Depth = character.GetDepth world
                              PositionY = (character.GetPosition world).Y
                              AssetTag = model.AnimationSheet
                              LayeredDescriptor =
                              SpriteDescriptor
                                { Position = character.GetPosition world
                                  Size = character.GetSize world
                                  Rotation = character.GetRotation world
                                  Offset = Vector2.Zero
                                  ViewType = character.GetViewType world
                                  InsetOpt = Some (getSpriteInset character world)
                                  Image = model.AnimationSheet
                                  Color = getSpriteColor character world
                                  Glow = getSpriteGlow character world
                                  Flip = FlipNone }}))
                    world
            else world