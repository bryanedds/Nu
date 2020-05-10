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
            (CharacterModel.make (AllyIndex 0) (Ally Finn) 0 None None [] Assets.FinnAnimationSheet Rightward (Math.makeBounds  v2Zero v2One))

        static let getSpriteInset (character : Entity) world =
            let model = character.GetCharacterModel world
            let index = CharacterModel.getAnimationIndex (World.getTickTime world) model
            let offset = v2 (single index.X) (single index.Y) * Constants.Gameplay.CharacterSize
            let inset = Math.makeBounds offset Constants.Gameplay.CharacterSize
            inset

        static let getSpriteColor (character : Entity) world =
            let model = character.GetCharacterModel world
            let color =
                if model.AnimationCycle = CharacterAnimationCycle.WoundCycle && model.IsEnemy then
                    match CharacterModel.getAnimationProgressOpt (World.getTickTime world) model with
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
            if CharacterModel.runningTechAutoBattle model then Vector4 (1.0f,0.0f,0.0f,pulseIntensity) // red
            elif Set.contains PoisonStatus statuses then Vector4 (0.0f,1.0f,0.0f,pulseIntensity) // green
            elif Set.contains MuteStatus statuses then Vector4 (0.1f,1.0f,0.0f,pulseIntensity) // orange
            elif Set.contains SleepStatus statuses then Vector4 (0.0f,0.0f,1.0f,pulseIntensity) // blue
            else Vector4.Zero

        static member Properties =
            [define Entity.Omnipresent true]

        override this.Initializers (model, _, _) =
            [Entity.Bounds <== model --> fun (model : CharacterModel) -> model.Bounds]

        override this.Actualize (character, world) =
            if character.GetVisibleLayered world && character.GetInView world then
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