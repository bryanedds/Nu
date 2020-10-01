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
        inherit EntityDispatcher<CharacterModel, unit, unit> (CharacterModel.empty)

        static let getSpriteInset (character : Entity) world =
            let model = character.GetCharacterModel world
            let index = CharacterModel.getAnimationIndex (World.getTickTime world) model
            let offset = v2 (single index.X) (single index.Y) * Constants.Gameplay.CharacterSize
            let inset = v4Bounds offset Constants.Gameplay.CharacterSize
            inset

        static let getSpriteColor (character : Entity) world =
            let model = character.GetCharacterModel world
            let color =
                if model.AnimationCycle = CharacterAnimationCycle.WoundCycle && model.IsEnemy then
                    match CharacterModel.getAnimationProgressOpt (World.getTickTime world) model with
                    | Some progress -> Color (byte 255, byte 128, byte 255, byte 255 - (byte (progress * 255.0f))) // purple
                    | None -> failwithumf ()
                else Color.White
            color

        static let getSpriteGlow (character : Entity) world =
            let pulseTime = World.getTickTime world % Constants.Battle.CharacterPulseLength
            let pulseProgress = single pulseTime / single Constants.Battle.CharacterPulseLength
            let pulseIntensity = byte (sin (pulseProgress * single Math.PI) * 255.0f)
            let model = character.GetCharacterModel world
            let statuses = model.Statuses
            if CharacterModel.isAutoBattling model then Color (byte 255, byte 0, byte 0, pulseIntensity) // red
            elif Set.contains PoisonStatus statuses then Color (byte 0, byte 255, byte 0, pulseIntensity) // green
            elif Set.contains MuteStatus statuses then Color (byte 255,byte 255, byte 0, pulseIntensity) // orange
            elif Set.contains SleepStatus statuses then Color (byte 0, byte 0,byte 255, pulseIntensity) // blue
            else Color.Zero

        static member Properties =
            [define Entity.Omnipresent true]

        override this.Initializers (model, _) =
            [Entity.Bounds <== model --> fun (model : CharacterModel) -> model.Bounds]

        override this.View (model, character, world) =
            if character.GetVisible world && character.GetInView world then
                let transform = character.GetTransform world
                [Render (transform.Depth, transform.Position.Y, AssetTag.generalize model.AnimationSheet,
                     SpriteDescriptor
                       { Transform = transform
                         Offset = Vector2.Zero
                         InsetOpt = Some (getSpriteInset character world)
                         Image = model.AnimationSheet
                         Color = getSpriteColor character world
                         Glow = getSpriteGlow character world
                         Flip = FlipNone })]
            else []