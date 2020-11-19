// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace OmniBlade
open System
open System.Numerics
open FSharpx.Collections
open Prime
open Nu
open Nu.Declarative
open OmniBlade

[<AutoOpen>]
module CharacterDispatcher =

    type Entity with
        member this.GetCharacter = this.GetModel<Character>
        member this.SetCharacter = this.SetModel<Character>
        member this.Character = this.Model<Character> ()

    type CharacterDispatcher () =
        inherit EntityDispatcher<Character, unit, unit> (Character.empty)

        static let getSpriteInset (entity : Entity) world =
            let character = entity.GetCharacter world
            let index = Character.getAnimationIndex (World.getTickTime world) character
            let offset = v2 (single index.X) (single index.Y) * Constants.Gameplay.CharacterSize
            let inset = v4Bounds offset Constants.Gameplay.CharacterSize
            inset

        static let getSpriteColor (entity : Entity) world =
            let character = entity.GetCharacter world
            let color =
                if character.AnimationCycle = CharacterAnimationCycle.WoundCycle && character.IsEnemy then
                    match Character.getAnimationProgressOpt (World.getTickTime world) character with
                    | Some progress -> Color (byte 255, byte 128, byte 255, byte 255 - (byte (progress * 255.0f))) // purple
                    | None -> failwithumf ()
                else Color.White
            color

        static let getSpriteGlow (entity : Entity) world =
            let pulseTime = World.getTickTime world % Constants.Battle.CharacterPulseLength
            let pulseProgress = single pulseTime / single Constants.Battle.CharacterPulseLength
            let pulseIntensity = byte (sin (pulseProgress * single Math.PI) * 255.0f)
            let character = entity.GetCharacter world
            let statuses = character.StatusesActive
            if character.IsWounded then Color.Zero
            elif Character.isAutoBattling character then Color (byte 255, byte 64, byte 64, pulseIntensity) // bright red
            elif Set.contains PoisonStatus statuses then Color (byte 0, byte 255, byte 0, pulseIntensity) // green
            elif Set.contains MuteStatus statuses then Color (byte 255,byte 255, byte 0, pulseIntensity) // orange
            elif Set.contains SleepStatus statuses then Color (byte 0, byte 0, byte 255, pulseIntensity) // blue
            else Color.Zero

        static member Properties =
            [define Entity.Omnipresent true]

        override this.Initializers (character, _) =
            [Entity.Bounds <== character --> fun character -> character.Bounds]

        override this.View (character, entity, world) =
            if entity.GetVisible world && entity.GetInView world then
                let transform = entity.GetTransform world
                [Render (transform.Depth, transform.Position.Y, AssetTag.generalize character.AnimationSheet,
                     SpriteDescriptor
                       { Transform = transform
                         Offset = Vector2.Zero
                         InsetOpt = Some (getSpriteInset entity world)
                         Image = character.AnimationSheet
                         Color = getSpriteColor entity world
                         Glow = getSpriteGlow entity world
                         Flip = FlipNone })]
            else []