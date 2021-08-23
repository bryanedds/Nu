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
            let index = Character.getAnimationIndex (World.getUpdateTime world) character
            let offset = v2 (single index.X) (single index.Y) * character.CelSize
            let inset = v4Bounds offset character.CelSize
            inset

        static let getSpriteColor (entity : Entity) world =
            let character = entity.GetCharacter world
            let color =
                if character.CharacterAnimationType = WoundAnimation && character.IsEnemy then
                    match Character.getAnimationProgressOpt (World.getUpdateTime world) character with
                    | Some progress -> Color (byte 255, byte 128, byte 255, byte 255 - (byte (progress * 255.0f))) // purple
                    | None -> failwithumf ()
                else Color.White
            color

        static let getSpriteGlow (entity : Entity) world =
            let pulseTime = World.getUpdateTime world % Constants.Battle.CharacterPulseLength
            let pulseProgress = single pulseTime / single Constants.Battle.CharacterPulseLength
            let pulseIntensity = byte (sin (pulseProgress * single Math.PI) * 255.0f)
            let character = entity.GetCharacter world
            let statuses = character.Statuses
            if character.IsWounded then Color.Zero
            elif Character.isAutoTeching character then Color (byte 255, byte 64, byte 64, pulseIntensity) // bright red
            elif Map.containsKey Poison statuses then Color (byte 0, byte 255, byte 0, pulseIntensity) // green
            elif Map.containsKey Silence statuses then Color (byte 255,byte 255, byte 0, pulseIntensity) // orange
            elif Map.containsKey Sleep statuses then Color (byte 0, byte 0, byte 255, pulseIntensity) // blue
            elif Map.exists (fun key _ -> match key with Time true -> true | _ -> false) statuses then Color (byte 127, byte 255, byte 127, pulseIntensity) // bright yellow
            elif Map.exists (fun key _ -> match key with Power (true, _) -> true | _ -> false) statuses then Color (byte 255, byte 255, byte 127, pulseIntensity) // bright orange
            elif Map.exists (fun key _ -> match key with Magic (true, _) -> true | _ -> false) statuses then Color (byte 255, byte 127, byte 255, pulseIntensity) // bright purple
            elif Map.exists (fun key _ -> match key with Shield (true, _) -> true | _ -> false) statuses then Color (byte 255, byte 255, byte 255, pulseIntensity) // bright white
            elif Map.exists (fun key _ -> match key with Time false -> true | _ -> false) statuses then Color (byte 0, byte 127, byte 0, pulseIntensity) // dark red
            elif Map.exists (fun key _ -> match key with Power (false, _) -> true | _ -> false) statuses then Color (byte 127, byte 127, byte 0, pulseIntensity) // dark orange
            elif Map.exists (fun key _ -> match key with Magic (false, _) -> true | _ -> false) statuses then Color (byte 127, byte 0, byte 127, pulseIntensity) // dark purple
            elif Map.exists (fun key _ -> match key with Shield (false, _) -> true | _ -> false) statuses then Color (byte 127, byte 127, byte 127, pulseIntensity) // dark white
            else Color.Zero

        static member Properties =
            [define Entity.Omnipresent true]

        override this.Initializers (character, _) =
            [Entity.Bounds <== character --> fun character -> character.Bounds]

        override this.View (character, entity, world) =
            if entity.GetVisible world && entity.GetInView world then
                let transform = entity.GetTransform world
                Render (transform.Elevation, transform.Position.Y, AssetTag.generalize character.AnimationSheet,
                    SpriteDescriptor
                        { Transform = transform
                          Absolute = entity.GetAbsolute world
                          Offset = Vector2.Zero
                          InsetOpt = Some (getSpriteInset entity world)
                          Image = character.AnimationSheet
                          Color = getSpriteColor entity world
                          Blend = Transparent
                          Glow = getSpriteGlow entity world
                          Flip = FlipNone })
            else View.empty