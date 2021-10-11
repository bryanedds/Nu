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
        member this.GetCharacter world = this.GetModelGeneric<Character> world
        member this.SetCharacter value world = this.SetModelGeneric<Character> value world
        member this.Character = this.ModelGeneric<Character> ()

    type CharacterDispatcher () =
        inherit EntityDispatcher<Character, unit, unit> (Character.empty)

        static let getSpriteInset (character : Character) world =
            Character.getAnimationInset (World.getUpdateTime world) character

        static let getSpriteColor (character : Character) world =
            let color =
                if character.CharacterAnimationType = WoundAnimation && character.IsEnemy then
                    match Character.getAnimationProgressOpt (World.getUpdateTime world) character with
                    | Some progress -> Color (byte 255, byte 128, byte 255, byte 255 - (byte (progress * 255.0f))) // purple
                    | None -> failwithumf ()
                else Color.White
            color

        static let getSpriteGlow (character : Character) world =
            let pulseTime = World.getUpdateTime world % Constants.Battle.CharacterPulseLength
            let pulseProgress = single pulseTime / single Constants.Battle.CharacterPulseLength
            let pulseIntensity = byte (sin (pulseProgress * single Math.PI) * 255.0f)
            let statuses = character.Statuses
            if character.IsWounded then Color.Zero
            elif Character.isAutoTeching character then Color (byte 255, byte 64, byte 64, pulseIntensity) // bright red
            elif Map.exists (fun key _ -> match key with Time true -> true | _ -> false) statuses then Color (byte 255, byte 255, byte 255, pulseIntensity) // bright white
            elif Map.exists (fun key _ -> match key with Power (true, _) -> true | _ -> false) statuses then Color (byte 255, byte 255, byte 127, pulseIntensity) // bright orange
            elif Map.exists (fun key _ -> match key with Magic (true, _) -> true | _ -> false) statuses then Color (byte 255, byte 127, byte 255, pulseIntensity) // bright purple
            elif Map.exists (fun key _ -> match key with Shield (true, _) -> true | _ -> false) statuses then Color (byte 127, byte 255, byte 127, pulseIntensity) // bright yellow
            elif Map.containsKey Confuse statuses then Color (byte 191, byte 191, byte 255, pulseIntensity) // blue-green
            elif Map.containsKey Sleep statuses then Color (byte 0, byte 0, byte 255, pulseIntensity) // blue
            elif Map.containsKey Silence statuses then Color (byte 255,byte 255, byte 0, pulseIntensity) // orange
            elif Map.containsKey Poison statuses then Color (byte 0, byte 191, byte 0, pulseIntensity) // green
            elif Map.exists (fun key _ -> match key with Time false -> true | _ -> false) statuses then Color (byte 127, byte 127, byte 127, pulseIntensity) // dark white
            elif Map.exists (fun key _ -> match key with Power (false, _) -> true | _ -> false) statuses then Color (byte 127, byte 127, byte 0, pulseIntensity) // dark orange
            elif Map.exists (fun key _ -> match key with Magic (false, _) -> true | _ -> false) statuses then Color (byte 127, byte 0, byte 127, pulseIntensity) // dark purple
            elif Map.exists (fun key _ -> match key with Shield (false, _) -> true | _ -> false) statuses then Color (byte 0, byte 127, byte 0, pulseIntensity) // dark yellow
            else Color.Zero

        static let getAfflictionInsetOpt (character : Character) world =
            if character.IsHealthy && not character.IsWounding then
                let statuses = character.Statuses
                let celYOpt =
                    if character.IsWounded then None
                    elif Map.containsKey Confuse statuses then Some 3
                    elif Map.containsKey Sleep statuses then Some 2
                    elif Map.containsKey Silence statuses then Some 1
                    elif Map.containsKey Poison statuses then Some 0
                    elif Map.exists (fun key _ -> match key with Time false -> true | _ -> false) statuses then Some 4
                    elif Map.exists (fun key _ -> match key with Power (false, _) -> true | _ -> false) statuses then Some 5
                    elif Map.exists (fun key _ -> match key with Magic (false, _) -> true | _ -> false) statuses then Some 6
                    elif Map.exists (fun key _ -> match key with Shield (false, _) -> true | _ -> false) statuses then Some 7
                    else None
                match celYOpt with
                | Some afflictionY ->
                    let time = World.getUpdateTime world
                    let afflictionX = time / 8L % 8L |> int
                    let inset =
                        v4Bounds
                            (v2 (single afflictionX * Constants.Battle.AfflictionCelSize.X) (single afflictionY * Constants.Battle.AfflictionCelSize.Y))
                            Constants.Battle.AfflictionCelSize
                    Some inset
                | None -> None
            else None

        static let getChargeOrbInsetOpt (character : Character) world =
            if character.IsHealthy && not character.IsWounding then
                let celXOpt =
                    match character.ChargeTechOpt with
                    | Some (_, chargeAmount, _) ->
                        if chargeAmount < 3 then Some 0
                        elif chargeAmount < 6 then Some 1
                        elif chargeAmount < 9 then Some 2
                        elif chargeAmount < 12 then Some 3
                        else World.getUpdateTime world / 12L % 4L + 4L |> int |> Some
                    | None -> None
                match celXOpt with
                | Some celX ->
                    let inset = v4Bounds (v2 (single celX * Constants.Battle.ChargeOrbCelSize.X) 0.0f) Constants.Battle.ChargeOrbCelSize
                    Some inset
                | None -> None
            else None

        override this.Initializers (character, _) =
            [Entity.Omnipresent == true
             Entity.Bounds <== character --> fun character -> character.Bounds]

        override this.View (character, entity, world) =
            if entity.GetVisible world && entity.GetInView world then
                let transform = entity.GetTransform world
                let characterView =
                    Render (transform.Elevation, transform.Position.Y, AssetTag.generalize character.AnimationSheet,
                        SpriteDescriptor
                            { Transform = transform
                              Absolute = entity.GetAbsolute world
                              Offset = Vector2.Zero
                              InsetOpt = Some (getSpriteInset character world)
                              Image = character.AnimationSheet
                              Color = getSpriteColor character world
                              Blend = Transparent
                              Glow = getSpriteGlow character world
                              Flip = FlipNone })
                let afflictionView =
                    match getAfflictionInsetOpt character world with
                    | Some _ as insetOpt ->
                        let image = Assets.Battle.AfflictionsAnimationSheet
                        let position =
                            match character.Stature with
                            | SmallStature | NormalStature | LargeStature ->
                                transform.Position + transform.Size - Constants.Battle.AfflictionSize
                            | BossStature ->
                                transform.Position + transform.Size - Constants.Battle.ChargeOrbSize.MapX((*) 2.0f).MapY((*) 1.75f)
                        let transform = { transform with Position = position; Size = Constants.Battle.AfflictionSize }
                        Render (transform.Elevation + 0.1f, transform.Position.Y, AssetTag.generalize image,
                            SpriteDescriptor
                                { Transform = transform
                                  Absolute = entity.GetAbsolute world
                                  Offset = Vector2.Zero
                                  InsetOpt = insetOpt
                                  Image = image
                                  Color = colWhite
                                  Blend = Transparent
                                  Glow = colZero
                                  Flip = FlipNone })
                    | None -> View.empty
                let chargeOrbView =
                    match getChargeOrbInsetOpt character world with
                    | Some _ as insetOpt ->
                        let image = Assets.Battle.ChargeOrbAnimationSheet
                        let position =
                            match character.Stature with
                            | SmallStature | NormalStature | LargeStature ->
                                transform.Position + transform.Size - Constants.Battle.ChargeOrbSize.MapX((*) 1.5f)
                            | BossStature ->
                                transform.Position + transform.Size - Constants.Battle.ChargeOrbSize.MapX((*) 2.5f).MapY((*) 1.75f)
                        let transform = { transform with Position = position; Size = Constants.Battle.ChargeOrbSize }
                        Render (transform.Elevation + 0.1f, transform.Position.Y, AssetTag.generalize image,
                            SpriteDescriptor
                                { Transform = transform
                                  Absolute = entity.GetAbsolute world
                                  Offset = Vector2.Zero
                                  InsetOpt = insetOpt
                                  Image = image
                                  Color = colWhite
                                  Blend = Transparent
                                  Glow = colZero
                                  Flip = FlipNone })
                    | None -> View.empty
                Views [|characterView; afflictionView; chargeOrbView|]
            else View.empty