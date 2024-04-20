// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace OmniBlade
open System
open Prime
open Nu

[<AutoOpen>]
module CharacterExtensions =
    type Entity with
        member this.GetCharacterPlus world = this.GetModelGeneric<CharacterPlus> world
        member this.SetCharacterPlus value world = this.SetModelGeneric<CharacterPlus> value world
        member this.CharacterPlus = this.ModelGeneric<CharacterPlus> ()

type CharacterDispatcher () =
    inherit Entity2dDispatcher<CharacterPlus, Message, Command> (true, CharacterPlus.empty)

    static let getAfflictionInsetOpt time (character : Character) =
        if character.Standing then
            let statuses = character.Statuses
            let celYOpt =
                if character.Wounded then None
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
                let afflictionX = time / 8L % 8L |> int
                let afflictionPosition = v2 (single afflictionX * Constants.Battle.AfflictionCelSize.X) (single afflictionY * Constants.Battle.AfflictionCelSize.Y)
                let inset = box2 afflictionPosition Constants.Battle.AfflictionCelSize
                Some inset
            | None -> None
        else None

    static let getChargeOrbInsetOpt time (character : Character) =
        if character.Standing then
            let celXOpt =
                match (character.ConjureChargeOpt, character.TechChargeOpt |> Option.map Triple.snd) with
                | (Some chargeAmount, _)
                | (_, Some chargeAmount) ->
                    if chargeAmount < 3 then Some 0
                    elif chargeAmount < 6 then Some 1
                    elif chargeAmount < 9 then Some 2
                    elif chargeAmount < 12 then Some 3
                    else time / 12L % 4L + 4L |> int |> Some
                | (None, None) -> None
            match celXOpt with
            | Some celX ->
                let chargeOrbPosition = v2 (single celX * Constants.Battle.ChargeOrbCelSize.X) 0.0f
                let inset = box2 chargeOrbPosition Constants.Battle.ChargeOrbCelSize
                Some inset
            | None -> None
        else None

    override this.Definitions (characterPlus, _) =
        let character = characterPlus.Character
        [Entity.Presence == Omnipresent
         Entity.Perimeter := character.Perimeter
         Entity.Elevation == Constants.Battle.ForegroundElevation]

    override this.Render (characterPlus, _, entity, world) =
        let time = characterPlus.UpdateTime
        let character = characterPlus.Character
        if entity.GetVisible world then
            let mutable transform = entity.GetTransform world
            let perimeter = transform.Perimeter
            World.enqueueLayeredOperation2d
                { Elevation = transform.Elevation
                  Horizon = transform.Horizon
                  AssetTag = character.AnimationSheet
                  RenderOperation2d =
                    RenderSprite
                        { Transform = transform
                          InsetOpt = ValueSome (Character.getAnimationInset time character)
                          Image = character.AnimationSheet
                          Color = Character.getAnimationColor time character
                          Blend = Transparent
                          Emission = Character.getAnimationEmission time character
                          Flip = FlipNone }}
                world
            match getAfflictionInsetOpt time character with
            | Some afflictionInset ->
                let afflictionImage = Assets.Battle.AfflictionsAnimationSheet
                let afflictionPosition =
                    match character.Stature with
                    | SmallStature | NormalStature ->
                        perimeter.Min + perimeter.Size - Constants.Battle.AfflictionSize
                    | LargeStature ->
                        perimeter.Min + perimeter.Size - Constants.Battle.AfflictionSize.MapY((*) 0.5f)
                    | BossStature ->
                        perimeter.Min + perimeter.Size - Constants.Battle.AfflictionSize.MapX((*) 2.0f).MapY((*) 1.75f)
                let mutable afflictionTransform = Transform.makeDefault false
                afflictionTransform.Position <- afflictionPosition
                afflictionTransform.Size <- Constants.Battle.AfflictionSize
                afflictionTransform.Elevation <- transform.Elevation + 0.1f
                World.enqueueLayeredOperation2d
                    { Elevation = afflictionTransform.Elevation
                      Horizon = afflictionTransform.Horizon
                      AssetTag = afflictionImage
                      RenderOperation2d =
                        RenderSprite
                            { Transform = afflictionTransform
                              InsetOpt = ValueSome afflictionInset
                              Image = afflictionImage
                              Color = Color.One
                              Blend = Transparent
                              Emission = Color.Zero
                              Flip = FlipNone }}
                    world
            | None -> ()
            match getChargeOrbInsetOpt time character with
            | Some chargeOrbInset ->
                let chargeOrbImage = Assets.Battle.ChargeOrbAnimationSheet
                let chargeOrbPosition =
                    match character.Stature with
                    | SmallStature | NormalStature ->
                        perimeter.Min + perimeter.Size - Constants.Battle.ChargeOrbSize.MapX((*) 1.5f)
                    | LargeStature ->
                        perimeter.Min + perimeter.Size - Constants.Battle.ChargeOrbSize.MapX((*) 1.5f).MapY((*) 0.5f)
                    | BossStature ->
                        perimeter.Min + perimeter.Size - Constants.Battle.ChargeOrbSize.MapX((*) 2.5f).MapY((*) 1.75f)
                let mutable chargeOrbTransform = Transform.makeDefault false
                chargeOrbTransform.Position <- chargeOrbPosition
                chargeOrbTransform.Size <- Constants.Battle.ChargeOrbSize
                chargeOrbTransform.Elevation <- transform.Elevation + 0.1f
                World.enqueueLayeredOperation2d
                    { Elevation = chargeOrbTransform.Elevation
                      Horizon = chargeOrbTransform.Horizon
                      AssetTag = chargeOrbImage
                      RenderOperation2d =
                        RenderSprite
                            { Transform = chargeOrbTransform
                              InsetOpt = ValueSome chargeOrbInset
                              Image = chargeOrbImage
                              Color = Color.One
                              Blend = Transparent
                              Emission = Color.Zero
                              Flip = FlipNone }}
                    world
            | None -> ()