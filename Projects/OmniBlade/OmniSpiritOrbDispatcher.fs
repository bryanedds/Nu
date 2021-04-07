// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace OmniBlade
open System
open System.Numerics
open Prime
open Nu
open OmniBlade

[<AutoOpen>]
module SpiritOrbDispatcher =

    type [<ReferenceEquality; NoComparison>] SpiritOrb =
        { AvatarLowerCenter : Vector2
          Spirits : Spirit array }

    type Entity with
        member this.GetSpiritOrb = this.GetModel<SpiritOrb>
        member this.SetSpiritOrb = this.SetModel<SpiritOrb>
        member this.SpiritOrb = this.Model<SpiritOrb> ()

    type SpiritOrbDispatcher () =
        inherit GuiDispatcher<SpiritOrb, unit, unit> ({ AvatarLowerCenter = v2Zero; Spirits = [||] })

        override this.View (spiritOrb, entity, world) =
            let orbTransform = entity.GetTransform world
            let orbImage = Assets.Field.SpiritOrbImage
            let orbDescriptor = { Transform = orbTransform; Absolute = entity.GetAbsolute world; Offset = v2Zero; InsetOpt = None; Image = orbImage; Color = colWhite; Blend = Transparent; Glow = colZero; Flip = FlipNone }
            let orbView = Render (orbTransform.Elevation, orbTransform.Position.Y, AssetTag.generalize orbImage, SpriteDescriptor orbDescriptor)
            let avatarTransform = { Position = orbTransform.Position + orbTransform.Size * 0.5f - Constants.Field.SpiritOrbBlipSize * 0.5f; Size = Constants.Field.SpiritOrbBlipSize; Elevation = orbTransform.Elevation + 1.0f; Rotation = 0.0f; Flags = 0 }
            let avatarImage = Assets.Field.SpiritAvatar
            let avatarDescriptor = { Transform = avatarTransform; Absolute = entity.GetAbsolute world; Offset = v2Zero; InsetOpt = None; Image = avatarImage; Color = colWhite; Blend = Transparent; Glow = colZero; Flip = FlipNone }
            let avatarView = Render (avatarTransform.Elevation, avatarTransform.Position.Y, AssetTag.generalize avatarImage, SpriteDescriptor avatarDescriptor)
            let spiritViews =
                Array.fold (fun spiritViews (spirit : Spirit) ->
                    let delta = spirit.Position - spiritOrb.AvatarLowerCenter
                    let distance = delta.Length ()
                    if distance < Constants.Field.SpiritRadius then
                        let position = entity.GetCenter world + delta * Constants.Field.SpiritOrbRatio - Constants.Field.SpiritOrbBlipSize * 0.5f
                        let bounds = v4Bounds position Constants.Field.SpiritOrbBlipSize
                        let transform = { Position = bounds.Position; Size = bounds.Size; Elevation = orbTransform.Elevation + 2.0f; Rotation = 0.0f; Flags = 0 }
                        let image = Assets.Field.SpiritImage
                        let color = SpiritType.getColor spirit.SpiritType
                        let colorFadeIn =
                            let distanceNormalized = (Constants.Field.SpiritRadius - distance) / Constants.Field.SpiritRadius
                            if distanceNormalized < 0.333f then color.MapA (fun a -> single a * (distanceNormalized - 0.667f) / 0.333f |> byte) else color
                        let descriptor = { Transform = transform; Absolute = entity.GetAbsolute world; Offset = v2Zero; InsetOpt = None; Image = image; Blend = Transparent; Color = colorFadeIn; Glow = colZero; Flip = FlipNone }
                        let spiritView = Render (transform.Elevation, transform.Position.Y, AssetTag.generalize image, SpriteDescriptor descriptor)
                        spiritView :: spiritViews
                    else spiritViews)
                    [] spiritOrb.Spirits
            let views = orbView :: avatarView :: spiritViews
            Views (List.toArray views)