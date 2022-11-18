// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace OmniBlade
open System
open System.Numerics
open Prime
open Nu
open Nu.Declarative

[<AutoOpen>]
module SpiritOrbDispatcher =

    type [<ReferenceEquality; NoComparison>] SpiritOrb =
        { AvatarLowerCenter : Vector3
          Spirits : Spirit array
          Chests : Chest array
          Portals : Portal array }

    type [<NoEquality; NoComparison>] SpiritOrbInhabitant =
        | SpiritInhabitant of Spirit
        | ChestInhabitant of Chest
        | PortalInhabitant of Portal

    type Entity with
        member this.GetSpiritOrb world = this.GetModelGeneric<SpiritOrb> world
        member this.SetSpiritOrb value world = this.SetModelGeneric<SpiritOrb> value world
        member this.SpiritOrb = this.ModelGeneric<SpiritOrb> ()

    type SpiritOrbDispatcher () =
        inherit GuiDispatcher<SpiritOrb, unit, unit> ({ AvatarLowerCenter = v3Zero; Spirits = [||]; Chests = [||]; Portals = [||] })

        static let makeViews avatarLowerCenter (orbTransform : Transform) inhabitants =
            let mutable orbTransform = orbTransform
            Array.fold (fun views inhabitant ->
                let (position, image, color) =
                    match inhabitant with
                    | SpiritInhabitant spirit ->
                        let color = SpiritType.getColor spirit.SpiritType
                        (spirit.Position, Assets.Field.SpiritImage, color)
                    | ChestInhabitant chest ->
                        let image = if chest.Opened then Assets.Field.SpiritChestOpenedImage else Assets.Field.SpiritChestClosedImage
                        let color = Color.One.WithA 0.5f
                        (chest.Center, image, color)
                    | PortalInhabitant portal ->
                        let image = if portal.Active then Assets.Field.SpiritPortalImage else Assets.Default.ImageEmpty
                        let color = Color.One.WithA 0.5f
                        (portal.Center, image, color)
                let delta = position - avatarLowerCenter
                let distance = delta.Magnitude
                if distance < Constants.Field.SpiritRadius then
                    let position = orbTransform.Perimeter.Center + delta * Constants.Field.SpiritOrbRatio - Constants.Field.SpiritOrbBlipSize * 0.5f
                    let mutable transform = Transform.makeDefault false
                    transform.Position <- position
                    transform.Size <- Constants.Field.SpiritOrbBlipSize
                    transform.Elevation <- orbTransform.Elevation + 2.0f
                    transform.Absolute <- orbTransform.Absolute
                    let colorFadeIn =
                        let distanceNormalized = (Constants.Field.SpiritRadius - distance) / Constants.Field.SpiritRadius
                        if distanceNormalized < 0.25f then color.MapA ((*) (distanceNormalized / 0.25f)) else color
                    let descriptor = { Transform = transform; InsetOpt = ValueNone; Image = image; Blend = Transparent; Color = colorFadeIn; Glow = Color.Zero; Flip = FlipNone }
                    let view = Render2d (transform.Elevation, transform.Perimeter.Position.Y, AssetTag.generalize image, SpriteDescriptor descriptor)
                    view :: views
                else views)
                [] inhabitants

        override this.View (spiritOrb, entity, world) =
            let mutable orbTransform = entity.GetTransform world
            let orbImage = Assets.Field.SpiritOrbImage
            let orbDescriptor = { Transform = orbTransform; InsetOpt = ValueNone; Image = orbImage; Color = Color.One; Blend = Transparent; Glow = Color.Zero; Flip = FlipNone }
            let orbView = Render2d (orbTransform.Elevation, orbTransform.Perimeter.Position.Y, AssetTag.generalize orbImage, SpriteDescriptor orbDescriptor)
            let mutable avatarTransform = Transform.makeDefault false
            avatarTransform.Position <- orbTransform.Position + orbTransform.Size * 0.5f - Constants.Field.SpiritOrbBlipSize * 0.5f
            avatarTransform.Size <- Constants.Field.SpiritOrbBlipSize
            avatarTransform.Elevation <- orbTransform.Elevation + 1.0f
            avatarTransform.Absolute <- orbTransform.Absolute
            let avatarImage = Assets.Field.SpiritAvatarImage
            let avatarDescriptor = { Transform = avatarTransform; InsetOpt = ValueNone; Image = avatarImage; Color = Color.One; Blend = Transparent; Glow = Color.Zero; Flip = FlipNone }
            let avatarView = Render2d (avatarTransform.Elevation, avatarTransform.Perimeter.Position.Y, AssetTag.generalize avatarImage, SpriteDescriptor avatarDescriptor)
            let spiritViews = makeViews spiritOrb.AvatarLowerCenter orbTransform (Array.map SpiritInhabitant spiritOrb.Spirits)
            let chestViews = makeViews spiritOrb.AvatarLowerCenter orbTransform (Array.map ChestInhabitant spiritOrb.Chests)
            let portalViews = makeViews spiritOrb.AvatarLowerCenter orbTransform (Array.map PortalInhabitant spiritOrb.Portals)
            let views = orbView :: avatarView :: spiritViews @ chestViews @ portalViews
            Views (List.toArray views)