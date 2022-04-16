// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace OmniBlade
open System
open System.Numerics
open Prime
open Nu
open Nu.Declarative
open OmniBlade

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
                    | SpiritInhabitant spirit -> (spirit.Position, Assets.Field.SpiritImage, SpiritType.getColor spirit.SpiritType)
                    | ChestInhabitant chest ->
                        let image = if chest.Opened then Assets.Field.SpiritChestOpenedImage else Assets.Field.SpiritChestClosedImage
                        let color = colWhite.WithA (byte 127)
                        (chest.Center, image, color)
                    | PortalInhabitant portal ->
                        let image = if portal.Active then Assets.Field.SpiritPortalImage else Assets.Default.ImageEmpty
                        let color = colWhite.WithA (byte 127)
                        (portal.Center, image, color)
                let delta = position - avatarLowerCenter
                let distance = delta.Length ()
                if distance < Constants.Field.SpiritRadius then
                    let position = orbTransform.DimensionsRaw.Center + delta * Constants.Field.SpiritOrbRatio - Constants.Field.SpiritOrbBlipSize * 0.5f
                    let mutable transform = Transform.make2d ()
                    transform.Position <- position
                    transform.Size <- Constants.Field.SpiritOrbBlipSize
                    transform.Elevation <- orbTransform.Elevation + 2.0f
                    let colorFadeIn =
                        let distanceNormalized = (Constants.Field.SpiritRadius - distance) / Constants.Field.SpiritRadius
                        if distanceNormalized < 0.25f then color.MapA (fun a -> single a * (distanceNormalized / 0.25f) |> byte) else color
                    let descriptor = { Transform = transform; InsetOpt = None; Image = image; Blend = Transparent; Color = colorFadeIn; Glow = colZero; Flip = FlipNone }
                    let view = Render2d (transform.Elevation, transform.DimensionsRaw.Position.Y, AssetTag.generalize image, SpriteDescriptor descriptor)
                    view :: views
                else views)
                [] inhabitants

        override this.View (spiritOrb, entity, world) =
            let mutable orbTransform = entity.GetTransform world
            let orbImage = Assets.Field.SpiritOrbImage
            let orbDescriptor = { Transform = orbTransform; InsetOpt = None; Image = orbImage; Color = colWhite; Blend = Transparent; Glow = colZero; Flip = FlipNone }
            let orbView = Render2d (orbTransform.Elevation, orbTransform.DimensionsRaw.Position.Y, AssetTag.generalize orbImage, SpriteDescriptor orbDescriptor)
            let mutable avatarTransform = Transform.make2d ()
            avatarTransform.Position <- orbTransform.Position + orbTransform.Size * 0.5f - Constants.Field.SpiritOrbBlipSize * 0.5f
            avatarTransform.Size <- Constants.Field.SpiritOrbBlipSize
            avatarTransform.Elevation <- orbTransform.Elevation + 1.0f
            let avatarImage = Assets.Field.SpiritAvatarImage
            let avatarDescriptor = { Transform = avatarTransform; InsetOpt = None; Image = avatarImage; Color = colWhite; Blend = Transparent; Glow = colZero; Flip = FlipNone }
            let avatarView = Render2d (avatarTransform.Elevation, avatarTransform.DimensionsRaw.Position.Y, AssetTag.generalize avatarImage, SpriteDescriptor avatarDescriptor)
            let spiritViews = makeViews spiritOrb.AvatarLowerCenter orbTransform (Array.map SpiritInhabitant spiritOrb.Spirits)
            let chestViews = makeViews spiritOrb.AvatarLowerCenter orbTransform (Array.map ChestInhabitant spiritOrb.Chests)
            let portalViews = makeViews spiritOrb.AvatarLowerCenter orbTransform (Array.map PortalInhabitant spiritOrb.Portals)
            let views = orbView :: avatarView :: spiritViews @ chestViews @ portalViews
            Views (List.toArray views)