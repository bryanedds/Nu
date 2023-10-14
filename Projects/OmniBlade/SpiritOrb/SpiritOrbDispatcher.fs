// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace OmniBlade
open System
open System.Numerics
open Prime
open Nu

[<AutoOpen>]
module SpiritOrbDispatcher =

    type Entity with
        member this.GetSpiritOrb world = this.GetModelGeneric<SpiritOrb> world
        member this.SetSpiritOrb value world = this.SetModelGeneric<SpiritOrb> value world
        member this.SpiritOrb = this.ModelGeneric<SpiritOrb> ()

    type SpiritOrbDispatcher () =
        inherit GuiDispatcher<SpiritOrb, Message, Command> ({ AvatarLowerCenter = v3Zero; ShowUnopenedChests = true; Spirits = [||]; Chests = [||]; Portals = [||] })

        static let makeViews avatarLowerCenter (orbTransform : Transform) inhabitants =
            let mutable orbTransform = orbTransform
            Array.fold (fun views inhabitant ->
                let (center, image, color) =
                    match inhabitant with
                    | ChestInhabitant chest ->
                        let image = if chest.Opened then Assets.Field.SpiritChestOpenedImage else Assets.Field.SpiritChestClosedImage
                        let color = Color.One.WithA 0.5f
                        (chest.Center, image, color)
                    | PortalInhabitant portal ->
                        let image = if portal.Active then Assets.Field.SpiritPortalImage else Assets.Default.ImageEmpty
                        let color = Color.One.WithA 0.5f
                        (portal.Center, image, color)
                    | SpiritInhabitant spirit ->
                        let color = SpiritType.getColor spirit.SpiritType
                        (spirit.Center, Assets.Field.SpiritImage, color)
                let delta = center - avatarLowerCenter
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
                    let descriptor = { Transform = transform; InsetOpt = ValueNone; Image = image; Blend = Transparent; Color = colorFadeIn; Emission = Color.Zero; Flip = FlipNone }
                    let view = Render2d (transform.Elevation, transform.Horizon, AssetTag.generalize image, RenderSprite descriptor)
                    view :: views
                else views)
                [] inhabitants

        override this.View (spiritOrb, entity, world) =
            let mutable orbTransform = entity.GetTransform world
            let orbImage = Assets.Field.SpiritOrbImage
            let orbDescriptor = { Transform = orbTransform; InsetOpt = ValueNone; Image = orbImage; Color = Color.One; Blend = Transparent; Emission = Color.Zero; Flip = FlipNone }
            let orbView = Render2d (orbTransform.Elevation, orbTransform.Horizon, AssetTag.generalize orbImage, RenderSprite orbDescriptor)
            let mutable avatarTransform = Transform.makeDefault false
            avatarTransform.Position <- orbTransform.Position + orbTransform.Size * 0.5f - Constants.Field.SpiritOrbBlipSize * 0.5f
            avatarTransform.Size <- Constants.Field.SpiritOrbBlipSize
            avatarTransform.Elevation <- orbTransform.Elevation + 1.0f
            avatarTransform.Absolute <- orbTransform.Absolute
            let avatarImage = Assets.Field.SpiritAvatarImage
            let avatarDescriptor = { Transform = avatarTransform; InsetOpt = ValueNone; Image = avatarImage; Color = Color.One; Blend = Transparent; Emission = Color.Zero; Flip = FlipNone }
            let avatarView = Render2d (avatarTransform.Elevation, avatarTransform.Horizon, AssetTag.generalize avatarImage, RenderSprite avatarDescriptor)
            let chests = Array.filter (fun (chest : Chest) -> spiritOrb.ShowUnopenedChests || chest.Opened) spiritOrb.Chests
            let chestViews = makeViews spiritOrb.AvatarLowerCenter orbTransform (Array.map ChestInhabitant chests)
            let portalViews = makeViews spiritOrb.AvatarLowerCenter orbTransform (Array.map PortalInhabitant spiritOrb.Portals)
            let spiritViews = makeViews spiritOrb.AvatarLowerCenter orbTransform (Array.map SpiritInhabitant spiritOrb.Spirits)
            let views = orbView :: avatarView :: chestViews @ portalViews @ spiritViews
            Views (List.toArray views)