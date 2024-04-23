// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace OmniBlade
open System
open Prime
open Nu

[<AutoOpen>]
module SpiritOrbExtensions =
    type Entity with
        member this.GetSpiritOrb world = this.GetModelGeneric<SpiritOrb> world
        member this.SetSpiritOrb value world = this.SetModelGeneric<SpiritOrb> value world
        member this.SpiritOrb = this.ModelGeneric<SpiritOrb> ()

type SpiritOrbDispatcher () =
    inherit GuiDispatcher<SpiritOrb, Message, Command>
        ({ AvatarLowerCenter = v3Zero
           ShowUnopenedChests = true
           Chests = [||]
           Portals = [||]
           Narratives = [||]
           Spirits = [||] })

    static let renderInhabitants time avatarLowerCenter (orbTransform : Transform) inhabitants world =
        let mutable orbTransform = orbTransform
        for inhabitant in inhabitants do
            let (center, image, color, insetOpt) =
                match inhabitant with
                | ChestInhabitant chest ->
                    let image = if chest.Opened then Assets.Field.SpiritChestOpenedImage else Assets.Field.SpiritChestClosedImage
                    let color = Color.One.WithA 0.5f
                    (chest.Center, image, color, ValueNone)
                | PortalInhabitant portal ->
                    let image = if portal.Active then Assets.Field.SpiritPortalImage else Assets.Default.EmptyImage
                    let color = Color.One.WithA 0.5f
                    (portal.Center, image, color, ValueNone)
                | NarrativeInhabitant narrative ->
                    let image = if narrative.Active then Assets.Field.SpiritNarrativeImage else Assets.Default.EmptyImage
                    let color = Color.One.WithA 0.5f
                    let column = time % 48L / 12L
                    let inset = box2 (v2 (7.0f * single column) 0.0f) (v2Dup 7.0f)
                    (narrative.Center, image, color, ValueSome inset)
                | SpiritInhabitant spirit ->
                    let color = SpiritType.getColor spirit.SpiritType
                    (spirit.Center, Assets.Field.SpiritImage, color, ValueNone)
            let delta = center - avatarLowerCenter
            let distance = delta.Magnitude
            if distance < Constants.Field.SpiritRadius then
                let position = orbTransform.PerimeterCenter + delta * Constants.Field.SpiritOrbRatio - Constants.Field.SpiritOrbBlipSize * 0.5f
                let mutable transform = Transform.makeDefault false
                transform.Position <- position
                transform.Size <- Constants.Field.SpiritOrbBlipSize
                transform.Elevation <- orbTransform.Elevation + 2.0f
                transform.Absolute <- orbTransform.Absolute
                let colorFadeIn =
                    let distanceNormalized = (Constants.Field.SpiritRadius - distance) / Constants.Field.SpiritRadius
                    if distanceNormalized < 0.25f then color.MapA ((*) (distanceNormalized / 0.25f)) else color
                let descriptor = { Transform = transform; InsetOpt = insetOpt; Image = image; Blend = Transparent; Color = colorFadeIn; Emission = Color.Zero; Flip = FlipNone }
                World.enqueueLayeredOperation2d
                    { Elevation = transform.Elevation
                      Horizon = transform.Horizon
                      AssetTag = image
                      RenderOperation2d = RenderSprite descriptor }
                    world

    override this.Render (spiritOrb, _, entity, world) =
        let mutable orbTransform = entity.GetTransform world
        let orbImage = Assets.Field.SpiritOrbImage
        let orbDescriptor = { Transform = orbTransform; InsetOpt = ValueNone; Image = orbImage; Color = Color.One; Blend = Transparent; Emission = Color.Zero; Flip = FlipNone }
        World.enqueueLayeredOperation2d
            { Elevation = orbTransform.Elevation
              Horizon = orbTransform.Horizon
              AssetTag = orbImage
              RenderOperation2d = RenderSprite orbDescriptor }
            world
        let mutable avatarTransform = Transform.makeDefault false
        avatarTransform.Position <- orbTransform.Position + orbTransform.Size * 0.5f - Constants.Field.SpiritOrbBlipSize * 0.5f
        avatarTransform.Size <- Constants.Field.SpiritOrbBlipSize
        avatarTransform.Elevation <- orbTransform.Elevation + 1.0f
        avatarTransform.Absolute <- orbTransform.Absolute
        let avatarImage = Assets.Field.SpiritAvatarImage
        let avatarDescriptor = { Transform = avatarTransform; InsetOpt = ValueNone; Image = avatarImage; Color = Color.One; Blend = Transparent; Emission = Color.Zero; Flip = FlipNone }
        World.enqueueLayeredOperation2d
            { Elevation = avatarTransform.Elevation
              Horizon = avatarTransform.Horizon
              AssetTag = avatarImage
              RenderOperation2d = RenderSprite avatarDescriptor }
            world
        let chests = Array.filter (fun (chest : Chest) -> spiritOrb.ShowUnopenedChests || chest.Opened) spiritOrb.Chests
        renderInhabitants world.UpdateTime spiritOrb.AvatarLowerCenter orbTransform (Array.map ChestInhabitant chests) world
        renderInhabitants world.UpdateTime spiritOrb.AvatarLowerCenter orbTransform (Array.map PortalInhabitant spiritOrb.Portals) world
        renderInhabitants world.UpdateTime spiritOrb.AvatarLowerCenter orbTransform (Array.map NarrativeInhabitant spiritOrb.Narratives) world
        renderInhabitants world.UpdateTime spiritOrb.AvatarLowerCenter orbTransform (Array.map SpiritInhabitant spiritOrb.Spirits) world