// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace OmniBlade
open System
open Prime
open Nu
open Nu.Declarative
open OmniBlade

[<AutoOpen>]
module TeammateDispatcher =

    type Entity with
        member this.GetTeammate world = this.GetModelGeneric<Teammate> world
        member this.SetTeammate value world = this.SetModelGeneric<Teammate> value world
        member this.Teammate = this.ModelGeneric<Teammate> ()

    type TeammateDispatcher () =
        inherit GuiDispatcher<Teammate, Message, Command> (Teammate.empty)

        static let viewFillBar borderImage (borderImageColor : Color) fillImage (fillImageColor : Color) (transform : Transform byref) fill =

            // border sprite
            let perimeter = transform.Perimeter // gui currently ignores rotation
            let horizon = transform.Horizon
            let mutable borderTransform = Transform.makeDefault transform.Centered
            borderTransform.Position <- perimeter.Min
            borderTransform.Size <- perimeter.Size
            borderTransform.Offset <- transform.Offset
            borderTransform.Elevation <- transform.Elevation + 0.5f
            borderTransform.Absolute <- transform.Absolute
            let borderView =
                Render2d (borderTransform.Elevation, horizon, AssetTag.generalize borderImage, 
                    RenderSprite
                        { Transform = borderTransform
                          InsetOpt = ValueNone
                          Image = borderImage
                          Color = borderImageColor
                          Blend = Transparent
                          Emission = Color.Zero
                          Flip = FlipNone })

            // fill sprite
            let fillSize = perimeter.Size
            let fillInset = fillSize * (1.0f / 24.0f)
            let fillPosition = perimeter.Min + fillInset
            let fillWidth = (fillSize.X - fillInset.X * 2.0f) * fill
            let fillHeight = fillSize.Y - fillInset.Y * 2.0f
            let fillSize = v3 fillWidth fillHeight 0.0f
            let mutable fillTransform = Transform.makeDefault transform.Centered
            fillTransform.Position <- fillPosition
            fillTransform.Size <- fillSize
            fillTransform.Offset <- transform.Offset
            fillTransform.Elevation <- transform.Elevation
            fillTransform.Absolute <- transform.Absolute
            let fillView =
                Render2d (fillTransform.Elevation, horizon, AssetTag.generalize fillImage,
                    RenderSprite
                        { Transform = fillTransform
                          InsetOpt = ValueNone
                          Image = fillImage
                          Color = fillImageColor
                          Blend = Transparent
                          Emission = Color.Zero
                          Flip = FlipNone })

            // fin
            Views [|borderView; fillView|]

        static member Facets =
            [typeof<TextFacet>
             typeof<ButtonFacet>]

        override this.Initialize (teammate, _) =
            [Entity.Text := teammate.Name
             Entity.TextColor == Color.White
             Entity.TextDisabledColor == color8Dup (byte 192) // TODO: make constant?
             Entity.Font == Assets.Gui.Font
             Entity.DownOffset == v2 0.0f -3.0f
             Entity.UpImage == Assets.Gui.ButtonBigUpImage
             Entity.DownImage == Assets.Gui.ButtonBigDownImage
             Entity.ClickSoundOpt == Some Assets.Gui.AffirmSound]

        override this.View (character, entity, world) =
            let mutable transform = entity.GetTransform world
            let mutable hitPointsTransform = transform
            let downOffset = if entity.GetDown world then entity.GetDownOffset world else v2Zero
            hitPointsTransform.Position <- v3 (hitPointsTransform.Min.X + 102.0f) (hitPointsTransform.Min.Y + 16.0f + downOffset.Y) 0.0f
            hitPointsTransform.Size <- v3 48.0f 6.0f 0.0f
            hitPointsTransform.Elevation <- hitPointsTransform.Elevation + 0.25f
            let hitPointsView =
                viewFillBar
                    Assets.Gui.HealthBorderImage
                    (color8 (byte 51) (byte 51) (byte 51) (byte 255)) // TODO: use a constant.
                    Assets.Default.White
                    (Color.Red.WithA8 (byte 131)) // TODO: use a constant.
                    &hitPointsTransform
                    (single character.HitPoints / single character.HitPointsMax)
            let mutable techPointsTransform = transform
            techPointsTransform.Position <- v3 (techPointsTransform.Min.X + 102.0f) (techPointsTransform.Min.Y + 12.0f + downOffset.Y) 0.0f
            techPointsTransform.Size <- v3 48.0f 6.0f 0.0f
            techPointsTransform.Elevation <- techPointsTransform.Elevation + 0.25f
            let techPointsView =
                viewFillBar
                    Assets.Gui.HealthBorderImage
                    (color8 (byte 51) (byte 51) (byte 51) (byte 255)) // TODO: use a constant.
                    Assets.Default.White
                    ((color8 (byte 74) (byte 91) (byte 169) (byte 255)).WithA8 (byte 131)) // TODO: use a constant.
                    &techPointsTransform
                    (single character.TechPoints / single character.TechPointsMax)
            Views [|hitPointsView; techPointsView|]