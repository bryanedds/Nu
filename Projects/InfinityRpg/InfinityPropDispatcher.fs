namespace InfinityRpg
open System
open System.Numerics
open Prime
open Nu
open Nu.Declarative
open InfinityRpg

[<AutoOpen>]
module PropDispatcher =

    type Entity with
        member this.GetProp = this.GetModel<Prop>
        member this.SetProp = this.SetModel<Prop>
        member this.Prop = this.Model<Prop> ()

    type PropDispatcher () =
        inherit EntityDispatcher<Prop, unit, unit> (Prop.initial)

        static let getSpriteInsetOpt prop =
            let spriteOffset =
                v2
                    (Constants.Layout.TileSize.X * single 0)
                    (Constants.Layout.TileSize.Y * single 0)
            let spriteInset = v4Bounds spriteOffset Constants.Layout.TileSize
            Some spriteInset
        
        static member Properties =
            [define Entity.Elevation Constants.Layout.PropElevation
             define Entity.PublishChanges true
             define Entity.Omnipresent true]
        
        override this.Initializers (prop, _) =
            [Entity.Position <== prop --> fun prop -> prop.Position]
        
        override this.View (prop, entity, world) =
            if entity.GetVisible world && entity.GetInView world then
                let transform = entity.GetTransform world
                [Render (transform.Elevation, transform.Position.Y, AssetTag.generalize prop.PropImage,
                     SpriteDescriptor
                       { Transform = transform
                         Offset = v2Zero
                         InsetOpt = getSpriteInsetOpt prop
                         Image = prop.PropImage
                         Color = Color.White
                         Glow = Color.Zero
                         Flip = FlipNone })]
            else []