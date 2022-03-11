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
        member this.GetProp = this.GetModelGeneric<Prop>
        member this.SetProp = this.SetModelGeneric<Prop>
        member this.Prop = this.ModelGeneric<Prop> ()

    type PropDispatcher () =
        inherit EntityDispatcher<Prop, unit, unit> (Prop.initial)

        static let getSpriteInsetOpt prop =
            let xOffset =
                match prop.PropAnimationType with
                | PropAnimationStanding -> single 0
                | PropAnimationDestroyed -> single 1
            let spriteOffset =
                v2
                    (Constants.Gameplay.TileSize.X * xOffset)
                    (Constants.Gameplay.TileSize.Y * single 0)
            let spriteInset = v4Bounds spriteOffset Constants.Gameplay.TileSize
            Some spriteInset
        
        static member Properties =
            [define Entity.Elevation Constants.Gameplay.PropElevation
             define Entity.Omnipresent true]
        
        override this.Initializers (prop, _) =
            [Entity.Position <== prop --> fun prop -> prop.Position]
        
        override this.View (prop, entity, world) =
            if entity.GetVisible world && entity.GetInView world then
                let transform = entity.GetTransform world
                Render (transform.Elevation, transform.Position.Y, AssetTag.generalize prop.PropImage,
                    SpriteDescriptor
                        { Transform = transform
                          Absolute = entity.GetAbsolute world
                          Offset = v2Zero
                          InsetOpt = getSpriteInsetOpt prop
                          Image = prop.PropImage
                          Color = Color.White
                          Blend = Transparent
                          Glow = Color.Zero
                          Flip = FlipNone })
            else View.empty