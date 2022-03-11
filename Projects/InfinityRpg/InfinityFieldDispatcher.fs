namespace InfinityRpg
open System
open System.Numerics
open Prime
open Nu
open Nu.Declarative
open InfinityRpg

[<AutoOpen>]
module FieldDispatcher =

    type Entity with
        member this.GetField = this.GetModelGeneric<Field>
        member this.SetField = this.SetModelGeneric<Field>
        member this.Field = this.ModelGeneric<Field> ()

    type FieldDispatcher () =
        inherit EntityDispatcher<Field, unit, unit> (Field.initial)

        static let getTileInset (tileSheetCoordinates : Vector2i) =
            let tileOffset = vctovf tileSheetCoordinates
            let tileInset = v4Bounds tileOffset Constants.Gameplay.TileSize
            tileInset

        static let viewBoundsToMapUnits (viewBounds : Vector4) =
            let right = int viewBounds.X + int viewBounds.Z
            let top = int viewBounds.Y + int viewBounds.W
            v4i
                (itoc (int viewBounds.X))
                (itoc (int viewBounds.Y))
                (if Math.isSnapped right then (itoc right) - 1 else itoc right)
                (if Math.isSnapped top then (itoc top) - 1 else itoc top)

        static let tilePositionInView (tileCoordinates : Vector2i) (mInViewBounds : Vector4i) =
            tileCoordinates.X >= mInViewBounds.X &&
            tileCoordinates.Y >= mInViewBounds.Y &&
            tileCoordinates.X <= mInViewBounds.X + mInViewBounds.Z &&
            tileCoordinates.Y <= mInViewBounds.Y + mInViewBounds.W
        
        static member Properties =
            [define Entity.Omnipresent true]

        override this.Initializers (field, _) =
            [Entity.Size <== field --> fun field -> vctovf field.FieldMap.FieldSizeC]
        
        override this.View (field, entity, world) =
            let fieldTransform = entity.GetTransform world
            let tileTransform = { fieldTransform with Size = Constants.Gameplay.TileSize }
            let absolute = entity.GetAbsolute world
            let bounds =
                v4BoundsOverflow
                    fieldTransform.Position
                    (Vector2.Multiply (Constants.Gameplay.TileSize, Constants.Gameplay.TileSheetSize))
                    (entity.GetOverflow world)
            if World.isBoundsInView absolute bounds world then
                let fieldMap = field.FieldMap
                let image = fieldMap.FieldTileSheet
                let mInViewBounds = World.getViewBounds absolute world |> viewBoundsToMapUnits
                let tiles = fieldMap.FieldTiles
                let sprites =
                    Map.foldBack
                        (fun tileCoordinates tile sprites ->
                            if tilePositionInView tileCoordinates mInViewBounds then
                                let tilePosition = vctovf tileCoordinates // NOTE: field position assumed at origin
                                let tileTransform = { tileTransform with Position = tilePosition }
                                let tileInset = getTileInset tile.TileSheetCoordinates
                                let sprite : Sprite =
                                    { Transform = tileTransform
                                      Absolute = entity.GetAbsolute world
                                      Offset = v2Zero
                                      Inset = tileInset
                                      Image = image
                                      Color = Color.White
                                      Blend = Transparent
                                      Glow = Color.Zero
                                      Flip = FlipNone }
                                sprite :: sprites
                            else sprites)
                        tiles [] |>
                    Array.ofList
                Render (fieldTransform.Elevation, fieldTransform.Position.Y, AssetTag.generalize image, SpritesDescriptor { Sprites = sprites })
            else View.empty

        override this.GetQuickSize (entity, world) =
            vctovf ((entity.GetField world).FieldMap).FieldSizeC