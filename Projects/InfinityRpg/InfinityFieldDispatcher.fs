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
        member this.GetField = this.GetModel<Field>
        member this.SetField = this.SetModel<Field>
        member this.Field = this.Model<Field> ()

    type FieldDispatcher () =
        inherit EntityDispatcher<Field, unit, unit> (Field.initial ())

        static let getTileInsetOpt (tileSheetCoordinates : Vector2i) =
            let tileOffset = vctovf tileSheetCoordinates
            let tileInset = v4Bounds tileOffset Constants.Layout.TileSize
            Some tileInset

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
            [Entity.Size <== field --> fun field -> vctovf field.FieldMapNp.FieldSizeC]
        
        override this.View (field, entity, world) =
            let fieldTransform = entity.GetTransform world
            let tileTransform = { fieldTransform with Size = Constants.Layout.TileSize }
            let absolute = entity.GetAbsolute world
            let bounds =
                v4BoundsOverflow
                    fieldTransform.Position
                    (Vector2.Multiply (Constants.Layout.TileSize, Constants.Layout.TileSheetSize))
                    (entity.GetOverflow world)
            if World.isBoundsInView absolute bounds world then
                let fieldMap = field.FieldMapNp
                let image = fieldMap.FieldTileSheet
                let mInViewBounds = World.getViewBounds absolute world |> viewBoundsToMapUnits
                let tiles = fieldMap.FieldTiles
                let sprites =
                    Map.foldBack
                        (fun tileCoordinates tile sprites ->
                            if tilePositionInView tileCoordinates mInViewBounds then
                                let tilePosition = vctovf tileCoordinates // NOTE: field position assumed at origin
                                let tileInsetOpt = getTileInsetOpt tile.TileSheetCoordinates
                                let tileTransform = { tileTransform with Position = tilePosition }
                                let sprite =
                                    { Transform = tileTransform
                                      Offset = v2Zero
                                      InsetOpt = tileInsetOpt
                                      Image = image
                                      Color = Color.White
                                      Glow = Color.Zero
                                      Flip = FlipNone }
                                sprite :: sprites
                            else sprites)
                        tiles [] |>
                    Array.ofList
                Render (fieldTransform.Elevation, fieldTransform.Position.Y, AssetTag.generalize image, SpritesDescriptor sprites)
            else View.empty

        override this.GetQuickSize (entity, world) =
            vctovf ((entity.GetField world).FieldMapNp).FieldSizeC