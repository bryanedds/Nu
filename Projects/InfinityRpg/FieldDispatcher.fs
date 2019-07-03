namespace InfinityRpg
open System
open OpenTK
open Prime
open Nu
open Nu.Declarative
open InfinityRpg

[<AutoOpen>]
module FieldDispatcherModule =

    type Entity with
    
        member this.GetFieldMapNp = this.Get Property? FieldMapNp
        member this.SetFieldMapNp = this.Set Property? FieldMapNp
        member this.FieldMapNp = Lens.make<FieldMap, World> Property? FieldMapNp this.GetFieldMapNp this.SetFieldMapNp this

    type FieldDispatcher () =
        inherit EntityDispatcher ()

        static let DefaultRand = Rand.make ()
        static let DefaultSizeM = Vector2i (4, 4)
        static let DefaultPathEdgesM = [(Vector2i (1, 1), Vector2i (2, 2))]
        static let DefaultFieldMap = fst (FieldMap.make Assets.FieldTileSheetImage DefaultSizeM DefaultPathEdgesM DefaultRand)

        static let getTileInsetOpt (tileSheetPositionM : Vector2i) =
            let tileOffset = vmtovf tileSheetPositionM
            let tileInset =
                Vector4
                    (tileOffset.X,
                     tileOffset.Y,
                     tileOffset.X + Constants.Layout.TileSize.X,
                     tileOffset.Y + Constants.Layout.TileSize.Y)
            Some tileInset

        static member Properties =
            [define Entity.Omnipresent true
             define Entity.FieldMapNp DefaultFieldMap]

        override dispatcher.Actualize (field, world) =

            let viewType =
                field.GetViewType world

            let bounds =
                Math.makeBoundsOverflow
                    (field.GetPosition world)
                    (Vector2.Multiply (Constants.Layout.TileSize, Constants.Layout.TileSheetSize))
                    (field.GetOverflow world)

            if World.isBoundsInView viewType bounds world then
                let fieldMap = field.GetFieldMapNp world
                let image = fieldMap.FieldTileSheet
                let sprites =
                    Map.foldBack
                        (fun tilePositionM tile sprites ->
                            let tilePosition = vmtovf tilePositionM // NOTE: field position assumed at origin
                            let tileInsetOpt = getTileInsetOpt tile.TileSheetPositionM
                            let sprite =
                                { Position = tilePosition
                                  Size = Constants.Layout.TileSize
                                  Rotation = 0.0f // NOTE: rotation assumed zero
                                  Offset = Vector2.Zero
                                  ViewType = Relative // NOTE: ViewType assumed relative
                                  InsetOpt = tileInsetOpt
                                  Image = image
                                  Color = Vector4.One }
                            sprite :: sprites)
                        fieldMap.FieldTiles [] |>
                    Array.ofList

                World.enqueueRenderMessage
                    (RenderDescriptorsMessage
                        [|LayerableDescriptor
                            { Depth = field.GetDepth world
                              AssetTag = image
                              PositionY = (field.GetPosition world).Y
                              LayeredDescriptor = SpritesDescriptor sprites }|])

                    world
            else world

        override dispatcher.GetQuickSize (field, world) =
            vmtovf (field.GetFieldMapNp world).FieldSizeM