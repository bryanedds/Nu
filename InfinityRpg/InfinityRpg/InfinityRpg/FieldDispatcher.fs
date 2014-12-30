namespace InfinityRpg
open System
open OpenTK
open Prime
open Nu
open Nu.Constants
open Nu.WorldConstants
open InfinityRpg
open InfinityRpg.Constants

[<AutoOpen>]
module FieldDispatcherModule =

    type Entity with
    
        member entity.FieldMapNp = entity?FieldMapNp : FieldMap
        static member getFieldMapNp (entity : Entity) = entity.FieldMapNp
        static member setFieldMapNp (value : FieldMap) (entity : Entity) = entity?FieldMapNp <- value

    type FieldDispatcher () =
        inherit EntityDispatcher ()

        static let DefaultRand = Rand.makeDefault ()
        static let DefaultSizeM = Vector2i (4, 4)
        static let DefaultPathEdgesM = [(Vector2i (1, 1), Vector2i (2, 2))]
        static let DefaultFieldMap = fst <| FieldMap.make FieldTileSheetImage DefaultSizeM DefaultPathEdgesM DefaultRand

        static let getOptTileInset (tileSheetPositionM : Vector2i) =
            let tileOffset = vmtovf tileSheetPositionM
            let tileInset =
                Vector4 (
                    tileOffset.X,
                    tileOffset.Y,
                    tileOffset.X + TileSize.X,
                    tileOffset.Y + TileSize.Y)
            Some tileInset

        static member FieldDefinitions =
            [define? FieldMapNp DefaultFieldMap]

        override dispatcher.GetRenderDescriptors (field, world) =
            if field.Visible then
                let size = Vector2.Multiply (TileSize, TileSheetSize)
                if Camera.inView3 field.ViewType field.Position size world.State.Camera then
                    let sprites =
                        Map.fold
                            (fun sprites tilePositionM tile ->
                                let tileOffset = vmtovf tilePositionM
                                let tilePosition = field.Position + tileOffset
                                let optTileInset = getOptTileInset tile.TileSheetPositionM
                                let sprite =
                                    { Position = tilePosition
                                      Size = TileSize
                                      Rotation = field.Rotation
                                      ViewType = field.ViewType
                                      OptInset = optTileInset
                                      Image = field.FieldMapNp.FieldTileSheet
                                      Color = Vector4.One }
                                sprite :: sprites)
                            []
                            field.FieldMapNp.FieldTiles
                    [LayerableDescriptor { Depth = field.Depth; LayeredDescriptor = SpritesDescriptor sprites }]
                else []
            else []

        override dispatcher.GetQuickSize (field, _) =
            vmtovf field.FieldMapNp.FieldSizeM