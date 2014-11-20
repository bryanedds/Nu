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
        static member setFieldMapNp (value : FieldMap) (entity : Entity) = entity?FieldMapNp <- value

    type FieldDispatcher () =
        inherit EntityDispatcher ()

        static let DefaultRand = Rand.makeDefault ()
        static let DefaultSizeM = Vector2i (4, 4)
        static let DefaultPathEdgesM = [(Vector2i (1, 1), Vector2i (2, 2))]
        static let DefaultFieldMap = FieldMap.make FieldTileSheetImage DefaultSizeM DefaultPathEdgesM DefaultRand

        static let getOptTileInset (tileSheetPositionM : Vector2i) =
            let tileOffsetM = Vector2i.Multiply (tileSheetPositionM, TileSizeI)
            let tileInset =
                Vector4 (
                    single tileOffsetM.X,
                    single tileOffsetM.Y,
                    single <| tileOffsetM.X + TileSizeI.X,
                    single <| tileOffsetM.Y + TileSizeI.Y)
            Some tileInset

        static member FieldDefinitions =
            [define? FieldMapNp DefaultFieldMap]

        override dispatcher.GetRenderDescriptors (field, world) =
            if field.Visible then
                let fieldMap = field.FieldMapNp
                let fieldTileSheetAssetTag = Image.toAssetTag fieldMap.FieldTileSheet
                match Metadata.tryGetTextureSize fieldTileSheetAssetTag world.State.AssetMetadataMap with
                | Some tileSheetSize ->
                    let size = Vector2.Multiply (TileSize, TileSheetSize)
                    if Camera.inView3 field.ViewType field.Position size world.Camera then
                        let sprites =
                            Map.fold
                                (fun sprites tileCoords tile ->
                                    let tileOffset = Vector2i.Multiply (tileCoords, TileSizeI)
                                    let tilePosition = Vector2i field.Position + tileOffset
                                    let sprite =
                                        { Position = tilePosition.Vector2
                                          Size = TileSize
                                          Rotation = field.Rotation
                                          ViewType = field.ViewType
                                          OptInset = getOptTileInset tile.FieldTileSheetPositionM
                                          Image = fieldMap.FieldTileSheet
                                          Color = Vector4.One }
                                    sprite :: sprites)
                                []
                                fieldMap.FieldTiles
                        [LayerableDescriptor { Depth = field.Depth; LayeredDescriptor = SpritesDescriptor sprites }]
                    else []
                | None -> []
            else []

        override dispatcher.GetQuickSize (field, world) =
            let fieldMap = field.FieldMapNp
            Vector2.Multiply (TileSize, fieldMap.FieldSizeM.Vector2)