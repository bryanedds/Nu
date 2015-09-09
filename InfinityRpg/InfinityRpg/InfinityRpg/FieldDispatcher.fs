namespace InfinityRpg
open System
open OpenTK
open Prime
open Nu
open InfinityRpg

[<AutoOpen>]
module FieldDispatcherModule =

    type Entity with
    
        member this.GetFieldMapNp world : FieldMap = (this.GetXtension world)?FieldMapNp
        member this.SetFieldMapNp (value : FieldMap) world = this.UpdateXtension (fun xtension -> xtension?FieldMapNp <- value) world

    type FieldDispatcher () =
        inherit EntityDispatcher ()

        static let DefaultRand = Rand.makeDefault ()
        static let DefaultSizeM = Vector2i (4, 4)
        static let DefaultPathEdgesM = [(Vector2i (1, 1), Vector2i (2, 2))]
        static let DefaultFieldMap = fst ^ FieldMap.make Constants.Assets.FieldTileSheetImage DefaultSizeM DefaultPathEdgesM DefaultRand

        static let getOptTileInset (tileSheetPositionM : Vector2i) =
            let tileOffset = vmtovf tileSheetPositionM
            let tileInset =
                Vector4
                    (tileOffset.X,
                     tileOffset.Y,
                     tileOffset.X + Constants.Layout.TileSize.X,
                     tileOffset.Y + Constants.Layout.TileSize.Y)
            Some tileInset

        static member FieldDefinitions =
            [define? Omnipresent true
             define? FieldMapNp DefaultFieldMap]

        override dispatcher.GetRenderDescriptors (field, world) =
            let viewType = field.GetViewType world
            let position = field.GetPosition world
            let size = Vector2.Multiply (Constants.Layout.TileSize, Constants.Layout.TileSheetSize)
            if World.getCameraBy (Camera.inView3 viewType position size) world then
                let fieldMap = field.GetFieldMapNp world
                let sprites =
                    Map.foldBack
                        (fun tilePositionM tile sprites ->
                            let tilePosition = vmtovf tilePositionM // NOTE: field position assumed at origin
                            let optTileInset = getOptTileInset tile.TileSheetPositionM
                            let sprite =
                                { Position = tilePosition
                                  Size = Constants.Layout.TileSize
                                  Rotation = 0.0f // NOTE: rotation assumed zero
                                  ViewType = Relative // NOTE: ViewType assumed relative
                                  OptInset = optTileInset
                                  Image = fieldMap.FieldTileSheet
                                  Color = Vector4.One }
                            sprite :: sprites)
                        fieldMap.FieldTiles
                        []
                [LayerableDescriptor { Depth = field.GetDepth world; LayeredDescriptor = SpritesDescriptor sprites }]
            else []

        override dispatcher.GetQuickSize (field, world) =
            vmtovf (field.GetFieldMapNp world).FieldSizeM