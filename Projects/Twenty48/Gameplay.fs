namespace Twenty48
open System
open System.Collections.Generic
open Prime
open Nu

type Direction =
    | Upward
    | Rightward
    | Downward
    | Leftward

type Tile =
    { TileId : Guid
      Position : Vector2i
      Value : int }

    static member make position value =
        { TileId = Gen.id
          Position = position
          Value = value }

type GameplayState =
    | Commencing
    | Commence of bool
    | Quitting
    | Quit

type Gameplay =
    { GameplayTime : int64
      GameplayState : GameplayState
      BoardSize : Vector2i
      Tiles : Tile list
      Score : int }

    member this.TilesOrdered =
        List.sortBy (fun t -> t.TileId) this.Tiles

    member this.Columns =
        let columns = List.init this.BoardSize.X (fun _ -> List ())
        for tile in this.Tiles do columns.[tile.Position.X].Add tile
        columns |>
        List.map List.ofSeq |>
        List.map (List.sortBy (fun tile -> -tile.Position.Y))

    member this.Rows =
        let rows = List.init this.BoardSize.Y (fun _ -> List ())
        for tile in this.Tiles do rows.[tile.Position.Y].Add tile
        rows |>
        List.map List.ofSeq |>
        List.map (List.sortBy (fun tile -> tile.Position.X))

    member this.Positions =
        Set.ofList
            [for x in 0 .. dec this.BoardSize.X do
                for y in 0 .. dec this.BoardSize.Y do
                    v2i x y]

    member this.PositionsOccupied =
        Set.ofListBy (fun tile -> tile.Position) this.Tiles

    member this.PositionsUnoccupied =
        let positionsOccupied = this.PositionsOccupied
        [for position in this.Positions do
            if not (positionsOccupied.Contains position) then
                position]

    static member private compact line score =
        let rec step line (compacted, score) =
            match line with
            | [] -> (compacted, score)
            | [head] -> (compacted @ [head], score)
            | head :: neck :: tail ->
                if head.Value = neck.Value then
                    let value = head.Value + neck.Value
                    let head = { head with Value = value }
                    let score = score + value
                    step tail (compacted @ [head], score)
                else step (neck :: tail) (compacted @ [head], score)
        step line ([], score)

    static member shiftLeft gameplay =
        let (rows, score) =
            List.foldMap (fun row score ->
                let (row, score) = Gameplay.compact row score
                let row = List.mapi (fun i (tile : Tile) -> { tile with Position = v2i i tile.Position.Y }) row
                (row, score))
                gameplay.Score
                gameplay.Rows
        { gameplay with
            Tiles = List.concat rows
            Score = score }

    static member shiftRight gameplay =
        let (rows, score) =
            List.foldMap (fun row score ->
                let (row, score) = Gameplay.compact (List.rev row) score
                let row = List.mapi (fun i (tile : Tile) -> { tile with Position = v2i (gameplay.BoardSize.X - i - 1) tile.Position.Y }) row
                (row, score))
                gameplay.Score
                gameplay.Rows
        { gameplay with
            Tiles = List.concat rows
            Score = score }

    static member shiftUp gameplay =
        let (rows, score) =
            List.foldMap (fun row score ->
                let (row, score) = Gameplay.compact row score
                let row = List.mapi (fun i (tile : Tile) -> { tile with Position = v2i tile.Position.X (gameplay.BoardSize.Y - i - 1) }) row
                (row, score))
                gameplay.Score
                gameplay.Columns
        { gameplay with
            Tiles = List.concat rows
            Score = score }

    static member shiftDown gameplay =
        let (rows, score) =
            List.foldMap (fun row score ->
                let (row, score) = Gameplay.compact (List.rev row) score
                let row = List.mapi (fun i (tile : Tile) -> { tile with Position = v2i tile.Position.X i }) row
                (row, score))
                gameplay.Score
                gameplay.Columns
        { gameplay with
            Tiles = List.concat rows
            Score = score }

    static member addTile (gameplay : Gameplay) =
        let position = Gen.randomItem gameplay.PositionsUnoccupied
        { gameplay with Tiles = Tile.make position (if Gen.random1 10 = 0 then 4 else 2) :: gameplay.Tiles }

    static member detectTileChange (gameplay : Gameplay) (gameplay2 : Gameplay) =
         gameplay.TilesOrdered <> gameplay2.TilesOrdered

    static member detectMoveAvailability gameplay =
        let movesPossible = [Gameplay.shiftUp; Gameplay.shiftRight; Gameplay.shiftDown; Gameplay.shiftLeft]
        let movesAvailable = List.filter (fun shift -> Gameplay.detectTileChange gameplay (shift gameplay)) movesPossible
        List.notEmpty movesAvailable

    static member empty =
        { GameplayTime = 0L
          GameplayState = Quit
          BoardSize = v2iDup 4
          Tiles = []
          Score = 0 }

    static member commencing =
        let gameplay = Gameplay.empty
        let position = v2i (Gen.random1 gameplay.BoardSize.X) (Gen.random1 gameplay.BoardSize.Y)
        let value = if Gen.random1 10 = 0 then 4 else 2
        let tile = Tile.make position value
        { gameplay with GameplayState = Commencing; Tiles = [tile] }

    static member commence =
        let gameplay = Gameplay.commencing
        { gameplay with GameplayState = Commence false }