namespace InfinityRpg
open System
open OpenTK
open Prime
open Nu
open InfinityRpg.Constants

module OccupationMap =

    let occupyByTurn characterTurn occupationMap =
        match characterTurn with
        | ActionTurn _ -> occupationMap
        | NavigationTurn navigationDescriptor ->
            let characterOriginM = navigationDescriptor.WalkDescriptor.WalkOriginM
            let characterNextPositionM = characterOriginM + Direction.toVector2i navigationDescriptor.WalkDescriptor.WalkDirection
            Map.add characterNextPositionM true occupationMap
        | CancelTurn -> occupationMap
        | NoTurn -> occupationMap
            
    let occupyByCharacter (character : Entity) occupationMap =
        let characterPositionM = Vector2i (Vector2.Divide (character.Position, TileSize))
        Map.add characterPositionM true occupationMap

    let occupyByCharacters characters occupationMap =
        List.fold (flip occupyByCharacter) occupationMap characters

    let occupyByAdjacentCharacter positionM (character : Entity) occupationMap =
        let characterPositionM = Vector2i (Vector2.Divide (character.Position, TileSize))
        if  characterPositionM = positionM + Vector2i.Up ||
            characterPositionM = positionM + Vector2i.Right ||
            characterPositionM = positionM + Vector2i.Down ||
            characterPositionM = positionM + Vector2i.Left then
            Map.add characterPositionM true occupationMap
        else occupationMap

    let occupyByAdjacentCharacters positionM characters occupationMap =
        List.fold (flip <| occupyByAdjacentCharacter positionM) occupationMap characters

    let unoccupyByCharacter (character : Entity) occupationMap =
        let characterPositionM = Vector2i (Vector2.Divide (character.Position, TileSize))
        Map.add characterPositionM false occupationMap

    let unoccupyByCharacters characters occupationMap =
        List.fold (flip unoccupyByCharacter) occupationMap characters

    let transferByTurn characterTurn character occupationMap =
        match characterTurn with
        | ActionTurn _ -> occupationMap
        | NavigationTurn _ -> unoccupyByCharacter character occupationMap |> occupyByTurn characterTurn
        | CancelTurn -> occupationMap
        | NoTurn -> occupationMap

    let makeFromFieldTiles fieldTiles =
        Map.fold
            (fun occupationMap fieldTilePositionM fieldTile ->
                match fieldTile.TileType with
                | Impassable -> Map.add fieldTilePositionM true occupationMap
                | Passable -> Map.add fieldTilePositionM false occupationMap)
            Map.empty
            fieldTiles

    let makeFromFieldTilesAndCharacters fieldTiles characters =
        let occupationMap = makeFromFieldTiles fieldTiles
        occupyByCharacters characters occupationMap

    let makeFromFieldTilesAndCharactersAndTurn fieldTiles characters turn =
        let occupationMap = makeFromFieldTilesAndCharacters fieldTiles characters
        occupyByTurn turn occupationMap

    let makeFromFieldTilesAndAdjacentCharacters positionM fieldTiles characters =
        let occupationMap = makeFromFieldTiles fieldTiles
        occupyByAdjacentCharacters positionM characters occupationMap