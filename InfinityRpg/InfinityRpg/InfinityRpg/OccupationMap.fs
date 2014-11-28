namespace InfinityRpg
open System
open OpenTK
open Nu
open InfinityRpg.Constants

module OccupationMap =

    let makeFromFieldTiles fieldTiles =
        Map.fold
            (fun occupationMap fieldTilePositionM fieldTile ->
                match fieldTile.TileType with
                | Impassable -> Map.add fieldTilePositionM true occupationMap
                | Passable -> Map.add fieldTilePositionM false occupationMap)
            Map.empty
            fieldTiles

    let occupyByTurn characterTurn occupationMap =
        match characterTurn with
        | NavigationTurn navigationDescriptor ->
            let characterOriginM = navigationDescriptor.WalkDescriptor.WalkOriginM
            let characterNextPositionM = characterOriginM + Direction.toVector2i navigationDescriptor.WalkDescriptor.WalkDirection
            Map.add characterNextPositionM true occupationMap
        | CancelTurn -> occupationMap
        | NoTurn -> occupationMap
            
    let occupyByCharacter (character : Entity) occupationMap =
        let characterPositionM = Vector2i (Vector2.Divide (character.Position, TileSize))
        Map.add characterPositionM true occupationMap
            
    let occupyByAdjacentCharacter positionM (character : Entity) occupationMap =
        let characterPositionM = Vector2i (Vector2.Divide (character.Position, TileSize))
        if  characterPositionM = positionM + Vector2i.Up ||
            characterPositionM = positionM + Vector2i.Right ||
            characterPositionM = positionM + Vector2i.Down ||
            characterPositionM = positionM + Vector2i.Left then
            Map.add characterPositionM true occupationMap
        else occupationMap
            
    let unoccupyByCharacter (character : Entity) occupationMap =
        let characterPositionM = Vector2i (Vector2.Divide (character.Position, TileSize))
        Map.add characterPositionM false occupationMap

    let transferByTurn characterTurn character occupationMap =
        match characterTurn with
        | NavigationTurn navigationDescriptor ->
            let occupationMap = unoccupyByCharacter character occupationMap
            occupyByTurn characterTurn occupationMap
        | CancelTurn -> occupationMap
        | NoTurn -> occupationMap