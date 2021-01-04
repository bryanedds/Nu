namespace InfinityRpg
open System
open System.Numerics
open Prime
open Nu
open Nu.Declarative
open InfinityRpg

type [<NoComparison>] Occupant =
    | OccupyingCharacter of Character
    | OccupyingProp of PropType

type [<ReferenceEquality; NoComparison>] Chessboard =
    { Spaces : Vector2i Set
      Characters : Map<Vector2i, Character>
      Props : Map<Vector2i, PropType>
      Pickups : Map<Vector2i, PickupType> }
    
    static member updateSpaces updater chessboard =
        { chessboard with Spaces = updater chessboard.Spaces }
    
    static member updateCharacterSpaces updater chessboard =
        { chessboard with Characters = updater chessboard.Characters }

    static member updatePropSpaces updater chessboard =
        { chessboard with Props = updater chessboard.Props }

    static member updatePickupSpaces updater chessboard =
        { chessboard with Pickups = updater chessboard.Pickups }

    static member getOccupiedSpaces chessboard =
        let characterSpaces = chessboard.Characters |> Map.toKeySeq |> Set.ofSeq
        let propSpaces = chessboard.Props |> Map.toKeySeq |> Set.ofSeq
        Set.union characterSpaces propSpaces

    static member getUnoccupiedSpaces chessboard =
        let occupiedSpaces = Chessboard.getOccupiedSpaces chessboard
        Set.difference chessboard.Spaces occupiedSpaces

    static member getOpenDirections coordinates chessboard =
        let unoccupiedSpaces = Chessboard.getUnoccupiedSpaces chessboard
        [Upward; Rightward; Downward; Leftward] |>
        Set.ofList |>
        Set.filter (fun direction -> Set.exists (fun coordinates2 -> coordinates2 = coordinates + dtovc direction) unoccupiedSpaces) 

    static member getEnemySpaces chessboard =
        Map.filter
            (fun _ (character : Character) -> character.CharacterIndex.IsEnemy)
            chessboard.Characters

    static member getEnemies chessboard =
        let enemySpaces = Chessboard.getEnemySpaces chessboard
        Map.toValueList enemySpaces

    static member getEnemyIndices chessboard =
        let enemies = Chessboard.getEnemies chessboard
        List.map (fun (enemy : Character) -> enemy.CharacterIndex) enemies

    static member getCharacterCoordinates index chessboard =
        Map.findKey (fun _ (character : Character) -> character.CharacterIndex = index) chessboard.Characters

    static member getCharacterAtCoordinates coordinates chessboard =
        chessboard.Characters.[coordinates]

    static member getCharacter index chessboard =
        let coordinates = Chessboard.getCharacterCoordinates index chessboard
        Chessboard.getCharacterAtCoordinates coordinates chessboard

    static member getPickup coordinates chessboard =
        chessboard.Pickups.[coordinates]

    static member getNavigationMap currentCoordinates chessboard =
        let navigableSpaces coordinates =
            not
                (Map.containsKey coordinates chessboard.Characters &&
                 (coordinates = currentCoordinates + dtovc Upward ||
                  coordinates = currentCoordinates + dtovc Rightward ||
                  coordinates = currentCoordinates + dtovc Downward ||
                  coordinates = currentCoordinates + dtovc Leftward))
        chessboard.Spaces |>
        Set.removeMany (Map.toKeyList chessboard.Props) |>
        Set.filter navigableSpaces |>
        Map.ofSeqBy (fun space -> (space, false))

    static member tryGetOccupantAtCoordinates coordinates chessboard =
        match Map.tryFind coordinates chessboard.Characters with
        | Some character -> Some (OccupyingCharacter character)
        | None ->
            match Map.tryFind coordinates chessboard.Props with
            | Some prop -> Some (OccupyingProp prop)
            | None -> None

    static member doesSpaceExist coordinates chessboard =
        Set.contains coordinates chessboard.Spaces

    static member doesCharacterExist index chessboard =
        Map.exists
            (fun _ (character : Character) -> character.CharacterIndex = index)
            chessboard.Characters

    static member isEnemyAtCoordinates coordinates chessboard =
        match Map.tryFind coordinates chessboard.Characters with
        | Some characterSpace -> characterSpace.IsEnemy
        | None -> false

    static member isPickupAtCoordinates coordinates chessboard =
        Map.containsKey coordinates chessboard.Pickups

    static member addCharacter character coordinates (chessboard : Chessboard) =
        Chessboard.updateCharacterSpaces (Map.add coordinates character) chessboard

    static member updateCharacter index updater chessboard =
        let coordinates = Chessboard.getCharacterCoordinates index chessboard
        Chessboard.updateCharacterSpaces (Map.add coordinates (updater chessboard.Characters.[coordinates])) chessboard

    static member removeCharacter index chessboard =
        let coordinates = Chessboard.getCharacterCoordinates index chessboard
        Chessboard.updateCharacterSpaces (Map.remove coordinates) chessboard

    static member relocateCharacter index coordinates (chessboard : Chessboard) =
        let oldCoordinates = Chessboard.getCharacterCoordinates index chessboard
        let character = Chessboard.getCharacterAtCoordinates oldCoordinates chessboard
        let chessboard = Chessboard.updateCharacterSpaces (Map.remove oldCoordinates) chessboard
        Chessboard.addCharacter character coordinates chessboard

    static member clearEnemies (chessboard : Chessboard) =
        Chessboard.updateCharacterSpaces (Map.filter (fun _ character -> character.IsAlly)) chessboard

    static member addProp prop coordinates chessboard =
        Chessboard.updatePropSpaces (Map.add coordinates prop) chessboard

    static member removeProp coordinates chessboard =
        Chessboard.updatePropSpaces (Map.remove coordinates) chessboard

    static member clearProps chessboard =
        Chessboard.updatePropSpaces (constant Map.empty) chessboard

    static member addPickup pickup coordinates chessboard =
        Chessboard.updatePickupSpaces (Map.add coordinates pickup) chessboard

    static member removePickup coordinates chessboard =
        Chessboard.updatePickupSpaces (Map.remove coordinates) chessboard

    static member clearPickups chessboard =
        Chessboard.updatePickupSpaces (constant Map.empty) chessboard

    static member setFieldSpaces fieldMap chessboard =
        let spaces =
            fieldMap.FieldTiles |>
            Map.filter (fun _ fieldTile -> fieldTile.TileType = Passable) |>
            Map.toKeySeq |>
            Set.ofSeq
        Chessboard.updateSpaces (constant spaces) chessboard

    static member transitionMap fieldMap chessboard =
        let playerCoordinates = Chessboard.getCharacterCoordinates PlayerIndex chessboard
        let player = Chessboard.getCharacterAtCoordinates playerCoordinates chessboard
        let chessboard = Chessboard.setFieldSpaces fieldMap chessboard
        Chessboard.addCharacter player playerCoordinates chessboard

    static member empty =
        { Spaces = Set.empty; Characters = Map.empty; Props = Map.empty; Pickups = Map.empty }

    static member make fieldMap =
        let chessboard = Chessboard.setFieldSpaces fieldMap Chessboard.empty
        Chessboard.addCharacter (Character.makePlayer ()) v2iZero chessboard