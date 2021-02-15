namespace InfinityRpg
open System
open System.Numerics
open Prime
open Nu
open Nu.Declarative
open InfinityRpg

/// TODO: turn this into an abstract data type.
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

    static member tryGetCharacterCoordinates index chessboard =
        Map.tryFindKey (fun _ (character : Character) -> character.CharacterIndex = index) chessboard.Characters

    static member tryGetCharacterAtCoordinates coordinates chessboard =
        Map.tryFind coordinates chessboard.Characters

    static member tryGetCharacter index chessboard =
        match Chessboard.tryGetCharacterCoordinates index chessboard with
        | Some coordinates -> Chessboard.tryGetCharacterAtCoordinates coordinates chessboard
        | None -> None
        
    static member tryGetPlayer (chessboard : Chessboard) =
        Chessboard.tryGetCharacter PlayerIndex chessboard
        
    static member getPlayer (chessboard : Chessboard) =
        match Chessboard.tryGetPlayer chessboard with
        | Some player -> player
        | None -> failwithumf ()

    static member tryGetPickup coordinates chessboard =
        Map.tryFind coordinates chessboard.Pickups

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

    static member doesCharacterExist index chessboard =
        Map.exists
            (fun _ (character : Character) -> character.CharacterIndex = index)
            chessboard.Characters

    static member addCharacter character coordinates (chessboard : Chessboard) =
        Chessboard.updateCharacterSpaces (Map.add coordinates character) chessboard

    static member tryUpdateCharacter index updater chessboard =
        match Chessboard.tryGetCharacterCoordinates index chessboard with
        | Some coordinates ->
            let character = Chessboard.tryGetCharacterAtCoordinates coordinates chessboard |> Option.get // we know it's there - we just got it
            Chessboard.updateCharacterSpaces (Map.add coordinates (updater character)) chessboard
        | None -> chessboard

    static member tryRemoveCharacter index chessboard =
        match Chessboard.tryGetCharacterCoordinates index chessboard with
        | Some coordinates -> Chessboard.updateCharacterSpaces (Map.remove coordinates) chessboard
        | None -> chessboard

    static member tryRelocateCharacter index coordinates (chessboard : Chessboard) =
        match Chessboard.tryGetCharacterCoordinates index chessboard with
        | Some oldCoordinates ->
            let character = Chessboard.tryGetCharacterAtCoordinates oldCoordinates chessboard |> Option.get // we know it's there - we just got it
            let chessboard = Chessboard.updateCharacterSpaces (Map.remove oldCoordinates) chessboard
            Chessboard.addCharacter character coordinates chessboard
        | None -> chessboard

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
        match Chessboard.tryGetCharacterCoordinates PlayerIndex chessboard with
        | Some coordinates ->
            let player = Chessboard.tryGetCharacterAtCoordinates coordinates chessboard |> Option.get // we know it's there - we just got it
            let chessboard = Chessboard.setFieldSpaces fieldMap chessboard
            Chessboard.addCharacter player coordinates chessboard
        | None -> chessboard

    static member empty =
        { Spaces = Set.empty
          Characters = Map.empty
          Props = Map.empty
          Pickups = Map.empty }

    static member make fieldMap =
        let chessboard = Chessboard.setFieldSpaces fieldMap Chessboard.empty
        Chessboard.addCharacter (Character.makePlayer ()) v2iZero chessboard