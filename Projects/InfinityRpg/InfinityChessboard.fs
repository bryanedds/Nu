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
    { FieldSpaces : Map<Vector2i, unit>
      CharacterSpaces : Map<Vector2i, Character>
      PropSpaces : Map<Vector2i, PropType>
      PickupSpaces : Map<Vector2i, PickupType> }

    member this.EnemySpaces =
        Map.filter (fun _ (v : Character) -> v.CharacterIndex.IsEnemy) this.CharacterSpaces

    member this.OccupiedSpaces =
        ((Map.map (fun _ _ -> ()) this.CharacterSpaces) @@ (Map.map (fun _ _ -> ()) this.PropSpaces)) |> Map.toKeyList
    
    member this.UnoccupiedSpaces =
        Map.removeMany this.OccupiedSpaces this.FieldSpaces |> Map.toKeyList

    static member openDirections coordinates (chessboard : Chessboard) =
        List.filter (fun d -> List.exists (fun x -> x = (coordinates + (dtovc d))) chessboard.UnoccupiedSpaces) [Upward; Rightward; Downward; Leftward]

    member this.EnemyIndices =
        Map.toListBy (fun _ v -> v.CharacterIndex) this.EnemySpaces
    
    static member getCharacterCoordinates index chessboard =
        Map.findKey (fun _ v -> v.CharacterIndex = index) chessboard.CharacterSpaces

    static member getCharacterAtCoordinates coordinates chessboard =
        chessboard.CharacterSpaces.[coordinates]
    
    static member getCharacter index chessboard =
        let coordinates = Chessboard.getCharacterCoordinates index chessboard
        Chessboard.getCharacterAtCoordinates coordinates chessboard
    
    static member tryGetOccupantAtCoordinates coordinates chessboard =
        match Map.tryFind coordinates chessboard.CharacterSpaces with
        | Some character -> Some (OccupyingCharacter character)
        | None ->
            match Map.tryFind coordinates chessboard.PropSpaces with
            | Some prop -> Some (OccupyingProp prop)
            | None -> None
    
    static member getPickup coordinates chessboard =
        chessboard.PickupSpaces.[coordinates]
    
    // ignores non-adjacent enemies
    static member getNavigationMap currentCoordinates chessboard =
        let predicate k _ = not ((Map.exists (fun k' _ -> k' = k) chessboard.CharacterSpaces) && (k = currentCoordinates + (dtovc Upward) || k = currentCoordinates + (dtovc Rightward) || k = currentCoordinates + (dtovc Downward) || k = currentCoordinates + (dtovc Leftward)))
        chessboard.FieldSpaces |>
        Map.removeMany (Map.toKeyList chessboard.PropSpaces) |>
        Map.filter predicate |>
        Map.map absurdity2

    static member spaceExists coordinates chessboard =
        Map.exists (fun k _ -> k = coordinates) chessboard.FieldSpaces
    
    static member characterExists index chessboard =
        Map.exists (fun _ v -> v.CharacterIndex = index) chessboard.CharacterSpaces
    
    static member enemyAtCoordinates coordinates chessboard =
        if Map.exists (fun k _ -> k = coordinates) chessboard.CharacterSpaces then
            chessboard.CharacterSpaces.[coordinates].IsEnemy
        else false
    
    static member pickupAtCoordinates coordinates chessboard =
        Map.exists (fun k _ -> k = coordinates) chessboard.PickupSpaces
    
    static member updateFieldSpaces updater chessboard =
        { chessboard with FieldSpaces = updater chessboard.FieldSpaces }
    
    static member updateCharacterSpaces updater chessboard =
        { chessboard with CharacterSpaces = updater chessboard.CharacterSpaces }

    static member updatePropSpaces updater chessboard =
        { chessboard with PropSpaces = updater chessboard.PropSpaces }

    static member updatePickupSpaces updater chessboard =
        { chessboard with PickupSpaces = updater chessboard.PickupSpaces }
    
    static member addCharacter character coordinates (chessboard : Chessboard) =
        Chessboard.updateCharacterSpaces (Map.add coordinates character) chessboard

    static member updateCharacter index updater chessboard =
        let coordinates = Chessboard.getCharacterCoordinates index chessboard
        Chessboard.updateCharacterSpaces (Map.add coordinates (updater chessboard.CharacterSpaces.[coordinates])) chessboard
    
    static member removeCharacter index chessboard =
        let coordinates = Chessboard.getCharacterCoordinates index chessboard
        Chessboard.updateCharacterSpaces (Map.remove coordinates) chessboard
    
    static member relocateCharacter index coordinates (chessboard : Chessboard) =
        let oldCoordinates = Chessboard.getCharacterCoordinates index chessboard
        let character = Chessboard.getCharacterAtCoordinates oldCoordinates chessboard
        let chessboard = Chessboard.updateCharacterSpaces (Map.remove oldCoordinates) chessboard
        Chessboard.addCharacter character coordinates chessboard
    
    static member clearEnemies (chessboard : Chessboard) =
        Chessboard.updateCharacterSpaces (Map.filter (fun _ v -> not v.IsEnemy)) chessboard
    
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
        let fieldSpaces = fieldMap.FieldTiles |> Map.filter (fun _ fieldTile -> fieldTile.TileType = Passable) |> Map.map (fun _ _ -> ())
        Chessboard.updateFieldSpaces (constant fieldSpaces) chessboard

    static member transitionMap fieldMap chessboard =
        let playerCoordinates = Chessboard.getCharacterCoordinates PlayerIndex chessboard
        let player = Chessboard.getCharacterAtCoordinates playerCoordinates chessboard
        let chessboard = Chessboard.setFieldSpaces fieldMap chessboard
        Chessboard.addCharacter player playerCoordinates chessboard

    static member empty () =
        { FieldSpaces = Map.empty; CharacterSpaces = Map.empty; PropSpaces = Map.empty; PickupSpaces = Map.empty }

    static member init fieldMap =
        let chessboard = Chessboard.empty () |> Chessboard.setFieldSpaces fieldMap
        Chessboard.addCharacter (Character.makePlayer ()) v2iZero chessboard