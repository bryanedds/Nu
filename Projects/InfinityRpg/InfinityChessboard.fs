namespace InfinityRpg
open System
open System.Numerics
open Prime
open Nu
open Nu.Declarative
open InfinityRpg

type Occupant =
    | OccupyingCharacter of Character
    | OccupyingProp of PropType

type [<ReferenceEquality; NoComparison>] FieldSpace =
    { CharacterOpt : Character Option
      PropOpt : PropType Option
      PickupOpt : PickupType Option }

    member this.GetCharacter =
        match this.CharacterOpt with Some character -> character | None -> failwithumf ()
    
    member this.GetCharacterIndex =
        this.GetCharacter.CharacterIndex
    
    member this.TryGetOccupant =
        match this.CharacterOpt with
        | Some character -> Some (OccupyingCharacter character)
        | None ->
            match this.PropOpt with
            | Some prop -> Some (OccupyingProp prop)
            | None -> None
    
    member this.GetPickup =
        match this.PickupOpt with Some pickup -> pickup | None -> failwithumf ()
    
    member this.ContainsCharacter =
        match this.CharacterOpt with Some _ -> true | None -> false

    member this.ContainsEnemy =
        match this.CharacterOpt with
        | None -> false // more efficient order
        | Some character -> character.IsEnemy

    static member containsEnemy (fieldSpace : FieldSpace) =
        fieldSpace.ContainsEnemy
    
    static member containsSpecifiedCharacter index fieldSpace =
        match fieldSpace.CharacterOpt with
        | None -> false // more efficient order
        | Some character -> character.CharacterIndex = index

    member this.ContainsProp =
        match this.PropOpt with Some _ -> true | None -> false

    static member containsProp (fieldSpace : FieldSpace) =
        fieldSpace.ContainsProp
    
    member this.ContainsPickup =
        match this.PickupOpt with Some _ -> true | None -> false
    
    static member containsPickup (fieldSpace : FieldSpace) =
        fieldSpace.ContainsPickup

    member this.IsOccupied =
        this.ContainsCharacter || this.ContainsProp

    static member isOccupied (fieldSpace : FieldSpace) =
        fieldSpace.IsOccupied

    // internal updaters
    
    static member updateCharacterOpt updater fieldSpace =
        { fieldSpace with CharacterOpt = updater fieldSpace.CharacterOpt }

    static member updatePropOpt updater fieldSpace =
        { fieldSpace with PropOpt = updater fieldSpace.PropOpt }
    
    static member updatePickupOpt updater fieldSpace =
        { fieldSpace with PickupOpt = updater fieldSpace.PickupOpt }
    
    static member updateCharacterInternal updater characterOpt =
        match characterOpt with
        | Some character -> Some (updater character)
        | None -> failwithumf ()
    
    // interface
    
    static member addCharacter character fieldSpace =
        FieldSpace.updateCharacterOpt (constant (Some character)) fieldSpace
    
    static member updateCharacter updater fieldSpace =
        FieldSpace.updateCharacterOpt (FieldSpace.updateCharacterInternal updater) fieldSpace
    
    static member removeCharacter fieldSpace =
        FieldSpace.updateCharacterOpt (constant None) fieldSpace
    
    static member addProp prop fieldSpace =
        FieldSpace.updatePropOpt (constant (Some prop)) fieldSpace

    static member removeProp fieldSpace =
        FieldSpace.updatePropOpt (constant None) fieldSpace
    
    static member addPickup pickup fieldSpace =
        FieldSpace.updatePickupOpt (constant (Some pickup)) fieldSpace
    
    static member removePickup fieldSpace =
        FieldSpace.updatePickupOpt (constant None) fieldSpace
    
    static member empty =
        { CharacterOpt = None
          PropOpt = None
          PickupOpt = None }

type [<ReferenceEquality; NoComparison>] Chessboard =
    { FieldSpaces : Map<Vector2i, FieldSpace> }

    // NOTE: the following subset data can be optimized on demand by converting them from filters to storage caches, without interfering with the interface.
    
    member this.CharacterSpaces =
        Map.filter (fun _ (v : FieldSpace) -> v.ContainsCharacter) this.FieldSpaces
    
    member this.EnemySpaces =
        Map.filter (fun _ (v : FieldSpace) -> v.ContainsEnemy) this.FieldSpaces

    member this.PropSpaces =
        Map.filter (fun _ (v : FieldSpace) -> v.ContainsProp) this.FieldSpaces
    
    member this.PickupSpaces =
        Map.filter (fun _ (v : FieldSpace) -> v.ContainsPickup) this.FieldSpaces

    member this.OccupiedSpaces =
        Map.filter (fun _ (v : FieldSpace) -> v.IsOccupied) this.FieldSpaces |> Map.toKeyList
    
    member this.UnoccupiedSpaces =
        Map.filter (fun _ (v : FieldSpace) -> not v.IsOccupied) this.FieldSpaces |> Map.toKeyList

    member this.OpenDirections coordinates =
        List.filter (fun d -> List.exists (fun x -> x = (coordinates + (dtovc d))) this.UnoccupiedSpaces) [Upward; Rightward; Downward; Leftward]

    member this.Characters =
        Map.map (fun _ (v : FieldSpace) -> v.GetCharacter) this.CharacterSpaces

    member this.EnemyIndices =
        Map.toListBy (fun _ (v : FieldSpace) -> v.GetCharacterIndex) this.EnemySpaces
    
    member this.Pickups =
        Map.map (fun _ (v : FieldSpace) -> v.GetPickup) this.PickupSpaces
    
    member this.EnemyCount =
        this.EnemySpaces.Count

    member this.PickupCount =
        this.PickupSpaces.Count
    
    static member getCharacterCoordinates index chessboard =
        Map.findKey (fun _ v -> FieldSpace.containsSpecifiedCharacter index v) chessboard.FieldSpaces

    static member getCharacterAtCoordinates coordinates chessboard =
        chessboard.FieldSpaces.[coordinates].GetCharacter
    
    static member getCharacter index chessboard =
        let coordinates = Chessboard.getCharacterCoordinates index chessboard
        Chessboard.getCharacterAtCoordinates coordinates chessboard
    
    static member tryGetOccupantAtCoordinates coordinates chessboard =
        chessboard.FieldSpaces.[coordinates].TryGetOccupant
    
    static member getPickup coordinates chessboard =
        chessboard.FieldSpaces.[coordinates].GetPickup
    
    // ignores non-adjacent enemies
    static member getNavigationMap currentCoordinates chessboard =
        let predicate k (v : FieldSpace) = not (v.ContainsCharacter && (k = currentCoordinates + (dtovc Upward) || k = currentCoordinates + (dtovc Rightward) || k = currentCoordinates + (dtovc Downward) || k = currentCoordinates + (dtovc Leftward)))
        chessboard.FieldSpaces |>
        Map.filter (fun _ (v : FieldSpace) -> not v.ContainsProp) |>
        Map.filter predicate |>
        Map.map absurdity2

    static member spaceExists coordinates chessboard =
        Map.exists (fun k _ -> k = coordinates) chessboard.FieldSpaces
    
    static member characterExists index chessboard =
        Map.exists (fun _ v -> FieldSpace.containsSpecifiedCharacter index v) chessboard.FieldSpaces
    
    static member enemyAtCoordinates coordinates chessboard =
        chessboard.FieldSpaces.[coordinates].ContainsEnemy
    
    static member pickupAtCoordinates coordinates chessboard =
        chessboard.FieldSpaces.[coordinates].ContainsPickup
    
    // internal updaters
    
    static member updateFieldSpaces updater chessboard =
        { chessboard with FieldSpaces = updater chessboard.FieldSpaces }
    
    static member updateByCoordinatesInternal coordinates updater (fieldSpaces : Map<Vector2i, FieldSpace>) =
        Map.add coordinates (updater fieldSpaces.[coordinates]) fieldSpaces

    static member updateByPredicateInternal predicate updater (fieldSpaces : Map<Vector2i, FieldSpace>) =
        Map.map (fun _ v -> if predicate v then updater v else v) fieldSpaces
    
    static member updateByCoordinates coordinates updater chessboard =
        Chessboard.updateFieldSpaces (Chessboard.updateByCoordinatesInternal coordinates updater) chessboard
    
    static member updateByPredicate predicate updater chessboard =
        Chessboard.updateFieldSpaces (Chessboard.updateByPredicateInternal predicate updater) chessboard
    
    // interface
    
    static member addCharacter character coordinates (chessboard : Chessboard) =
        Chessboard.updateByCoordinates coordinates (FieldSpace.addCharacter character) chessboard

    static member updateCharacter index updater chessboard =
        let coordinates = Chessboard.getCharacterCoordinates index chessboard
        Chessboard.updateByCoordinates coordinates (FieldSpace.updateCharacter updater) chessboard
    
    static member removeCharacter index chessboard =
        let coordinates = Chessboard.getCharacterCoordinates index chessboard
        Chessboard.updateByCoordinates coordinates FieldSpace.removeCharacter chessboard
    
    static member relocateCharacter index coordinates (chessboard : Chessboard) =
        let oldCoordinates = Chessboard.getCharacterCoordinates index chessboard
        let character = Chessboard.getCharacterAtCoordinates oldCoordinates chessboard
        let chessboard = Chessboard.updateByCoordinates oldCoordinates FieldSpace.removeCharacter chessboard
        Chessboard.addCharacter character coordinates chessboard
    
    static member clearEnemies (chessboard : Chessboard) =
        Chessboard.updateByPredicate FieldSpace.containsEnemy FieldSpace.removeCharacter chessboard
    
    static member addProp prop coordinates chessboard =
        Chessboard.updateByCoordinates coordinates (FieldSpace.addProp prop) chessboard

    static member removeProp coordinates chessboard =
        Chessboard.updateByCoordinates coordinates FieldSpace.removeProp chessboard

    static member clearProps chessboard =
        Chessboard.updateByPredicate FieldSpace.containsProp FieldSpace.removeProp chessboard
    
    static member addPickup pickup coordinates chessboard =
        Chessboard.updateByCoordinates coordinates (FieldSpace.addPickup pickup) chessboard

    static member addRandomPickup coordinates chessboard =
        let pickup = if Gen.random1 2 = 0 then Health else (Item (Special MagicMissile))
        Chessboard.addPickup pickup coordinates chessboard
    
    static member removePickup coordinates chessboard =
        Chessboard.updateByCoordinates coordinates FieldSpace.removePickup chessboard
    
    static member clearPickups chessboard =
        Chessboard.updateByPredicate FieldSpace.containsPickup FieldSpace.removePickup chessboard
    
    static member setFieldSpaces fieldMap chessboard =
        let fieldSpaces = fieldMap.FieldTiles |> Map.filter (fun _ fieldTile -> fieldTile.TileType = Passable) |> Map.map (fun _ _ -> FieldSpace.empty)
        Chessboard.updateFieldSpaces (constant fieldSpaces) chessboard

    static member transitionMap fieldMap chessboard =
        let playerCoordinates = Chessboard.getCharacterCoordinates PlayerIndex chessboard
        let player = Chessboard.getCharacterAtCoordinates playerCoordinates chessboard
        let chessboard = Chessboard.setFieldSpaces fieldMap chessboard
        Chessboard.addCharacter player playerCoordinates chessboard

    static member init fieldMap =
        let chessboard = Chessboard.setFieldSpaces fieldMap { FieldSpaces = Map.empty }
        Chessboard.addCharacter Character.makePlayer v2iZero chessboard