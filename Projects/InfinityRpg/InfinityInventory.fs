namespace InfinityRpg
open Prime
open Nu

type [<ReferenceEquality; NoComparison>] Inventory =
    { Items : Map<ItemType, int> }

    static member containsItem item inventory =
        match Map.tryFind item inventory.Items with
        | Some itemCount when itemCount > 0 -> true
        | _ -> false
    
    static member tryAddItem item inventory =
        match Map.tryFind item inventory.Items with
        | Some itemCount ->
            if itemCount < Constants.Gameplay.ItemLimit
            then { inventory with Items = Map.add item (inc itemCount) inventory.Items }
            else inventory
        | None -> { inventory with Items = Map.add item 1 inventory.Items }
    
    static member removeItem item inventory =
        match Map.tryFind item inventory.Items with
        | Some itemCount when itemCount > 1 ->
            { inventory with Items = Map.add item (dec itemCount) inventory.Items }
        | Some itemCount when itemCount = 1 ->
            { inventory with Items = Map.remove item inventory.Items }
        | _ -> inventory
    
    static member initial =
        { Items = Map.singleton (Special MagicMissile) 3 }