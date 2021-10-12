// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace OmniBlade
open System
open FSharpx.Collections
open Prime
open Nu

 type FrenchWordGender =
     |Masculin
     |Feminin
     member this.UndefinedArticle =
            match this with
            |Masculin -> "un "
            |Feminin -> "une "
 type NumberAndGender = 
        |Singular of FrenchWordGender
        |Plural of FrenchWordGender 
        member this.UndefinedArticle =
             match this with
             |Singular gender -> gender.UndefinedArticle
             |Plural _ -> "des"

type Advent =
    | DebugSwitch
    | DebugSwitch2
    | Opened of Guid
    | ShadeRecruited
    | MaelRecruited
    | RiainRecruited
    | PericRecruited
    | MadTrixterDefeated
    | HeavyArmorosDefeated
    | AraneaImplicitumDefeated
    | CastleSealed
    | ForestSealed
    | FactorySealed
    | MountainSealed
    | DeadSeaSealed
    | RuinsSealed
    | DesertSealed
    | DarkCastleSealed
    | SeasonsSealed
    | VolcanoSealed
    | KylaAdvent of int

[<RequireQualifiedAccess>]
module Advents =

    let empty =
        Set.empty<Advent>

    let initial =
        Set.ofList
            [CastleSealed
             ForestSealed
             FactorySealed
             MountainSealed
             DeadSeaSealed
             RuinsSealed
             DesertSealed
             DarkCastleSealed
             SeasonsSealed
             VolcanoSealed
             KylaAdvent 0]

type WeaponType =
    | Bare
    | ShortSword
    | Dagger
    | OakRod
    | OakBow
    | Paws
    | BronzeSword
    | BronzeKatana
    | BronzeRod
    | LightBow
    | Claws
    | IronSword
    | IronKatana
    | SightedBow
    | IvoryRod
    | Fangs
    static member frenchName wt = match wt with 
                                  |Bare -> "Mains Nues"
                                  |ShortSword -> "Epee Courte"
                                  |Dagger -> "Dague"
                                  |OakRod -> "Baguette de Bois "
                                  |OakBow -> "Arc en Bois"
                                  |Paws -> "Pattes"
                                  |BronzeSword -> "Epee de Bronze"
                                  |BronzeKatana -> "Katana de Bronze"
                                  |BronzeRod -> "Baguette de Bronze"
                                  |LightBow -> "Arc Leger"
                                  |Claws -> "Griffes"
                                  |IronSword -> "Epee de Fer"
                                  |IronKatana -> "Katana de Fer"
                                  |SightedBow -> "Arc Droit"
    static member NumberAndGender wt = match wt with
                                        |Bare -> Plural Feminin
                                        |ShortSword -> Singular Feminin
                                        |Dagger -> Singular Feminin
                                        |OakRod -> Singular Feminin
                                        |OakBow -> Singular Masculin 
                                        |Paws -> Plural Feminin
                                        |BronzeSword -> Singular Feminin
                                        |BronzeKatana -> Singular Masculin 
                                        |BronzeRod -> Singular Feminin  
                                        |LightBow -> Singular Masculin 
                                        |Claws -> Plural Feminin
                                        |IronSword -> Singular Feminin 
                                        |IronKatana -> Singular Masculin 
                                        |SightedBow -> Singular Masculin 
    static member frenchWithUndefinedArticle wt = 
        let article = (WeaponType.NumberAndGender wt).UndefinedArticle
        article + WeaponType.frenchName wt

type ArmorType =
    | MicroFur
    | TinMail
    | CottonVest
    | CottonRobe
    | ThinFur
    | BronzeMail
    | LeatherVest
    | LeatherRobe
    | ThickFur
    | IronMail
    | RubberVest
    | SilkRobe
    | ToughHide
    | StoneHide
    static member frenchName at = match at with
                                  |MicroFur -> "Fourrure Fine"
                                  |TinMail -> "Cotte de Fer Blanc"
                                  |CottonVest -> "Veste de Coton"
                                  |CottonRobe -> "Robe de Coton"
                                  |ThinFur -> "Fourrure Fine"
                                  |BronzeMail -> "Cotte de Bronze"
                                  |LeatherVest -> "Veste de Cuir"
                                  |LeatherRobe -> "Robe de Cuir"
                                  |ThickFur -> "Fourrure"
                                  |IronMail -> "Cotte de Fer"
                                  |RubberVest -> "Veste de Caoutchouc"
                                  |SilkRobe -> "Robe de Soie"
                                  |ToughHide -> "Peau Rigide"
                                  |StoneHide -> "Armure de Pierre"
    static member frenchNameGender _ = Feminin 
    static member frenchWithUndefinedArticle at = 
        let article =  (ArmorType.frenchNameGender at).UndefinedArticle
        article + ArmorType.frenchName at

type AccessoryType =
    | SilverRing
    | IronBrace
    static member frenchName at = match at with
                                  |SilverRing -> "Bague en Argent"
                                  |IronBrace -> "Corset de Fer"
    static member frenchNameGender at = match at with
                                        |SilverRing -> Feminin
                                        |IronBrace -> Masculin 
    static member frenchWithUndefinedArticle at =
        let article =   match AccessoryType.frenchNameGender at with
                        |Feminin -> "une "
                        |Masculin -> "un "
        article + AccessoryType.frenchName at

type WeaponSubtype =
    | Melee
    | Sword
    | Knife
    | Rod
    | Bow

type ArmorSubtype =
    | Robe
    | Vest
    | Mail
    | Pelt
    static member frenchName ast =
        match ast with
        |Robe -> "Robe"
        |Vest -> "Veste"
        |Mail -> "Cotte de Mailles"
        |Pelt -> "Peau"

type EquipmentType =
    | WeaponType of WeaponType
    | ArmorType of ArmorType
    | AccessoryType of AccessoryType

type ConsumableType =
    | GreenHerb
    | RedHerb
    | GoldHerb
    | Remedy
    | Ether
    | HighEther
    | TurboEther
    | Revive
    
    static member frenchName ct =
        match ct with
        |GreenHerb -> "Herbe Verte"
        |RedHerb -> "Herbe Rouge"
        |GoldHerb -> "Herbe Doree"
        |Remedy -> "Remede"
        |Ether -> "Ether"
        |HighEther -> "Ether Fort"
        |TurboEther -> "Ether Turbo"
        |Revive -> "Remontant"

    static member frenchGender ct = match ct with 
                                    |GreenHerb -> Feminin
                                    |RedHerb -> Feminin
                                    |GoldHerb -> Feminin 
                                    | _ -> Masculin

    static member frenchWithUndefinedArticle ct = 
        let article = (ConsumableType.frenchGender ct).UndefinedArticle 
        article + ConsumableType.frenchName ct

type KeyItemType =
    | BrassKey
    | IronKey
    | CopperKey
    | AluminumKey
    | PewterKey
    | SteelKey

    static member frenchName kt = match kt with
                                  |BrassKey -> "Cle de Laiton"
    static member frenchGender kt = Feminin 
    
    static member frenchWithUndefinedArticle kt = 
        let article = (KeyItemType.frenchGender kt).UndefinedArticle
        article + KeyItemType.frenchName kt

type ItemType =
    | Consumable of ConsumableType
    | Equipment of EquipmentType
    | KeyItem of KeyItemType
    | Stash of int

    static member sortItems items =
        Seq.sortBy
            (function
             | (Consumable _, _) -> 0
             | (Equipment _, _) -> 1
             | (KeyItem _, _) -> 2
             | (Stash _, _) -> 3)
            items

    static member filterSellableItems items =
        Seq.choose
            (function
             | (Equipment _, _) | (Consumable _, _) as item -> Some item
             | (KeyItem _, _) | (Stash _, _) -> None)
            items

    static member getName item = match item with
                                 | Consumable ty -> ConsumableType.frenchName ty
                                 | Equipment ty -> match ty with WeaponType ty -> WeaponType.frenchName ty | ArmorType ty -> ArmorType.frenchName ty | AccessoryType ty -> AccessoryType.frenchName ty
                                 | KeyItem ty -> KeyItemType.frenchName ty
                                 | Stash gold -> string gold + " Ors"

    static member frenchWithQuantity item =
                  match item with
                         | Consumable ty -> ConsumableType.frenchWithUndefinedArticle ty
                         | Equipment ty -> match ty with WeaponType ty -> string ty | ArmorType ty -> ArmorType.frenchWithUndefinedArticle ty | AccessoryType ty -> AccessoryType.frenchName ty
                         | KeyItem ty -> KeyItemType.frenchWithUndefinedArticle ty
                         | Stash gold -> string gold + " Ors"


type [<ReferenceEquality; NoComparison>] PrizePool =
    { Consequents : Advent Set
      Items : ItemType list
      Gold : int
      Exp : int }

    static member empty =
        { Consequents = Set.empty; Items = []; Gold = 0; Exp = 0 }

// TODO: make this abstract due to item limit constraints.
type [<ReferenceEquality; NoComparison>] Inventory =
    { Items : Map<ItemType, int>
      Gold : int }

    static member getKeyItems inv =
        inv.Items |>
        Map.toArray |>
        Array.map (function (KeyItem keyItemType, count) -> Some (keyItemType, count) | _ -> None) |>
        Array.definitize |>
        Map.ofArray
        
    static member getConsumables inv =
        inv.Items |>
        Map.toArray |>
        Array.map (function (Consumable consumableType, count) -> Some (consumableType, count) | _ -> None) |>
        Array.definitize |>
        Map.ofArray

    static member containsItem item inventory =
        match Map.tryFind item inventory.Items with
        | Some itemCount when itemCount > 0 -> true
        | _ -> false

    static member containsItems items inventory =
        List.forall (flip Inventory.containsItem inventory) items

    static member canAddItem item inventory =
        match item with
        | Equipment _ | Consumable _ | KeyItem _ ->
            match Map.tryFind item inventory.Items with
            | Some itemCount -> itemCount < Constants.Gameplay.ItemLimit
            | None -> true
        | Stash _ -> true

    static member tryAddItem item inventory =
        match item with
        | Equipment _ | Consumable _ | KeyItem _ ->
            match Map.tryFind item inventory.Items with
            | Some itemCount ->
                if itemCount < Constants.Gameplay.ItemLimit then
                    let inventory = { inventory with Items = Map.add item (inc itemCount) inventory.Items }
                    (true, inventory)
                else (false, inventory)
            | None -> (true, { inventory with Items = Map.add item 1 inventory.Items })
        | Stash gold -> (true, { inventory with Gold = inventory.Gold + gold })

    static member tryAddItems items inventory =
        List.foldBack (fun item (failures, inventory) ->
            match Inventory.tryAddItem item inventory with
            | (true, inventory) -> (failures, inventory)
            | (false, inventory) -> (Some item :: failures, inventory))
            items ([], inventory)

    static member tryRemoveItem item inventory =
        match Map.tryFind item inventory.Items with
        | Some itemCount when itemCount > 1 ->
            (true, { inventory with Items = Map.add item (dec itemCount) inventory.Items })
        | Some itemCount when itemCount = 1 ->
            (true, { inventory with Items = Map.remove item inventory.Items })
        | _ -> (false, inventory)

    static member indexItems (inventory : Inventory) =
        inventory.Items |>
        Map.toSeq |>
        Seq.map (fun (ty, ct) -> List.init ct (fun _ -> ty)) |>
        Seq.concat |>
        Seq.index

    static member tryIndexItem index inventory =
        let items = Inventory.indexItems inventory
        let tail = Seq.trySkip index items
        Seq.tryHead tail

    static member getItemCount itemType inventory =
        match Map.tryFind itemType inventory.Items with
        | Some count -> count
        | None -> 0

    static member updateGold updater (inventory : Inventory) =
        { inventory with Gold = updater inventory.Gold }

    static member empty =
        { Items = Map.empty; Gold = 0 }

    static member initial =
        { Items = Map.singleton (Consumable GreenHerb) 1; Gold = 0 }