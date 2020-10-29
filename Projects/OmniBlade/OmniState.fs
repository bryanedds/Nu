// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace OmniBlade
open System
open FSharpx.Collections
open Prime
open Nu

type OriginRand =
    | OriginC
    | OriginN
    | OriginE
    | OriginS
    | OriginW
    | OriginNE
    | OriginNW
    | OriginSE
    | OriginSW

type SegmentRand =
    | Segment0 = 0b0000
    | Segment1N = 0b0001
    | Segment1E = 0b0010
    | Segment1S = 0b0100
    | Segment1W = 0b1000
    | Segment2NE = 0b0011
    | Segment2NW = 0b1001
    | Segment2SE = 0b0110
    | Segment2SW = 0b1100
    | Segment2EW = 0b1010
    | Segment2NS = 0b0101
    | Segment3N = 0b1011
    | Segment3E = 0b0111
    | Segment3S = 0b1110
    | Segment3W = 0b1101
    | Segment4 = 0b1111

type [<NoComparison>] SegmentsRand =
    { Segment1N : TiledSharp.TmxMap
      Segment1E : TiledSharp.TmxMap
      Segment1S : TiledSharp.TmxMap
      Segment1W : TiledSharp.TmxMap
      Segment2NE : TiledSharp.TmxMap
      Segment2NW : TiledSharp.TmxMap
      Segment2SE : TiledSharp.TmxMap
      Segment2SW : TiledSharp.TmxMap
      Segment2NS : TiledSharp.TmxMap
      Segment2EW : TiledSharp.TmxMap
      Segment3N : TiledSharp.TmxMap
      Segment3E : TiledSharp.TmxMap
      Segment3S : TiledSharp.TmxMap
      Segment3W : TiledSharp.TmxMap
      Segment4 : TiledSharp.TmxMap }

    static member load (filePath : string) =
      { Segment1N = TiledSharp.TmxMap (filePath + "+1N.tmx")
        Segment1E = TiledSharp.TmxMap (filePath + "+1E.tmx")
        Segment1S = TiledSharp.TmxMap (filePath + "+1S.tmx")
        Segment1W = TiledSharp.TmxMap (filePath + "+1W.tmx")
        Segment2NE = TiledSharp.TmxMap (filePath + "+2NE.tmx")
        Segment2NW = TiledSharp.TmxMap (filePath + "+2NW.tmx")
        Segment2SE = TiledSharp.TmxMap (filePath + "+2SE.tmx")
        Segment2SW = TiledSharp.TmxMap (filePath + "+2SW.tmx")
        Segment2NS = TiledSharp.TmxMap (filePath + "+2NS.tmx")
        Segment2EW = TiledSharp.TmxMap (filePath + "+2EW.tmx")
        Segment3N = TiledSharp.TmxMap (filePath + "+3N.tmx")
        Segment3E = TiledSharp.TmxMap (filePath + "+3E.tmx")
        Segment3S = TiledSharp.TmxMap (filePath + "+3S.tmx")
        Segment3W = TiledSharp.TmxMap (filePath + "+3W.tmx")
        Segment4 = TiledSharp.TmxMap (filePath + "+4A.tmx") }

type MapRand =
    { MapSegments : SegmentRand array array
      MapOriginOpt : Vector2i option
      MapSize : Vector2i }

    static member printfn map =
        match map.MapOriginOpt with
        | Some start ->
            printfn "Start: %s" (scstring start)
            for j in map.MapSize.Y - 1 .. -1 .. 0 do
                for i in 0 .. map.MapSize.X - 1 do
                    printf "%s\t" (scstring map.MapSegments.[i].[j])
                printfn ""
        | None -> ()

    static member clone map =
        { MapSegments = Seq.toArray (Array.map Seq.toArray map.MapSegments)
          MapOriginOpt = map.MapOriginOpt
          MapSize = map.MapSize }

    static member concat left right =
        if left.MapOriginOpt = right.MapOriginOpt then
            if left.MapSize = right.MapSize then
                let map = MapRand.clone left
                for i in 0 .. map.MapSize.X - 1 do
                    for j in 0 .. map.MapSize.Y - 1 do
                        map.MapSegments.[i].[j] <- left.MapSegments.[i].[j] ||| right.MapSegments.[i].[j]
                map
            else failwith "Cannot concat two RandMaps of differing sizes."
        else failwith "Cannot concat two RandMaps with different origins."

    static member private walk biasChance bias (cursor : Vector2i) map rand =
        let bounds = v4iBounds v2iZero map.MapSize
        let mutable cursor = cursor
        let (i, rand) = Rand.nextIntUnder 4 rand
        let (chance, rand) = Rand.nextSingleUnder 1.0f rand
        let direction = if chance < biasChance then bias else i
        match direction with
        | 0 ->
            // try go north
            if  cursor.Y < dec bounds.W then
                map.MapSegments.[cursor.X].[cursor.Y] <- map.MapSegments.[cursor.X].[cursor.Y] ||| SegmentRand.Segment1N
                cursor.Y <- inc cursor.Y
                map.MapSegments.[cursor.X].[cursor.Y] <- map.MapSegments.[cursor.X].[cursor.Y] ||| SegmentRand.Segment1S
        | 1 ->
            // try go east
            if  cursor.X < dec bounds.Z then
                map.MapSegments.[cursor.X].[cursor.Y] <- map.MapSegments.[cursor.X].[cursor.Y] ||| SegmentRand.Segment1E
                cursor.X <- inc cursor.X
                map.MapSegments.[cursor.X].[cursor.Y] <- map.MapSegments.[cursor.X].[cursor.Y] ||| SegmentRand.Segment1W
        | 2 ->
            // try go south
            if  cursor.Y > 1 then
                map.MapSegments.[cursor.X].[cursor.Y] <- map.MapSegments.[cursor.X].[cursor.Y] ||| SegmentRand.Segment1S
                cursor.Y <- dec cursor.Y
                map.MapSegments.[cursor.X].[cursor.Y] <- map.MapSegments.[cursor.X].[cursor.Y] ||| SegmentRand.Segment1N
        | 3 ->
            // try go west
            if  cursor.X > 1 then
                map.MapSegments.[cursor.X].[cursor.Y] <- map.MapSegments.[cursor.X].[cursor.Y] ||| SegmentRand.Segment1W
                cursor.X <- dec cursor.X
                map.MapSegments.[cursor.X].[cursor.Y] <- map.MapSegments.[cursor.X].[cursor.Y] ||| SegmentRand.Segment1E
        | _ -> failwithumf ()
        (cursor, rand)

    static member makeFromRand biasChance length (size : Vector2i) origin rand =
        if size.X < 4 || size.Y < 4 then failwith "Invalid MapRand size."
        let bounds = v4iBounds v2iZero size
        let (cursor, biases) =
            match origin with
            | OriginC ->        (bounds.Center,                     [0; 1; 2; 3]) // 0 = n; 1 = e; 2 = s; 3 = w
            | OriginN ->        (bounds.Top - v2iUp,                [2; 2; 1; 3])
            | OriginE ->        (bounds.Right - v2iRight,           [3; 3; 0; 2])
            | OriginS ->        (bounds.Bottom,                     [0; 0; 1; 3])
            | OriginW ->        (bounds.Left,                       [1; 1; 0; 2])
            | OriginNE ->       (v2i (dec bounds.Z) (dec bounds.W), [0; 1; 2; 3])
            | OriginNW ->       (v2i bounds.X (dec bounds.W),       [0; 1; 2; 3])
            | OriginSE ->       (v2i (dec bounds.Z) bounds.Y,       [0; 1; 2; 3])
            | OriginSW ->       (v2i bounds.X bounds.Y,             [0; 1; 2; 3])
        let (maps, rand) =
            List.fold (fun (maps, rand) bias ->
                let map = { MapRand.make size with MapOriginOpt = Some cursor }
                let (_, rand) = List.fold (fun (cursor, rand) _ -> MapRand.walk biasChance bias cursor map rand) (cursor, rand) [0 .. dec length]
                (map :: maps, rand))
                ([], rand)
                biases
        let map = List.reduce MapRand.concat maps
        (map, rand)

    static member make (size : Vector2i) =
        if size.X < 4 || size.Y < 4 then failwith "Invalid MapRand size."
        { MapSegments = Array.init size.X (fun _ -> Array.init size.Y (constant SegmentRand.Segment0))
          MapOriginOpt = None
          MapSize = size }

    static member toTmx abstractPath map =
        let mapTmx = TiledSharp.TmxMap (abstractPath + "+7x7.tmx")
        let segments = SegmentsRand.load abstractPath
        for l in 0 .. 2 - 1 do
            mapTmx.Layers.[l].Tiles.Clear ()
            for i in 0 .. 7 - 1 do
                for ii in 0 .. 32 - 1 do
                    for j in 0 .. 7 - 1 do
                        for jj in 0 .. 32 - 1 do
                            let x = i + ii * 32
                            let y = j + jj * 32
                            let segment = map.MapSegments.[i].[j]
                            let tile =
                                match segment with
                                | SegmentRand.Segment0 -> TiledSharp.TmxLayerTile (0u, x, y)
                                | SegmentRand.Segment1N -> segments.Segment1N.Layers.[l].Tiles.[ii * segments.Segment1N.Width + jj]
                                | SegmentRand.Segment1E -> segments.Segment1E.Layers.[l].Tiles.[ii * segments.Segment1E.Width + jj]
                                | SegmentRand.Segment1S -> segments.Segment1S.Layers.[l].Tiles.[ii * segments.Segment1S.Width + jj]
                                | SegmentRand.Segment1W -> segments.Segment1W.Layers.[l].Tiles.[ii * segments.Segment1W.Width + jj]
                                | SegmentRand.Segment2NE -> segments.Segment2NE.Layers.[l].Tiles.[ii * segments.Segment2NE.Width + jj]
                                | SegmentRand.Segment2NW -> segments.Segment2NW.Layers.[l].Tiles.[ii * segments.Segment2NW.Width + jj]
                                | SegmentRand.Segment2SE -> segments.Segment2SE.Layers.[l].Tiles.[ii * segments.Segment2SE.Width + jj]
                                | SegmentRand.Segment2SW -> segments.Segment2SW.Layers.[l].Tiles.[ii * segments.Segment2SW.Width + jj]
                                | SegmentRand.Segment2EW -> segments.Segment2EW.Layers.[l].Tiles.[ii * segments.Segment2EW.Width + jj]
                                | SegmentRand.Segment2NS -> segments.Segment2NS.Layers.[l].Tiles.[ii * segments.Segment2NS.Width + jj]
                                | SegmentRand.Segment3N -> segments.Segment3N.Layers.[l].Tiles.[ii * segments.Segment3N.Width + jj]
                                | SegmentRand.Segment3E -> segments.Segment3E.Layers.[l].Tiles.[ii * segments.Segment3E.Width + jj]
                                | SegmentRand.Segment3S -> segments.Segment3S.Layers.[l].Tiles.[ii * segments.Segment3S.Width + jj]
                                | SegmentRand.Segment3W -> segments.Segment3W.Layers.[l].Tiles.[ii * segments.Segment3W.Width + jj]
                                | SegmentRand.Segment4 -> segments.Segment4.Layers.[l].Tiles.[ii * segments.Segment4.Width + jj]
                                | _ -> failwithumf ()
                            let tileType = tile.GetType ()
                            let xProp = (tileType.GetProperties "X").[0]
                            let yProp = (tileType.GetProperties "Y").[0]
                            xProp.SetValue (tile, x)
                            yProp.SetValue (tile, y)
                            mapTmx.Layers.[l].Tiles.Add tile

type PropState =
    | DoorState of bool
    | SwitchState of bool
    | NpcState of bool
    | ShopkeepState of bool
    | NilState

type [<ReferenceEquality; NoComparison>] PrizePool =
    { Items : ItemType list
      Gold : int
      Exp : int }

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

    static member addItem item inventory =
        match item with
        | Equipment _ | Consumable _ | KeyItem _ ->
            match Map.tryFind item inventory.Items with
            | Some itemCount -> { inventory with Items = Map.add item (inc itemCount) inventory.Items }
            | None -> { inventory with Items = Map.add item 1 inventory.Items }
        | Stash gold -> { inventory with Gold = inventory.Gold + gold }

    static member addItems items inventory =
        List.fold (flip Inventory.addItem) inventory items

    static member removeItem item inventory =
        match Map.tryFind item inventory.Items with
        | Some itemCount when itemCount > 1 ->
            { inventory with Items = Map.add item (dec itemCount) inventory.Items }
        | Some itemCount when itemCount = 1 ->
            { inventory with Items = Map.remove item inventory.Items }
        | _ -> inventory

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

type [<ReferenceEquality; NoComparison>] Legionnaire =
    { LegionIndex : int // key
      PartyIndexOpt : int option
      CharacterType : CharacterType
      HitPoints : int
      TechPoints : int
      ExpPoints : int
      WeaponOpt : string option
      ArmorOpt : string option
      Accessories : string list }

    static member canUseItem itemType legionnaire =
        match Map.tryFind legionnaire.CharacterType data.Value.Characters with
        | Some characterData ->
            match Map.tryFind characterData.ArchetypeType data.Value.Archetypes with
            | Some archetypeData ->
                match itemType with
                | Consumable _ -> true
                | Equipment equipmentType ->
                    match equipmentType with
                    | WeaponType weaponType ->
                        match Map.tryFind weaponType data.Value.Weapons with
                        | Some weaponData -> weaponData.WeaponSubtype = archetypeData.WeaponSubtype
                        | None -> false
                    | ArmorType armorType ->
                        match Map.tryFind armorType data.Value.Armors with
                        | Some armorData -> armorData.ArmorSubtype = archetypeData.ArmorSubtype
                        | None -> false
                    | AccessoryType _ -> true
                | KeyItem _ -> false
                | Stash _ -> false
            | None -> false
        | None -> false

    static member tryUseItem itemType legionnaire =
        if Legionnaire.canUseItem itemType legionnaire then
            match Map.tryFind legionnaire.CharacterType data.Value.Characters with
            | Some characterData ->
                match itemType with
                | Consumable consumableType ->
                    match consumableType with
                    | GreenHerb ->
                        let level = Algorithms.expPointsToLevel legionnaire.ExpPoints
                        let hpm = Algorithms.hitPointsMax legionnaire.ArmorOpt characterData.ArchetypeType level
                        let legionnaire = { legionnaire with HitPoints = min hpm (legionnaire.HitPoints + 50) } // TODO: pull from data!
                        (true, None, legionnaire)
                    | RedHerb ->
                        let level = Algorithms.expPointsToLevel legionnaire.ExpPoints
                        let hpm = Algorithms.hitPointsMax legionnaire.ArmorOpt characterData.ArchetypeType level
                        let legionnaire = { legionnaire with HitPoints = min hpm (legionnaire.HitPoints + 250) } // TODO: pull from data!
                        (true, None, legionnaire)
                | Equipment equipmentType ->
                    match equipmentType with
                    | WeaponType weaponType -> (true, Option.map (Equipment << WeaponType) legionnaire.WeaponOpt, { legionnaire with WeaponOpt = Some weaponType })
                    | ArmorType armorType -> (true, Option.map (Equipment << ArmorType) legionnaire.ArmorOpt, { legionnaire with ArmorOpt = Some armorType })
                    | AccessoryType accessoryType -> (true, Option.map (Equipment << AccessoryType) (List.tryHead legionnaire.Accessories), { legionnaire with Accessories = [accessoryType] })
                | KeyItem _ -> (false, None, legionnaire)
                | Stash _ -> (false, None, legionnaire)
            | None -> (false, None, legionnaire)
        else (false, None, legionnaire)

    static member finn =
        let index = 0
        let characterType = Ally Finn
        let character = Map.find characterType data.Value.Characters
        let expPoints = Algorithms.levelToExpPoints character.LevelBase
        let archetypeType = character.ArchetypeType
        let weaponOpt = None
        let armorOpt = None
        { LegionIndex = index
          PartyIndexOpt = Some index
          CharacterType = characterType
          HitPoints = Algorithms.hitPointsMax armorOpt archetypeType character.LevelBase
          TechPoints = Algorithms.techPointsMax armorOpt archetypeType character.LevelBase
          ExpPoints = expPoints
          WeaponOpt = weaponOpt
          ArmorOpt = armorOpt
          Accessories = [] }

    static member glenn =
        let index = 1
        let characterType = Ally Glenn
        let character = Map.find characterType data.Value.Characters
        let expPoints = Algorithms.levelToExpPoints character.LevelBase
        let archetypeType = character.ArchetypeType
        let weaponOpt = None
        let armorOpt = None
        { LegionIndex = index
          PartyIndexOpt = Some index
          CharacterType = characterType
          HitPoints = Algorithms.hitPointsMax armorOpt archetypeType character.LevelBase
          TechPoints = Algorithms.techPointsMax armorOpt archetypeType character.LevelBase
          ExpPoints = expPoints
          WeaponOpt = weaponOpt
          ArmorOpt = armorOpt
          Accessories = [] }

type Legion =
    Map<int, Legionnaire>

type CharacterIndex =
    | AllyIndex of int
    | EnemyIndex of int
    static member isTeammate index index2 =
        match (index, index2) with
        | (AllyIndex _, AllyIndex _) -> true
        | (EnemyIndex _, EnemyIndex _) -> true
        | (_, _) -> false

type [<ReferenceEquality; NoComparison>] CharacterState =
    { ArchetypeType : ArchetypeType
      ExpPoints : int
      WeaponOpt : string option
      ArmorOpt : string option
      Accessories : string list
      HitPoints : int
      TechPoints : int
      Statuses : StatusType Set
      Defending : bool
      Charging : bool
      PowerBuff : single
      ShieldBuff : single
      MagicBuff : single
      CounterBuff : single
      GoldPrize : int
      ExpPrize : int }

    member this.Level = Algorithms.expPointsToLevel this.ExpPoints
    member this.IsHealthy = this.HitPoints > 0
    member this.IsWounded = this.HitPoints <= 0
    member this.HitPointsMax = Algorithms.hitPointsMax this.ArmorOpt this.ArchetypeType this.Level
    member this.TechPointsMax = Algorithms.techPointsMax this.ArmorOpt this.ArchetypeType this.Level
    member this.Power = Algorithms.power this.WeaponOpt this.PowerBuff this.ArchetypeType this.Level
    member this.Magic = Algorithms.magic this.WeaponOpt this.MagicBuff this.ArchetypeType this.Level
    member this.Shield effectType = Algorithms.shield effectType this.Accessories this.ShieldBuff this.ArchetypeType this.Level
    member this.Techs = Algorithms.techs this.ArchetypeType this.Level

    static member getAttackResult effectType (source : CharacterState) (target : CharacterState) =
        let power = source.Power
        let shield = target.Shield effectType
        let damageUnscaled = power - shield
        let damage = single damageUnscaled |> int |> max 1
        damage

    static member updateHitPoints updater (state : CharacterState) =
        let hitPoints = updater state.HitPoints
        let hitPoints = max 0 hitPoints
        let hitPoints = min state.HitPointsMax hitPoints
        { state with HitPoints = hitPoints }

    static member updateTechPoints updater state =
        let techPoints = updater state.TechPoints
        let techPoints = max 0 techPoints
        let techPoints = min state.TechPointsMax techPoints
        { state with TechPoints = techPoints }

    static member updateExpPoints updater state =
        let expPoints = updater state.ExpPoints
        let expPoints = max 0 expPoints
        { state with ExpPoints = expPoints }

    static member tryGetTechRandom (state : CharacterState) =
        let techs = state.Techs
        if Set.notEmpty techs then
            let techIndex = Gen.random1 techs.Count
            let tech = Seq.item techIndex techs
            Some tech
        else None

    static member getPoiseType state =
        if state.Defending then Defending
        elif state.Charging then Charging
        else Poising

    static member make (characterData : CharacterData) hitPoints techPoints expPoints weaponOpt armorOpt accessories =
        let archetypeType = characterData.ArchetypeType
        let level = Algorithms.expPointsToLevel expPoints
        let characterState =
            { ArchetypeType = archetypeType
              ExpPoints = expPoints
              WeaponOpt = weaponOpt
              ArmorOpt = armorOpt
              Accessories = accessories
              HitPoints = hitPoints
              TechPoints = techPoints
              Statuses = Set.empty
              Defending = false
              Charging = false
              PowerBuff = 1.0f
              MagicBuff = 1.0f
              ShieldBuff = 1.0f
              CounterBuff = 1.0f
              GoldPrize = Algorithms.goldPrize characterData.GoldScalar level
              ExpPrize = Algorithms.expPrize characterData.ExpScalar level }
        characterState

    static member empty =
        let characterState =
            { ArchetypeType = Squire
              ExpPoints = 0
              WeaponOpt = None
              ArmorOpt = None
              Accessories = []
              HitPoints = 1
              TechPoints = 0
              Statuses = Set.empty
              Defending = false
              Charging = false
              PowerBuff = 1.0f
              MagicBuff = 1.0f
              ShieldBuff = 1.0f
              CounterBuff = 1.0f
              GoldPrize = 0
              ExpPrize = 0 }
        characterState

type [<ReferenceEquality; NoComparison>] CharacterAnimationState =
    { TimeStart : int64
      AnimationSheet : Image AssetTag
      AnimationCycle : CharacterAnimationCycle
      Direction : Direction }

    static member setCycle timeOpt cycle state =
        if state.AnimationCycle <> cycle then
            match timeOpt with
            | Some time -> { state with TimeStart = time; AnimationCycle = cycle }
            | None -> { state with AnimationCycle = cycle }
        else state

    static member directionToInt direction =
        match direction with
        | Downward -> 0
        | Leftward -> 1
        | Upward -> 2
        | Rightward -> 3

    static member timeLocal time state =
        time - state.TimeStart

    static member indexCel stutter time state =
        let timeLocal = CharacterAnimationState.timeLocal time state
        int (timeLocal / stutter)

    static member indexLooped run stutter time state =
        CharacterAnimationState.indexCel stutter time state % run

    static member indexSaturated run stutter time state =
        let cel = CharacterAnimationState.indexCel stutter time state
        if cel < dec run then cel else dec run

    static member indexLoopedWithDirection run stutter offset time state =
        let position = CharacterAnimationState.directionToInt state.Direction * run
        let position = Vector2i (CharacterAnimationState.indexLooped run stutter time state + position, 0)
        let position = position + offset
        position

    static member indexLoopedWithoutDirection run stutter offset time state =
        let position = CharacterAnimationState.indexLooped run stutter time state
        let position = v2i position 0 + offset
        position

    static member indexSaturatedWithDirection run stutter offset time state =
        let position = CharacterAnimationState.directionToInt state.Direction * run
        let position = Vector2i (CharacterAnimationState.indexSaturated run stutter time state + position, 0)
        let position = position + offset
        position

    static member indexSaturatedWithoutDirection run stutter offset time state =
        let position = CharacterAnimationState.indexSaturated run stutter time state
        let position = Vector2i (position, 0)
        let position = position + offset
        position

    static member index time state =
        match Map.tryFind state.AnimationCycle data.Value.CharacterAnimations with
        | Some animationData ->
            match animationData.AnimationType with
            | LoopedWithDirection -> CharacterAnimationState.indexLoopedWithDirection animationData.Run animationData.Stutter animationData.Offset time state
            | LoopedWithoutDirection -> CharacterAnimationState.indexLoopedWithoutDirection animationData.Run animationData.Stutter animationData.Offset time state
            | SaturatedWithDirection -> CharacterAnimationState.indexSaturatedWithDirection animationData.Run animationData.Stutter animationData.Offset time state
            | SaturatedWithoutDirection -> CharacterAnimationState.indexSaturatedWithoutDirection animationData.Run animationData.Stutter animationData.Offset time state
        | None -> v2iZero

    static member progressOpt time state =
        match Map.tryFind state.AnimationCycle data.Value.CharacterAnimations with
        | Some animationData ->
            let timeLocal = CharacterAnimationState.timeLocal time state
            match animationData.LengthOpt with
            | Some length -> Some (min 1.0f (single timeLocal / single length))
            | None -> None
        | None -> None

    static member getFinished time state =
        match CharacterAnimationState.progressOpt time state with
        | Some progress -> progress = 1.0f
        | None -> false

    static member empty =
        { TimeStart = 0L
          AnimationSheet = Assets.FinnAnimationSheet
          AnimationCycle = IdleCycle
          Direction = Downward }

type CharacterInputState =
    | NoInput
    | RegularMenu
    | TechMenu
    | ItemMenu
    | AimReticles of string * AimType

    member this.AimType =
        match this with
        | NoInput | RegularMenu | TechMenu | ItemMenu -> NoAim
        | AimReticles (_, aimType) -> aimType