// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace OmniBlade
open System
open System.IO
open System.Numerics
open Prime
open Nu
open OmniBlade

type SaveSlot =
    | Slot1
    | Slot2
    | Slot3

type [<SymbolicExpansion>] Options =
    { BattleSpeed : BattleSpeed }

type FieldState =
    | Playing
    | Quitting
    | Quit

type FieldTransition =
    { FieldType : FieldType
      FieldDestination : Vector3
      FieldDirection : Direction
      FieldTransitionTime : int64 }

[<RequireQualifiedAccess>]
module Field =

    type [<ReferenceEquality; SymbolicExpansion>] Field =
        private
            { UpdateTime_ : int64
              FieldType_ : FieldType
              FieldState_ : FieldState
              SaveSlot_ : SaveSlot
              OmniSeedState_ : OmniSeedState
              Avatar_ : Avatar
              AvatarCollidedPropIds_ : int list
              AvatarSeparatedPropIds_ : int list
              AvatarIntersectedPropIds_ : int list
              Team_ : Map<int, Teammate>
              SpiritActivity_ : single
              Spirits_ : Spirit array
              Advents_ : Advent Set
              Props_ : Map<int, Prop>
              Inventory_ : Inventory
              Options_ : Options
              Menu_ : Menu
              Definitions_ : CueSystem.CueDefinitions
              DefinitionsOriginal_ : CueSystem.CueDefinitions
              Cue_ : CueSystem.Cue
              ScreenTransitioning_ : bool
              FieldTransitionOpt_ : FieldTransition option
              ShopOpt_ : Shop option
              DialogOpt_ : Dialog option
              BattleOpt_ : Battle option
              FieldSongTimeOpt_ : int64 option
              ViewBoundsAbsolute_ : Box2 }

        (* Local Properties *)
        member this.UpdateTime = this.UpdateTime_
        member this.FieldType = this.FieldType_
        member this.FieldState = this.FieldState_
        member this.OmniSeedState = this.OmniSeedState_
        member this.Avatar = this.Avatar_
        member this.AvatarCollidedPropIds = this.AvatarCollidedPropIds_
        member this.AvatarSeparatedPropIds = this.AvatarSeparatedPropIds_
        member this.AvatarIntersectedPropIds = this.AvatarIntersectedPropIds_
        member this.Team = this.Team_
        member this.SpiritActivity = this.SpiritActivity_
        member this.Spirits = this.Spirits_
        member this.Advents = this.Advents_
        member this.Props = this.Props_
        member this.Inventory = this.Inventory_
        member this.Options = this.Options_
        member this.Menu = this.Menu_
        member this.Definitions = this.Definitions_
        member this.DefinitionsOriginal = this.DefinitionsOriginal_
        member this.Cue = this.Cue_
        member this.ScreenTransitioning = this.ScreenTransitioning_
        member this.FieldTransitionOpt = this.FieldTransitionOpt_
        member this.ShopOpt = this.ShopOpt_
        member this.DialogOpt = this.DialogOpt_
        member this.BattleOpt = this.BattleOpt_
        member this.FieldSongTimeOpt = this.FieldSongTimeOpt_
        member this.ViewBoundsAbsolute = this.ViewBoundsAbsolute_

    let private makePropState time propDescriptor =
        match propDescriptor.PropData with
        | Sprite (_, image, color, blend, emission, flip, visible) -> SpriteState (image, color, blend, emission, flip, visible)
        | Door _ -> DoorState false
        | Switch _ -> SwitchState false
        | Character (characterType, direction, _, _, _, _) ->
            let animationSheet =
                match Data.Value.Characters.TryGetValue characterType with
                | (true, characterData) -> characterData.AnimationSheet
                | (false, _) -> Assets.Field.JinnAnimationSheet
            let characterAnimationState =
                { StartTime = time
                  AnimationSheet = animationSheet
                  CharacterAnimationType = IdleAnimation
                  MaterializationOpt = None
                  Direction = direction }
            CharacterState (Color.One, characterAnimationState)
        | Portal _ | Chest _ | Sensor _ | Npc _ | NpcBranching _ | Shopkeep _ | Seal _ | Flame _ | SavePoint | ChestSpawn | EmptyProp -> NilState

    let private makeProps fieldType omniSeedState world =
        match Map.tryFind fieldType Data.Value.Fields with
        | Some fieldData ->
            let time = World.getUpdateTime world
            FieldData.getPropDescriptors omniSeedState fieldData |>
            Map.ofListBy (fun propDescriptor ->
                let propState = makePropState time propDescriptor
                let prop = Prop.make propDescriptor.PropPerimeter propDescriptor.PropElevation propDescriptor.PropData propState propDescriptor.PropId
                (propDescriptor.PropId, prop))
        | None -> Map.empty

    let private makeBattleFromTeam inventory prizePool (team : Map<int, Teammate>) battleSpeed battleData world =
        let party = team |> Map.toList |> List.tryTake 3
        let allyPositions =
            if List.length party < 3
            then List.take 2 battleData.BattleAllyPositions
            else List.take 1 battleData.BattleAllyPositions @ List.skip 2 battleData.BattleAllyPositions
        let party =
            List.mapi
                (fun index (teamIndex, teammate) ->
                    match Map.tryFind teammate.CharacterType Data.Value.Characters with
                    | Some characterData ->
                        // TODO: bounds checking
                        let size = Constants.Gameplay.CharacterSize
                        let celSize = Constants.Gameplay.CharacterCelSize
                        let position = if party.Length = 1 then allyPositions.[teamIndex] + Constants.Battle.CharacterOffset else allyPositions.[teamIndex]
                        let bounds = box3 position size
                        let characterIndex = AllyIndex teamIndex
                        let characterType = characterData.CharacterType
                        let boss = characterData.Boss
                        let animationSheet = characterData.AnimationSheet
                        let direction = Direction.ofVector3 -bounds.Bottom
                        let actionTime = 1000.0f - Constants.Battle.AllyActionTimeSpacing * single index
                        let characterState = CharacterState.make characterData teammate.HitPoints teammate.TechPoints teammate.ExpPoints teammate.WeaponOpt teammate.ArmorOpt teammate.Accessories
                        let character = Character.make bounds characterIndex characterType boss animationSheet celSize direction characterState None actionTime
                        character
                    | None -> failwith ("Could not find CharacterData for '" + scstring teammate.CharacterType + "'."))
                party
        let battle = Battle.makeFromParty inventory prizePool party battleSpeed battleData world
        battle
        
    let rec detokenize (field : Field) (text : string) =
        text
            .Replace("$FEE", scstring (getRecruitmentFee field))
            .Replace("$GOLD", scstring field.Inventory.Gold)

    and getRecruitmentFee (field : Field) =
        let advents = Set.ofArray [|ShadeRecruited; MaelRecruited; RiainRecruited; PericRecruited|]
        let recruiteds = Set.intersect advents field.Advents
        let recruited = Set.count recruiteds
        match Array.tryItem recruited Constants.Field.RecruitmentFees with
        | Some recruitmentFee -> recruitmentFee
        | None -> 0

    let getParty field =
        field.Team_ |>
        Map.filter (fun _ teammate -> Option.isSome teammate.PartyIndexOpt) |>
        Map.toSeq |>
        Seq.tryTake 3 |>
        Map.ofSeq

    let getFieldSongOpt field =
        match Data.Value.Fields.TryGetValue field.FieldType_ with
        | (true, fieldData) -> fieldData.FieldSongOpt
        | (false, _) -> None

    let getProp propId field =
        field.Props_.[propId]

    let getChests field =
        field.Props_ |>
        Map.toValueArray |>
        Array.choose (fun prop ->
            match prop.PropData with
            | Chest (_, _, id, _, _, _) -> Some (Chest.make prop.Perimeter (field.Advents.Contains (Opened id)))
            | _ -> None)

    let getNonWarpPortals field =
        field.Props_ |>
        Map.toValueArray |>
        Array.choose (fun prop ->
            match prop.PropData with
            | Portal (portalType, _, _, _, _, _, requirements) when portalType <> WarpPortal -> Some (Portal.make prop.Perimeter (field.Advents.IsSupersetOf requirements))
            | _ -> None)

    let getShowUnopenedChests (field : Field) =
        match Map.tryFind field.FieldType Data.Value.Fields with
        | Some fieldData -> fieldData.ShowUnopenedChests
        | None -> true

    let tryGetFacingInteraction (prop : Prop) (field : Field) =
        match prop.PropData with
        | Sprite _ -> None
        | Portal (_, _, _, _, _, _, _) -> None
        | Door _ -> Some "Open"
        | Chest (_, _, chestId, _, _, _) -> if Set.contains (Opened chestId) field.Advents then None else Some "Open"
        | Switch (_, _, _, _) -> Some "Use"
        | Sensor (_, _, _, _, _) -> None
        | Character (_, _, _, isRising, _, _) ->
            if isRising then
                if prop.Bottom.Y - field.Avatar.Bottom.Y > 40.0f // NOTE: just a bit of hard-coding to ensure player is interacting with the character from the south.
                then Some "Talk"
                else None
            else Some "Talk"
        | Npc _ | NpcBranching _ -> Some "Talk"
        | Shopkeep _ -> Some "Shop"
        | Seal _ -> Some "Touch"
        | Flame _ -> None
        | SavePoint -> None
        | ChestSpawn -> None
        | EmptyProp -> None

    let isFacingProp propId (field : Field) =
        match field.Props_.TryGetValue propId with
        | (true, prop) ->
            let v = prop.Bottom - field.Avatar.Bottom
            let direction = Direction.ofVector3 v
            direction <> field.Avatar.Direction.Opposite
        | (false, _) -> false

    let getFacingProps (field : Field) =
        List.filter
            (fun propId -> isFacingProp propId field)
            field.AvatarIntersectedPropIds

    let tryGetFacingProp (field : Field) =
        match getFacingProps field with
        | head :: _ -> Some (field.Props_.[head])
        | [] -> None

    // NOTE: I really don't like the need to do these inefficient reverse map look-ups as a matter of course. Perhaps
    // there's an elegant alternative that is more performant.
    let tryGetPropIdByData predicate field =
        Map.tryFindKey (fun _ (prop : Prop) ->
            predicate prop.PropData)
            field.Props_

    let isTouchingSavePoint (field : Field) =
        List.exists (fun propId ->
            match field.Props_.TryGetValue propId with
            | (true, prop) -> prop.PropData = SavePoint
            | (false, _) -> false)
            field.AvatarIntersectedPropIds

    let tryGetInteraction (field : Field) =
        match field.DialogOpt with
        | Some dialog ->
            if  Dialog.canAdvance (detokenize field) dialog &&
                not
                    (Dialog.isExhausted (detokenize field) dialog &&
                     Option.isSome dialog.DialogPromptOpt)
            then Some "Next"
            else None
        | None ->
            if isTouchingSavePoint field then
                Some "Save"
            else
                match tryGetFacingProp field with
                | Some prop -> tryGetFacingInteraction prop field
                | None -> None

    let tryGetTouchingPortal (field : Field) =
        field.AvatarIntersectedPropIds |>
        List.choose (fun propId ->
            match field.Props_.TryGetValue propId with
            | (true, prop) ->
                match prop.PropData with
                | Portal (portalType, _, _, fieldType, portalIndex, _, requirements) ->
                    if field.Advents.IsSupersetOf requirements then
                        match Map.tryFind fieldType Data.Value.Fields with
                        | Some fieldData ->
                            match FieldData.tryGetPortal field.OmniSeedState portalIndex fieldData with
                            | Some portal ->
                                match portal.PropData with
                                | Portal (_, _, direction, _, _, extended, _) ->
                                    let destination =
                                        match direction with
                                        | Upward -> portal.PropPerimeter.Top + v3 0.0f 8.0f 0.0f + if extended then v3 0.0f 48.0f 0.0f else v3Zero
                                        | Rightward -> portal.PropPerimeter.Right + v3 32.0f 0.0f 0.0f + if extended then v3 48.0f 0.0f 0.0f else v3Zero
                                        | Downward -> portal.PropPerimeter.Bottom + v3 0.0f -54.0f 0.0f - if extended then v3 0.0f 48.0f 0.0f else v3Zero
                                        | Leftward -> portal.PropPerimeter.Left + v3 -32.0f 0.0f 0.0f - if extended then v3 48.0f 0.0f 0.0f else v3Zero
                                    let isWarp =
                                        match portalType with
                                        | AirPortal | StairsPortal (_, false) -> false
                                        | WarpPortal | StairsPortal (_, true) -> true
                                    Some (fieldType, destination, direction, isWarp)
                                | _ -> None
                            | None -> None
                        | None -> None
                    else None
                | _ -> None
            | _ -> None) |>
        List.tryHead

    let getTouchedSensors (field : Field) =
        List.choose (fun propId ->
            match field.Props_.TryGetValue propId with
            | (true, prop) ->
                match prop.PropData with
                | Sensor (sensorType, _, cue, _, requirements) -> Some (sensorType, cue, requirements)
                | _ -> None
            | (false, _) -> None)
            field.AvatarCollidedPropIds

    let getUntouchedSensors (field : Field) =
        List.choose (fun propId ->
            match field.Props_.TryGetValue propId with
            | (true, prop) ->
                match prop.PropData with
                | Sensor (sensorType, _, cue, _, requirements) -> Some (sensorType, cue, requirements)
                | _ -> None
            | (false, _) -> None)
            field.AvatarSeparatedPropIds

    let hasEncounters (field : Field) =
        match Data.Value.Fields.TryGetValue field.FieldType with
        | (true, fieldData) -> Option.isSome fieldData.EncounterTypeOpt
        | (false, _) -> false

    let updateFieldType updater field world =
        let fieldType = updater field.FieldType_
        let spiritActivity = 0.0f
        let props = makeProps fieldType field.OmniSeedState_ world
        { field with
            FieldType_ = fieldType
            SpiritActivity_ = spiritActivity
            Spirits_ = [||]
            Props_ = props
            FieldSongTimeOpt_ = None }

    let updateFieldState updater field =
        { field with FieldState_ = updater field.FieldState_ }

    let updateAvatar updater field =
        let avatar = field.Avatar_
        let avatar = updater avatar
        if avatar =/= field.Avatar_ then { field with Avatar_ = avatar }
        else field

    let updateAvatarCollidedPropIds updater field =
        let propIds = updater field.AvatarCollidedPropIds_
        if propIds =/= field.AvatarCollidedPropIds_
        then { field with AvatarCollidedPropIds_ = propIds }
        else field

    let updateAvatarSeparatedPropIds updater field =
        let propIds = updater field.AvatarSeparatedPropIds_
        if propIds =/= field.AvatarSeparatedPropIds_
        then { field with AvatarSeparatedPropIds_ = propIds }
        else field

    let updateAvatarIntersectedPropIds updater field =
        let propIds = updater field.AvatarIntersectedPropIds_
        if propIds =/= field.AvatarIntersectedPropIds_
        then { field with AvatarIntersectedPropIds_ = propIds }
        else field

    let updateTeam updater field =
        { field with Team_ = updater field.Team_ }

    let updateAdvents updater field =
        let advents = updater field.Advents_
        if advents =/= field.Advents_ then { field with Advents_ = advents }
        else field

    let private updateProps updater field =
        { field with Props_ = updater field.Props_ }

    let updateProp updater propId field =
        match Map.tryFind propId field.Props_ with
        | Some prop -> updateProps (Map.add propId (updater prop)) field
        | None -> field
    
    let updatePropState updater propId field =
        updateProp (Prop.updatePropState updater) propId field

    let updateInventory updater field =
        { field with Inventory_ = updater field.Inventory_ }

    let updateOptions updater field =
        { field with Options_ = updater field.Options_ }

    let updateMenu updater field =
        { field with Menu_ = updater field.Menu_ }

    let updateDefinitions updater field =
        { field with Definitions_ = updater field.Definitions_ }

    let updateCue updater field =
        { field with Cue_ = updater field.Cue_ }

    let updateScreenTransitioning updater field =
        { field with ScreenTransitioning_ = updater field.ScreenTransitioning_ }

    let updateFieldTransitionOpt updater field =
        { field with FieldTransitionOpt_ = updater field.FieldTransitionOpt_ }

    let updateShopOpt updater field =
        { field with ShopOpt_ = updater field.ShopOpt_ }

    let updateDialogOpt updater field =
        { field with DialogOpt_ = updater field.DialogOpt_ }

    let updateBattleOpt updater field =
        let battleOpt = updater field.BattleOpt_
        { field with BattleOpt_ = battleOpt }

    let updateReference field =
        { field with FieldType_ = field.FieldType_ }

    let updateFieldSongTimeOpt updater field =
        { field with FieldSongTimeOpt_ = updater field.FieldSongTimeOpt_ }

    let clearSpirits field =
        { field with SpiritActivity_ = 0.0f; Spirits_ = [||] }

    let recruit allyType (field : Field) =
        let lowestLevelTeammate = field.Team |> Map.toValueList |> Seq.sortBy (fun teammate -> teammate.Level) |> Seq.head
        let level = max 1 (dec lowestLevelTeammate.Level)
        let index = Map.count field.Team
        let teammate = Teammate.make level index allyType
        updateTeam (Map.add index teammate) field

    let restoreTeam field =
        { field with Team_ = Map.map (fun _ -> Teammate.restore) field.Team_ }

    let advanceUpdateTime field =
        { field with UpdateTime_ = inc field.UpdateTime_ }

    let advanceSpirits (field : Field) world =
        match field.FieldTransitionOpt with
        | None ->
            let field =
                { field with
                    SpiritActivity_ = inc field.SpiritActivity_ }
            let field =
                { field with
                    Spirits_ =
                        Array.map (Spirit.advance (World.getUpdateTime world) field.Avatar.Center) field.Spirits_ }
            let field =
                { field with
                    Spirits_ =
                        Array.filter (fun (spirit : Spirit) ->
                            let delta = field.Avatar.Bottom - spirit.Center
                            let distance = delta.Magnitude
                            distance < Constants.Field.SpiritRadius * 1.25f)
                            field.Spirits }
            let field =
                let spiritsNeeded = int (field.SpiritActivity_ / single Constants.Field.SpiritActivityThreshold)
                let spiritsDeficient = spiritsNeeded - Array.length field.Spirits
                let spiritsSpawned =
                    match Data.Value.Fields.TryGetValue field.FieldType with
                    | (true, fieldData) ->
                        [|0 .. spiritsDeficient - 1|] |>
                        Array.map (fun _ ->
                            let spiritPattern =
                                if spiritsNeeded >= Constants.Field.SpiritActivityAggressionThreshold
                                then SpiritPattern.generate ()
                                else SpiritPattern.Confused
                            match FieldData.tryGetSpiritType field.OmniSeedState field.Avatar.Bottom fieldData with
                            | Some spiritType ->
                                let spiritMovement = SpiritPattern.toSpiritMovement spiritPattern
                                let spirit = Spirit.spawn (World.getUpdateTime world) field.Avatar.Bottom spiritType spiritMovement
                                Some spirit
                            | None -> None) |>
                        Array.definitize
                    | (false, _) -> [||]
                { field with Spirits_ = Array.append field.Spirits_ spiritsSpawned }
            let lowerPerimeter = field.Avatar.LowerPerimeter
            match Array.tryFind (fun (spirit : Spirit) -> lowerPerimeter.Intersects spirit.Bottom) field.Spirits_ with
            | Some spirit ->
                match Data.Value.Fields.TryGetValue field.FieldType with
                | (true, fieldData) ->
                    match fieldData.EncounterTypeOpt with
                    | Some encounterType ->
                        match Data.Value.Encounters.TryGetValue encounterType with
                        | (true, encounterData) ->
                            let battleType =
                                // TODO: toughen up this code.
                                match spirit.SpiritType with
                                | WeakSpirit -> encounterData.BattleTypes.[Gen.random2 0 3]
                                | NormalSpirit -> encounterData.BattleTypes.[Gen.random2 3 6]
                                | StrongSpirit -> encounterData.BattleTypes.[Gen.random2 6 9]
                            match Data.Value.Battles.TryGetValue battleType with
                            | (true, battleData) -> Left (battleData, field)
                            | (false, _) -> Right field
                        | (false, _) -> Right field
                    | None -> Right field
                | (false, _) -> Right field
            | None -> Right field
        | Some _ -> Right field

    let synchronizeTeamFromAllies allies field =
        Map.foldi (fun i field _ (ally : Character) ->
            updateTeam (fun team ->
                match Map.tryFind i team with
                | Some teammate ->
                    let teammate =
                        { teammate with
                            HitPoints = ally.HitPoints
                            TechPoints = ally.TechPoints
                            ExpPoints = ally.ExpPoints }
                    Map.add i teammate team
                | None -> team)
                field)
            field
            allies

    let synchronizeFromBattle consequents battle field =
        let allies = Battle.getAllies battle
        let field = synchronizeTeamFromAllies allies field
        let field = updateInventory (constant battle.Inventory) field
        let field = updateAdvents (Set.union consequents) field
        let field = updateAvatarIntersectedPropIds (constant []) field
        field

    let enterBattle songTime prizePool battleData (field : Field) world =
        let battle = makeBattleFromTeam field.Inventory prizePool field.Team field.Options.BattleSpeed battleData world
        let field = updateFieldSongTimeOpt (constant (Some songTime)) field
        updateBattleOpt (constant (Some battle)) field

    let exitBattle consequents battle field =
        let field = synchronizeFromBattle consequents battle field
        let field = clearSpirits field
        field

    let toSymbolizable field =
        { field with
            UpdateTime_ = 0L
            Avatar_ = field.Avatar
            AvatarCollidedPropIds_ = []
            AvatarSeparatedPropIds_ = []
            AvatarIntersectedPropIds_ = []
            Props_ = Map.empty
            FieldSongTimeOpt_ = None }

    let make fieldType saveSlot randSeedState (avatar : Avatar) team advents inventory world =
        let (debugAdvents, debugKeyItems, definitions) =
            match Data.Value.Fields.TryGetValue fieldType with
            | (true, fieldData) -> (fieldData.FieldDebugAdvents, fieldData.FieldDebugKeyItems, fieldData.Definitions)
            | (false, _) -> (Set.empty, List.empty, Map.empty)
        let (advents, inventory) =
            match fieldType with
            | DebugField -> (debugAdvents, snd (Inventory.tryAddItems (List.map KeyItem debugKeyItems) inventory))
            | _ -> (advents, inventory)
        let omniSeedState = OmniSeedState.makeFromSeedState randSeedState
        let props = makeProps fieldType omniSeedState world
        { UpdateTime_ = 0L
          FieldType_ = fieldType
          FieldState_ = Playing
          SaveSlot_ = saveSlot
          OmniSeedState_ = omniSeedState
          Avatar_ = avatar
          AvatarCollidedPropIds_ = []
          AvatarSeparatedPropIds_ = []
          AvatarIntersectedPropIds_ = []
          SpiritActivity_ = 0.0f
          Spirits_ = [||]
          Team_ = team
          Advents_ = advents
          Props_ = props
          Inventory_ = inventory
          Options_ = { BattleSpeed = PacedSpeed }
          Menu_ = { MenuState = MenuClosed; MenuUseOpt = None }
          Definitions_ = definitions
          DefinitionsOriginal_ = definitions
          Cue_ = CueSystem.Fin
          ScreenTransitioning_ = false
          FieldTransitionOpt_ = None
          ShopOpt_ = None
          DialogOpt_ = None
          BattleOpt_ = None
          FieldSongTimeOpt_ = None
          ViewBoundsAbsolute_ = World.getViewBounds2dAbsolute world }

    let empty world =
        { UpdateTime_ = 0L
          FieldType_ = EmptyField
          FieldState_ = Quit
          SaveSlot_ = Slot1
          OmniSeedState_ = OmniSeedState.make ()
          Avatar_ = Avatar.empty ()
          AvatarCollidedPropIds_ = []
          AvatarSeparatedPropIds_ = []
          AvatarIntersectedPropIds_ = []
          SpiritActivity_ = 0.0f
          Spirits_ = [||]
          Team_ = Map.empty
          Advents_ = Advents.empty
          Props_ = Map.empty
          Inventory_ = Inventory.initial
          Options_ = { BattleSpeed = PacedSpeed }
          Menu_ = { MenuState = MenuClosed; MenuUseOpt = None }
          Definitions_ = Map.empty
          DefinitionsOriginal_ = Map.empty
          Cue_ = CueSystem.Fin
          ScreenTransitioning_ = false
          FieldTransitionOpt_ = None
          ShopOpt_ = None
          DialogOpt_ = None
          BattleOpt_ = None
          FieldSongTimeOpt_ = None
          ViewBoundsAbsolute_ = World.getViewBounds2dAbsolute world }

    let initial saveSlot world =
        make TombOuter saveSlot (max 1UL Gen.randomul) (Avatar.initial ()) (Map.singleton 0 (Teammate.make 3 0 Jinn)) Advents.initial Inventory.initial world

    let debug world =
        make DebugField Slot1 Rand.DefaultSeedState (Avatar.empty ()) (Map.singleton 0 (Teammate.make 3 0 Jinn)) Advents.initial Inventory.initial world

    let debugBattle world =
        let field = debug world
        let battle =
            match Map.tryFind DebugBattle Data.Value.Battles with
            | Some battle ->
                let level = 50
                let team =
                    Map.singleton 0 (Teammate.make level 0 Jinn) |>
                    Map.add 1 (Teammate.make level 1 Mael) |>
                    Map.add 2 (Teammate.make level 2 Riain)
                makeBattleFromTeam Inventory.initial PrizePool.empty team PacedSpeed battle world
            | None -> Battle.empty
        updateBattleOpt (constant (Some battle)) field

    let save field =
        let saveFilePath =
            match field.SaveSlot_ with
            | Slot1 -> Assets.Global.SaveFilePath1
            | Slot2 -> Assets.Global.SaveFilePath2
            | Slot3 -> Assets.Global.SaveFilePath3
        let fieldSymbolizable = toSymbolizable field
        let fieldSymbol = valueToSymbol fieldSymbolizable
        let fileStr = PrettyPrinter.prettyPrintSymbol fieldSymbol PrettyPrinter.defaultPrinter
        try File.WriteAllText (saveFilePath, fileStr) with _ -> ()

    let tryLoad saveSlot world =
        try let saveFilePath =
                match saveSlot with
                | Slot1 -> Assets.Global.SaveFilePath1
                | Slot2 -> Assets.Global.SaveFilePath2
                | Slot3 -> Assets.Global.SaveFilePath3
            let fieldStr = File.ReadAllText saveFilePath
            let field = scvalue<Field> fieldStr
            let props = makeProps field.FieldType_ field.OmniSeedState_ world
            Some { field with Props_ = props }
        with _ -> None

    let loadOrInitial saveSlot world =
        match tryLoad saveSlot world with
        | Some field -> field
        | None -> initial saveSlot world

type Field = Field.Field