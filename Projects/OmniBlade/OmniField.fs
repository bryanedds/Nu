// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace OmniBlade
open System
open System.IO
open System.Numerics
open FSharpx.Collections
open Prime
open Nu

type SaveSlot =
    | Slot1
    | Slot2
    | Slot3

type [<ReferenceEquality; NoComparison>] Options =
    { BattleSpeed : BattleSpeed }

type FieldState =
    | Playing
    | Quitting

type [<ReferenceEquality; NoComparison>] FieldTransition =
    { FieldType : FieldType
      FieldDestination : Vector3
      FieldDirection : Direction
      FieldTransitionTime : int64 }

[<RequireQualifiedAccess>]
module Field =

    type [<ReferenceEquality; NoComparison>] Field =
        private
            { FieldType_ : FieldType
              FieldState_ : FieldState
              SaveSlot_ : SaveSlot
              OmniSeedState_ : OmniSeedState
              Avatar_ : Avatar
              Team_ : Map<int, Teammate>
              SpiritActivity_ : single
              Spirits_ : Spirit array
              Advents_ : Advent Set
              Props_ : Map<int, Prop>
              Inventory_ : Inventory
              Options_ : Options
              Menu_ : Menu
              Definitions_ : CueDefinitions
              DefinitionsOriginal_ : CueDefinitions
              Cue_ : Cue
              ShopOpt_ : Shop option
              FieldTransitionOpt_ : FieldTransition option
              DialogOpt_ : Dialog option
              BattleOpt_ : Battle option
              FieldSongTimeOpt_ : int64 option }

        (* Local Properties *)
        member this.FieldType = this.FieldType_
        member this.FieldState = this.FieldState_
        member this.OmniSeedState = this.OmniSeedState_
        member this.Avatar = this.Avatar_
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
        member this.ShopOpt = this.ShopOpt_
        member this.FieldTransitionOpt = this.FieldTransitionOpt_
        member this.DialogOpt = this.DialogOpt_
        member this.BattleOpt = this.BattleOpt_
        member this.FieldSongTimeOpt = this.FieldSongTimeOpt_

    let private makePropState time propDescriptor =
        match propDescriptor.PropData with
        | Sprite (_, image, color, blend, glow, flip, visible) -> SpriteState (image, color, blend, glow, flip, visible)
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
                  Direction = direction }
            CharacterState (Color.One, characterAnimationState)
        | Portal _ | Chest _ | Sensor _ | Npc _ | NpcBranching _ | Shopkeep _ | Seal _ | Flame _ | SavePoint | ChestSpawn | EmptyProp -> NilState

    let private makeProps fieldType omniSeedState advents pointOfInterest world =
        match Map.tryFind fieldType Data.Value.Fields with
        | Some fieldData ->
            let time = World.getUpdateTime world
            FieldData.getPropDescriptors omniSeedState fieldData world |>
            Map.ofListBy (fun propDescriptor ->
                let propState = makePropState time propDescriptor
                let prop = Prop.make propDescriptor.PropPerimeter propDescriptor.PropElevation advents pointOfInterest propDescriptor.PropData propState propDescriptor.PropId
                (propDescriptor.PropId, prop))
        | None -> Map.empty

    let getRecruitmentFee (field : Field) =
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

    let updateFieldType updater field world =
        let fieldType = updater field.FieldType_
        let spiritActivity = 0.0f
        let props = makeProps fieldType field.OmniSeedState_ field.Advents_ field.Avatar_.BottomOffset world
        { field with FieldType_ = fieldType; SpiritActivity_ = spiritActivity; Spirits_ = [||]; Props_ = props }

    let updateFieldState updater field =
        { field with FieldState_ = updater field.FieldState_ }

    let updateAvatar updater field =
        let avatar = field.Avatar_
        let pointOfInterest = avatar.BottomOffset
        let avatar = updater avatar : Avatar
        let pointOfInterest' = avatar.BottomOffset
        let props =
            if pointOfInterest <> pointOfInterest'
            then Map.map (constant (Prop.updatePointOfInterest (constant pointOfInterest'))) field.Props_
            else field.Props_
        { field with
            Avatar_ = avatar
            Props_ = props }

    let updateTeam updater field =
        { field with Team_ = updater field.Team_ }

    let updateAdvents updater field =
        let advents = updater field.Advents_
        if advents <> field.Advents_ then
            let props = Map.map (fun _ prop -> Prop.updateAdvents (constant advents) prop) field.Props_
            { field with Advents_ = advents; Props_ = props }
        else field

    let private updateProps updater field =
        { field with Props_ = updater field.Props_ }

    // NOTE: I really don't like the need to do these inefficient reverse map look-ups as a matter of course. Perhaps
    // there's an elegant alternative that is more performant.
    let tryGetPropIdByData predicate field =
        Map.tryFindKey (fun _ (prop : Prop) ->
            predicate prop.PropData)
            field.Props_

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

    let updateShopOpt updater field =
        { field with ShopOpt_ = updater field.ShopOpt_ }

    let updateDialogOpt updater field =
        { field with DialogOpt_ = updater field.DialogOpt_ }

    let updateFieldTransitionOpt updater field =
        { field with FieldTransitionOpt_ = updater field.FieldTransitionOpt_ }

    let updateBattleOpt updater field =
        let battleOpt = updater field.BattleOpt_
        { field with BattleOpt_ = battleOpt }

    let updateReference field =
        { field with FieldType_ = field.FieldType_ }

    let updateFieldSongTimeOpt updater field =
        { field with FieldSongTimeOpt_ = updater field.FieldSongTimeOpt_ }

    let recruit allyType (field : Field) =
        let lowestLevelTeammate = field.Team |> Map.toValueList |> Seq.sortBy (fun teammate -> teammate.Level) |> Seq.head
        let level = max 1 (dec lowestLevelTeammate.Level)
        let index = Map.count field.Team
        let teammate = Teammate.make level index allyType
        updateTeam (Map.add index teammate) field

    let restoreTeam field =
        { field with Team_ = Map.map (fun _ -> Teammate.restore) field.Team_ }

    let hasEncounters (field : Field) =
        match Data.Value.Fields.TryGetValue field.FieldType with
        | (true, fieldData) -> Option.isSome fieldData.EncounterTypeOpt
        | (false, _) -> false

    let clearSpirits field =
        { field with
            SpiritActivity_ = 0.0f
            Spirits_ = [||] }

    let updateSpirits (field : Field) world =
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
                let spiritActivity = max 0.0f (field.SpiritActivity_  - single Constants.Field.SpiritActivityMinimum)
                let spiritsNeeded = int (spiritActivity / single Constants.Field.SpiritActivityThreshold)
                let spiritsDeficient = spiritsNeeded - Array.length field.Spirits
                let spiritsSpawned =
                    match Data.Value.Fields.TryGetValue field.FieldType with
                    | (true, fieldData) ->
                        [|0 .. spiritsDeficient - 1|] |>
                        Array.map (fun _ ->
                            match FieldData.tryGetSpiritType field.OmniSeedState field.Avatar.Bottom fieldData world with
                            | Some spiritType ->
                                let spiritMovement = SpiritPattern.toSpiritMovement (SpiritPattern.generate ())
                                let spirit = Spirit.spawn (World.getUpdateTime world) field.Avatar.Bottom spiritType spiritMovement
                                Some spirit
                            | None -> None) |>
                        Array.definitize
                    | (false, _) -> [||]
                { field with Spirits_ = Array.append field.Spirits_ spiritsSpawned }
            let lowerPerimeter = field.Avatar.LowerPerimeter
            match Array.tryFind (fun (spirit : Spirit) -> lowerPerimeter.Intersects spirit.Position) field.Spirits_ with
            | Some spirit ->
                match Data.Value.Fields.TryGetValue field.FieldType with
                | (true, fieldData) ->
                    match fieldData.EncounterTypeOpt with
                    | Some encounterType ->
                        match Data.Value.Encounters.TryGetValue encounterType with
                        | (true, encounterData) ->
                            let battleType =
                                // TODO: P1: toughen up this code.
                                match spirit.SpiritType with
                                | WeakSpirit -> encounterData.BattleTypes.[Gen.random2 0 3]
                                | NormalSpirit -> encounterData.BattleTypes.[Gen.random2 3 6]
                                | StrongSpirit -> encounterData.BattleTypes.[Gen.random2 6 9]
                            match Data.Value.Battles.TryGetValue battleType with
                            | (true, battleData) ->
                                let field = { field with Spirits_ = [||] }
                                Left (battleData, field)
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
        field

    let toSymbolizable field =
        { field with
            Avatar_ = Avatar.toSymbolizable field.Avatar
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
        let props = makeProps fieldType omniSeedState advents avatar.BottomOffset world
        { FieldType_ = fieldType
          FieldState_ = Playing
          SaveSlot_ = saveSlot
          OmniSeedState_ = omniSeedState
          Avatar_ = avatar
          SpiritActivity_ = 0.0f
          Spirits_ = [||]
          Team_ = team
          Advents_ = advents
          Props_ = props
          Inventory_ = inventory
          Options_ = { BattleSpeed = SwiftSpeed }
          Menu_ = { MenuState = MenuClosed; MenuUseOpt = None }
          Definitions_ = definitions
          DefinitionsOriginal_ = definitions
          Cue_ = Cue.Nil
          ShopOpt_ = None
          FieldTransitionOpt_ = None
          DialogOpt_ = None
          BattleOpt_ = None
          FieldSongTimeOpt_ = None }

    let empty =
        { FieldType_ = EmptyField
          FieldState_ = Quitting
          SaveSlot_ = Slot1
          OmniSeedState_ = OmniSeedState.make ()
          Avatar_ = Avatar.initial
          SpiritActivity_ = 0.0f
          Spirits_ = [||]
          Team_ = Map.empty
          Advents_ = Advents.empty
          Props_ = Map.empty
          Inventory_ = Inventory.initial
          Options_ = { BattleSpeed = SwiftSpeed }
          Menu_ = { MenuState = MenuClosed; MenuUseOpt = None }
          Definitions_ = Map.empty
          DefinitionsOriginal_ = Map.empty
          Cue_ = Cue.Nil
          ShopOpt_ = None
          FieldTransitionOpt_ = None
          DialogOpt_ = None
          BattleOpt_ = None
          FieldSongTimeOpt_ = None }

    let initial saveSlot randSeedState world =
        make TombOuter saveSlot randSeedState Avatar.initial (Map.singleton 0 (Teammate.make 3 0 Jinn)) Advents.initial Inventory.initial world

    let debug world =
        make DebugField Slot1 Rand.DefaultSeedState Avatar.empty (Map.singleton 0 (Teammate.make 3 0 Jinn)) Advents.initial Inventory.initial world

    let debugBattle world =
        let field = debug world
        let battle = Battle.debug
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
            let props = makeProps field.FieldType_ field.OmniSeedState_ field.Advents_ field.Avatar_.BottomOffset world
            Some { field with Props_ = props }
        with _ -> None

type Field = Field.Field