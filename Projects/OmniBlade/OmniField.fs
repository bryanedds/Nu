// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace OmniBlade
open System
open System.IO
open System.Numerics
open FSharpx.Collections
open Prime
open Nu

type [<StructuralEquality; NoComparison>] SaveSlot =
    | Slot1
    | Slot2
    | Slot3

type [<ReferenceEquality; NoComparison>] Options =
    { BattleSpeed : BattleSpeed }

type [<ReferenceEquality; NoComparison>] FieldTransition =
    { FieldType : FieldType
      FieldDestination : Vector2
      FieldDirection : Direction
      FieldTransitionTime : int64 }

[<RequireQualifiedAccess>]
module Field =

    type [<ReferenceEquality; NoComparison>] Field =
        private
            { FieldType_ : FieldType
              SaveSlot_ : SaveSlot
              OmniSeedState_ : OmniSeedState
              Avatar_ : Avatar
              Team_ : Map<int, Teammate>
              SpiritActivity_ : single
              Spirits_ : Spirit array
              Advents_ : Advent Set
              PropStates_ : Map<int, PropState>
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
        member this.OmniSeedState = this.OmniSeedState_
        member this.Avatar = this.Avatar_
        member this.Team = this.Team_
        member this.SpiritActivity = this.SpiritActivity_
        member this.Spirits = this.Spirits_
        member this.Advents = this.Advents_
        member this.PropStates = this.PropStates_
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

    let getPropState propDescriptor (advents : Advent Set) propStates =
        match Map.tryFind propDescriptor.PropId propStates with
        | None ->
            match propDescriptor.PropData with
            | Portal (_, _, _, _, _, _, requirements) -> PortalState (propDescriptor.PropBounds, advents.IsSupersetOf requirements)
            | Door (_, _, _, _, _) -> DoorState false
            | Switch (_, _, _, _) -> SwitchState false
            | Seal (_, _, requirements) -> SealState (not (advents.IsSupersetOf requirements))
            | Npc (npcType, direction, _, requirements) | NpcBranching (npcType, direction, _, requirements) -> NpcState (npcType, direction, colWhite, colZero, advents.IsSupersetOf requirements && NpcType.exists advents npcType)
            | Shopkeep (_, _, _, requirements) -> ShopkeepState (advents.IsSupersetOf requirements)
            | Chest (_, _, id, _, _, _) -> ChestState (propDescriptor.PropBounds, id)
            | Sensor _ | Flame _ | SavePoint | ChestSpawn | EmptyProp -> NilState
        | Some propState -> propState

    let getPropStates (field : Field) world =
        match Map.tryFind field.FieldType Data.Value.Fields with
        | Some fieldData ->
            FieldData.getPropDescriptors field.OmniSeedState fieldData world |>
            Map.ofListBy (fun propDescriptor -> (propDescriptor.PropId, getPropState propDescriptor field.Advents field.PropStates))
        | None -> Map.empty

    let getProps (field : Field) world =
        match Map.tryFind field.FieldType Data.Value.Fields with
        | Some fieldData ->
            FieldData.getPropDescriptors field.OmniSeedState fieldData world |>
            Map.ofListBy (fun propDescriptor ->
                let propState = getPropState propDescriptor field.Advents field.PropStates
                let prop = Prop.make propDescriptor.PropBounds propDescriptor.PropElevation field.Advents propDescriptor.PropData propState propDescriptor.PropId
                (propDescriptor.PropId, prop))
        | None -> Map.empty

    let getChests field world =
        getPropStates field world |>
        Map.toValueArray |>
        Array.choose (function ChestState (bounds, id) -> Some (Chest.make bounds (field.Advents.Contains (Opened id))) | _ -> None)

    let getPortals field world =
        getPropStates field world |>
        Map.toValueArray |>
        Array.choose (function PortalState (bounds, active) -> Some (Portal.make bounds active) | _ -> None)

    let updateFieldType updater field =
        { field with
            FieldType_ = updater field.FieldType_
            SpiritActivity_ = 0.0f }

    let updateAvatar updater field =
        { field with Avatar_ = updater field.Avatar_ }

    let updateTeam updater field =
        { field with Team_ = updater field.Team_ }

    let updateAdvents updater field =
        { field with Advents_ = updater field.Advents_ }

    let updatePropStates updater field =
        { field with PropStates_ = updater field.PropStates_ }

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
        { field with
            BattleOpt_ = battleOpt
            SpiritActivity_ = if Option.isSome battleOpt then 0.0f else field.SpiritActivity_ }

    let updateReference field =
        { field with FieldType_ = field.FieldType_ }

    let updateFieldSongTimeOpt updater field =
        { field with FieldSongTimeOpt_ = updater field.FieldSongTimeOpt_ }

    let recruit allyType (field : Field) =
        let index = Map.count field.Team
        let teammate = Teammate.make index allyType
        updateTeam (Map.add index teammate) field

    let restoreTeam field =
        { field with Team_ = Map.map (fun _ -> Teammate.restore) field.Team_ }

    let hasEncounters (field : Field) =
        match Data.Value.Fields.TryGetValue field.FieldType with
        | (true, fieldData) -> Option.isSome fieldData.EncounterTypeOpt
        | (false, _) -> false

    let clearSpirits field =
        { field with Spirits_= [||] }

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
                            let distance = delta.Length ()
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
                                let spiritMovement = SpiritPattern.toSpiritMovement (SpiritPattern.random ())
                                let spirit = Spirit.spawn (World.getUpdateTime world) field.Avatar.Bottom spiritType spiritMovement
                                Some spirit
                            | None -> None) |>
                        Array.definitize
                    | (false, _) -> [||]
                { field with Spirits_ = Array.append field.Spirits_ spiritsSpawned }
            match Array.tryFind (fun (spirit : Spirit) -> Math.isPointInBounds spirit.Position field.Avatar.LowerBounds) field.Spirits_ with
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
            FieldSongTimeOpt_ = None }

    let make fieldType saveSlot randSeedState avatar team inventory definitions =
        { FieldType_ = fieldType
          SaveSlot_ = saveSlot
          OmniSeedState_ = OmniSeedState.makeFromSeedState randSeedState
          Avatar_ = avatar
          SpiritActivity_ = 0.0f
          Spirits_ = [||]
          Team_ = team
          Advents_ = Set.empty
          PropStates_ = Map.empty
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
        { FieldType_ = DebugRoom
          SaveSlot_ = Slot1
          OmniSeedState_ = OmniSeedState.make ()
          Avatar_ = Avatar.empty
          Team_ = Map.empty
          SpiritActivity_ = 0.0f
          Spirits_ = [||]
          Advents_ = Set.empty
          PropStates_ = Map.empty
          Inventory_ = { Items = Map.empty; Gold = 0 }
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

    let debug =
        { empty with
            FieldType_ = DebugSection
            Team_ = Map.singleton 0 (Teammate.make 0 Jinn) }

    let initial saveSlot randSeedState =
        let fieldType = TombOuter
        let definitions =
            match Data.Value.Fields.TryGetValue fieldType with
            | (true, fieldData) -> fieldData.Definitions
            | (false, _) -> Map.empty
        make
            fieldType
            saveSlot
            randSeedState
            Avatar.initial
            (Map.singleton 0 (Teammate.make 0 Jinn))
            Inventory.initial
            definitions

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

    let tryLoad saveSlot =
        try let saveFilePath =
                match saveSlot with
                | Slot1 -> Assets.Global.SaveFilePath1
                | Slot2 -> Assets.Global.SaveFilePath2
                | Slot3 -> Assets.Global.SaveFilePath3
            let fieldStr = File.ReadAllText saveFilePath
            fieldStr |> scvalue<Field> |> Some
        with _ -> None

type Field = Field.Field