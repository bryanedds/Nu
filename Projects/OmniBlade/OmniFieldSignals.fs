// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace OmniBlade
open System
open System.Numerics
open Nu
open Nu.Declarative

type FieldMessage =
    | Update
    | UpdateFieldTransition
    | MenuTeamOpen
    | MenuTeamAlly of int
    | MenuItemsOpen
    | MenuItemsPageUp
    | MenuInventoryPageDown
    | MenuItemSelect of int * (ItemType * int Option)
    | MenuItemUse of int
    | MenuItemCancel
    | MenuTechOpen
    | MenuTechAlly of int
    | MenuTechSelect of int
    | MenuOptionsOpen
    | MenuOptionsSelectBattleSpeed of BattleSpeed
    | MenuClose
    | ShopBuy
    | ShopSell
    | ShopPageUp
    | ShopPageDown
    | ShopSelect of int * (ItemType * int Option)
    | ShopConfirmAccept
    | ShopConfirmDecline
    | ShopLeave
    | PromptLeft
    | PromptRight
    | TryBattle of BattleType * Advent Set
    | Interact

type [<NoComparison>] FieldCommand =
    | ProcessKeyInput
    | ProcessTouchInput of Vector2
    | UpdateEye
    | PlayFieldSong
    | PlaySound of int64 * single * Sound AssetTag
    | PlaySong of int * int * single * double * Song AssetTag
    | FadeOutSong of int
    | Nop