// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace OmniBlade
open Prime
open Nu

[<RequireQualifiedAccess>]
module Simulants =

    // global
    let Splash = Screen "Splash"

    // title
    let Title = Screen "Title"
    let TitleGui = Title / "Gui"
    let TitleNew = TitleGui / "New"
    let TitleLoad = TitleGui / "Load"
    let TitleCredits = TitleGui / "Credits"
    let TitleExit = TitleGui / "Exit"

    // credits
    let Credits = Screen "Credits"
    let CreditsGui = Credits / "Gui"
    let CreditsBack = CreditsGui / "Back"

    // intro
    let Intro = Screen "Intro"
    let Intro2 = Screen "Intro2"
    let Intro3 = Screen "Intro3"
    let Intro4 = Screen "Intro4"
    let Intro5 = Screen "Intro5"

    // field
    let Field = Simulants.DefaultScreen
    let FieldHud = Field / "Hud"
    let FieldBack = FieldHud / "Back"
    let FieldScene = Field / "Scene"
    let FieldBackdrop = FieldScene / "Backdrop"
    let FieldTransitionFade = FieldScene / "FieldTransitionFade"
    let FieldTileMap = FieldScene / "TileMap"
    let FieldAvatar = FieldScene / "Avatar"
    let FieldSubmenu = Field / "Submenu"
    let FieldInteract = FieldScene / "Interact"
    let FieldDialog = FieldScene / "Dialog"
    let FieldShop = Field / "Shop"
    let FieldShopBuy = Field / "ShopBuy"
    let FieldShopSell = Field / "ShopSell"
    let FieldShopLeave = Field / "ShopLeave"
    let FieldShopGold = Field / "ShopGold"
    let FieldShopPageUp = Field / "ShopPageUp"
    let FieldShopPageDown = Field / "ShopPageDown"
    let FieldShopConfirm = Field / "ShopConfirm"
    let FieldShopConfirmAccept = Field / "ShopConfirmAccept"
    let FieldShopConfirmDecline = Field / "ShopConfirmDecline"
    let FieldShopConfirmOffer = Field / "ShopConfirmOffer"
    let FieldShopConfirmLine1 = Field / "ShopConfirmLine1"
    let FieldShopConfirmLine2 = Field / "ShopConfirmLine2"
    let SubmenuTeam = Field / "Team"
    let SubmenuItem = Field / "Item"
    let SubmenuClose = Field / "Close"
    let SubmenuUse = Field / "Use"

    // battle
    let Battle = Screen "Battle"
    let BattleHud = Battle / "Hud"
    let BattleScene = Battle / "Scene"
    let BattleDialog = BattleScene / "Dialog"
    let BattleInteract = BattleScene / "Interact"
    let BattleRide = BattleScene / "Ride"