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
    let TitlePlay = TitleGui / "Play"
    let TitleCredits = TitleGui / "Credits"
    let TitleExit = TitleGui / "Exit"

    // credits
    let Credits = Screen "Credits"
    let CreditsGui = Credits / "Gui"
    let CreditsBack = CreditsGui / "Back"

    // field
    let Field = Simulants.DefaultScreen
    let FieldHud = Field / "Hud"
    let FieldBack = FieldHud / "Back"
    let FieldScene = Field / "Scene"
    let FieldBackdrop = FieldScene / "Backdrop"
    let FieldPortalFade = FieldScene / "PortalFade"
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
    let SubmenuLegion = Field / "Legion"
    let SubmenuItem = Field / "Item"
    let SubmenuClose = Field / "Close"
    let SubmenuUse = Field / "Use"

    // battle
    let Battle = Screen "Battle"
    let BattleHud = Battle / "Hud"
    let BattleScene = Battle / "Scene"
    let BattleRide = BattleScene / "Ride"