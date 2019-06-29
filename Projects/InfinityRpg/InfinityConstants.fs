namespace InfinityRpg
open OpenTK
open Nu

[<RequireQualifiedAccess>]
module Constants =

    [<RequireQualifiedAccess>]
    module Layout =

        let TileSizeI = Vector2i 64
        let TileSize = let t = TileSizeI in t.Vector2
        let TileSheetSizeM = Vector2i 4
        let TileSheetSizeI = Vector2i.Multiply (TileSheetSizeM, TileSizeI)
        let TileSheetSize = let t = TileSheetSizeI in t.Vector2
        let CharacterDepth = 1.0f
        let CharacterWalkSpeed = 4.0f

    [<RequireQualifiedAccess>]
    module InfinityRpg =

        // dissolve constants
        let DissolveData =
            { IncomingTime = 20L
              OutgoingTime = 30L
              DissolveImage = AssetTag.make<Image> Assets.GuiPackage "Dissolve" }

        // splash constants
        let SplashData =
            { DissolveData = DissolveData
              IdlingTime = 60L
              SplashImage = AssetTag.make<Image> Assets.GuiPackage "Nu" }

        // general data
        let AttackName = "Attack"
        let CharacterAnimationFacingDelay = 16L
        let CharacterAnimationActingDelay = 24L
        let ActionTicksMax = CharacterAnimationActingDelay * 3L