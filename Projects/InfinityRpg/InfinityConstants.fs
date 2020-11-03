namespace InfinityRpg
open Nu

[<RequireQualifiedAccess>]
module Constants =

    [<RequireQualifiedAccess>]
    module Layout =

        let TileSizeI = v2iDup 64
        let TileSize = let t = TileSizeI in t.Vector2
        let TileSheetSizeM = v2iDup 4
        let TileSheetSizeI = Vector2i.Multiply (TileSheetSizeM, TileSizeI)
        let TileSheetSize = let t = TileSheetSizeI in t.Vector2
        let FieldUnitSizeM = v2iDup 22
        let CharacterDepth = 1.0f
        let PickupDepth = 0.5f

    [<RequireQualifiedAccess>]
    module InfinityRpg =

        // dissolve constants
        let DissolveDescriptor =
            { IncomingTime = 20L
              OutgoingTime = 30L
              DissolveImage = asset<Image> Assets.GuiPackageName "Dissolve" }

        // splash constants
        let SplashData =
            { DissolveDescriptor = DissolveDescriptor
              IdlingTime = 60L
              SplashImageOpt = Some (asset<Image> Assets.GuiPackageName "Nu") }

        // general data
        let AttackName = "Attack"
        let CharacterWalkSpeed = 8.0f // original value is 4.0f
        let CharacterWalkResolution = Layout.TileSize.X / CharacterWalkSpeed
        let CharacterAnimationFacingDelay = 16L
        let CharacterAnimationActingDelay = 12L // original value is 24L
        let ReactionTick = CharacterAnimationActingDelay * 2L
        let ActionTicksMax = CharacterAnimationActingDelay * 3L