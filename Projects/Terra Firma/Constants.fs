namespace TerraFirma
open System
open Nu

[<RequireQualifiedAccess>]
module Constants =

    [<RequireQualifiedAccess>]
    module Gameplay =

        let [<Literal>] CharacterAnimatedModelName = "AnimatedModel"
        let [<Literal>] CharacterWeaponName = "Weapon"
        let [<Literal>] CharacterWeaponHandBoneName = "mixamorig:RightHand"

    [<RequireQualifiedAccess>]
    module Foliage =

        let [<Literal>] PipelineName = "Foliage"
        let [<Literal>] GustTileSize = 24.0f // meters per gust noise tile (must match GUST_SCALE in Foliage.vert)
        let [<Literal>] ShaderPath = "Assets/Gameplay/Foliage/Foliage"
        let [<Literal>] VariationResolution = 128
        let [<Literal>] VariationPeriod = 8 // noise lattice cells across the variation texture
        let [<Literal>] VariationOctaveRatio = 2.37f // non-integer so that the two octaves don't repeat together