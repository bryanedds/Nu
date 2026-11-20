// Zombie Game.
// Copyright (C) Bryan Edds.

namespace ZombieGame
open System
open System.Numerics
open Prime
open Nu

/// What a surface is made of, which decides how it takes the light.
type SurfaceType =
    | Cloth // scrubs, gowns, curtains and upholstery - matte
    | Skin // faces and hands - softly lit through
    | Hair
    | Leather // boots, belts and holsters
    | Hide // a beast's living skin - leathery but softly lit through, as skin is
    | Rot // rotten flesh - wet and dull
    | Blood // blood, still wet - dark and glossy
    | Bone // bone and teeth
    | Wood // doors, desks and shelves
    | Paint // painted walls and doors - an eggshell sheen
    | Linoleum // the floors - a waxed sheen
    | Tile // glazed ceramic tile, as the morgue's walls
    | Concrete // bare concrete, as the ambulance bay and generator room
    | Plastic // chairs, monitors, trays and casings
    | Rubber // mats, tires and seals
    | Steel // brushed steel - trolleys, cabinets, the morgue's drawers
    | Chrome // polished chrome - fittings and instruments, mirroring the room
    | Glass // windows, vials and screens
    | Light // made of light - lamps, glowing eyes and screens lit up

    /// The one material every surface shares. Its maps are all solid white so that each surface's material properties
    /// alone decide its roughness, metalness, emission and clear coat - the engine's default maps would otherwise halve
    /// roughness and zero out metalness, emission and clear coat. Sharing one material also keeps the surfaces batching.
    static member val BasicMaterial =
        { Material.empty with
            RoughnessImageOpt = ValueSome Assets.Gameplay.WhiteImage
            MetallicImageOpt = ValueSome Assets.Gameplay.WhiteImage
            EmissionImageOpt = ValueSome Assets.Gameplay.WhiteImage
            ClearCoatImageOpt = ValueSome Assets.Gameplay.WhiteImage
            ClearCoatRoughnessImageOpt = ValueSome Assets.Gameplay.WhiteImage }

    /// The material for see-through surfaces, as lantern glass - forward rendered and blended, since deferred surfaces
    /// can't be transparent. Draw with it any surface whose color isn't opaque.
    static member val TranslucentMaterial =
        { SurfaceType.BasicMaterial with
            BlendOpt = ValueSome Transparent
            DepthWriteOpt = ValueSome false } // so what's within, as a lantern's flame, still draws whichever sorts first

    /// The material of a surface in a color - see-through, as glass, if the color is and it's not made of light.
    static member getMaterial surface (color : Color) =
        if color.A < 1.0f && surface <> Light then SurfaceType.TranslucentMaterial else SurfaceType.BasicMaterial

    /// How a surface in a color renders - forward if it glows past its color, is see-through, or is to stay out of
    /// screen-space reflections, as a ceiling, else deferred.
    static member getRenderStyle surface (color : Color) unreflected =
        if surface = Light || color.A < 1.0f || unreflected then Forward (0.0f, 0.0f) else Deferred

/// How a surface takes the light - its roughness, metalness, subsurface scattering and clear coat.
type SurfaceData =
    { Surface : SurfaceType
      Roughness : single
      Metallic : single
      ScatterType : ScatterType
      ClearCoat : single }

    /// The material properties of a surface in a color, glowing by an emission.
    static member makeProperties (data : SurfaceData) (color : Color) emission =
        { MaterialProperties.empty with
            AlbedoOpt = ValueSome color
            RoughnessOpt = ValueSome data.Roughness
            MetallicOpt = ValueSome data.Metallic
            AmbientOcclusionOpt = ValueSome 1.0f
            EmissionOpt = ValueSome emission
            ScatterTypeOpt = ValueSome data.ScatterType
            ClearCoatOpt = ValueSome data.ClearCoat
            ClearCoatRoughnessOpt = ValueSome 0.04f }