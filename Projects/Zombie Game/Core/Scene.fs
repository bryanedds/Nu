// Zombie Game.
// Copyright (C) Bryan Edds.

namespace ZombieGame
open System
open System.ComponentModel
open System.IO
open System.Numerics
open Prime
open Nu

/// The primitive shapes everything is built of - puppets' parts, things in hand and scenes' parts.
type PartShapeType =
    | PartBox
    | PartBall
    | PartCylinder
    | PartCone
    | PartQuad // a flat square facing +z

    /// The primitive model for a shape.
    static member getModel shape =
        match shape with
        | PartBox -> Assets.Gameplay.BoxModel
        | PartBall -> Assets.Gameplay.BallModel
        | PartCylinder -> Assets.Gameplay.CylinderModel
        | PartCone -> Assets.Gameplay.ConeModel
        | PartQuad -> Assets.Gameplay.QuadModel

/// A slow swell and fade, as of a glow breathing - how far it swells either way, as a share of its strength, and the
/// seconds a swell and fade take.
[<SymbolicExpansion>]
type Pulse =
    { Depth : single
      Period : single }

    /// How much a pulse scales a strength at a time in seconds, from a phase in radians.
    static member getScale (time : single) phase (pulse : Pulse) =
        1.0f + pulse.Depth * sin (MathF.Tau * time / pulse.Period + phase)

/// What's been done in play, kept as the player goes from area to area - and what a save remembers.
type Advent =
    | Taken of string // a pickup, by its prop's name
    | Killed of string // a creature, by its enemy's name - gone for good
    | Unlocked of string // a door, by its prop's name - a door and its partner sharing it
    | Examined of string // something looked at that changes things, by its name
    | PowerRestored // the generator running again on its fuse
    | Enraged of string // a creature, by its enemy's name - hurt so badly it's driven to rage

/// Whether something holds of the game as it stands, as a cue asks - by what's been done, or what the player
/// carries.
type CueCondition =
    | Flagged of Advent
    | Carries of ItemType
    | Not of CueCondition
    | AllOf of CueCondition list
    | AnyOf of CueCondition list

    /// Whether a condition holds, by what's been done and what's carried.
    static member holdsBy (has : Advent -> bool) (carries : ItemType -> bool) condition =
        match condition with
        | Flagged flag -> has flag
        | Carries item -> carries item
        | Not condition -> not (CueCondition.holdsBy has carries condition)
        | AllOf conditions -> List.forall (CueCondition.holdsBy has carries) conditions
        | AnyOf conditions -> List.exists (CueCondition.holdsBy has carries) conditions

    /// The flags a condition looks at.
    static member getFlags condition =
        match condition with
        | Flagged flag -> [flag]
        | Carries _ -> []
        | Not condition -> CueCondition.getFlags condition
        | AllOf conditions | AnyOf conditions -> List.collect CueCondition.getFlags conditions

/// An area of the hospital - each the scene file whose `Area` statement names it.
type HospitalAreaType =
    | Reception
    | NurseStation // the safe room
    | WestHall
    | Records
    | Pharmacy
    | EastHall
    | Morgue
    | Generator
    | Security
    | AmbulanceBay

/// An area of the dorm - each the scene file whose `Area` statement names it.
type DormAreaType =
    | Lot // the parking lot out front, where the ambulance gives out
    | Lobby
    | Lounge // the safe room
    | EastCorridor
    | Laundry
    | Room104
    | Showers
    | Stairwell
    | UpperCorridor
    | RaOffice
    | Room212
    | Gym

/// An area of the meat packing plant - each the scene file whose `Area` statement names it.
type PlantAreaType =
    | Yard // the rail yard out front, where the van pulls in
    | Dock // the loading dock
    | Office // the foreman's office - the safe room
    | KillFloor
    | Lockers
    | Grinder
    | Smokehouse
    | Boiler
    | ColdStorage
    | ControlRoom
    | Rendering
    | Pit // the offal pit beneath the plant, down the freight elevator, where the meat king waits
    | Platform // the rail platform behind the plant, where the handcar is

/// An area - a place within a stage of the game, as Hospital Reception.
type AreaType =
    | Hospital of HospitalAreaType
    | Dorm of DormAreaType
    | Plant of PlantAreaType

    /// The area's name, as Hospital+Reception - naming the entity the area is declared under.
    member this.Name =
        match this with
        | Hospital area -> "Hospital+" + getCaseName area
        | Dorm area -> "Dorm+" + getCaseName area
        | Plant area -> "Plant+" + getCaseName area

/// The look of a door, as its opening shows it.
type DoorStyle =
    | WoodenDoor
    | DoubleDoor
    | SteelDoor
    | Shutter // the ambulance bay's roller shutter

    /// The sound a door of the style makes opening.
    member this.Sound =
        match this with
        | WoodenDoor -> Assets.Gameplay.DoorSound
        | DoubleDoor | SteelDoor -> Assets.Gameplay.DoubleDoorSound
        | Shutter -> Assets.Gameplay.ShutterSound

    /// How loud a door of the style plays opening.
    member this.Volume =
        match this with
        | WoodenDoor | DoubleDoor | SteelDoor -> 0.35f
        | Shutter -> 0.25f

    /// How wide and high the doorway a door of the style fills is.
    member this.Doorway =
        match this with
        | WoodenDoor -> v2 1.0f 2.1f
        | DoubleDoor -> v2 1.8f 2.1f
        | SteelDoor -> v2 1.2f 2.1f
        | Shutter -> v2 3.0f 2.8f

    /// Where a leaf of a door of the style is, and how it's turned, swung open so far, from where it stands shut - both in
    /// the door's own frame, its origin at the foot of its doorway's middle, facing +z.
    member this.Swing swing (position : Vector3) (rotation : Quaternion) =
        match this with
        | Shutter ->
            // the shutter rolling up into the dark, back behind the wall's face as it goes
            (position + v3 0.0f (swing * 1.6f) (-0.12f * min 1.0f (swing * 4.0f)), rotation)
        | WoodenDoor | DoubleDoor | SteelDoor ->
            // a leaf swinging on its hinge at the doorway's side - a double door's two each on their own side
            let side = if this = DoubleDoor && position.X >= 0.0f then 1.0f else -1.0f
            let hinge = v3 (side * this.Doorway.X * 0.5f) 0.0f 0.0f
            let turn = Quaternion.CreateFromAxisAngle (v3Up, if this = DoubleDoor then side * swing else swing)
            (hinge + Vector3.Transform (position - hinge, turn), Quaternion.Concatenate (rotation, turn))

/// Trying a door into another area - the area it leads to, how it looks opening, the item that
/// unlocks it, if it's locked, the condition it opens only while, if any, as the security door once the power's back,
/// and what's said trying it while it won't open. A door unlocked stays unlocked, its key used up - as the flag
/// Unlocked keeps. A door and its partner in the area it leads to share a name, so unlocking one unlocks both, and one
/// arrives through a door before its partner.
[<SymbolicExpansion>]
type EnterData =
    { To : AreaType
      Style : DoorStyle
      Key : ItemType option
      Opens : CueCondition option
      Locked : string option } // "It's locked." by default

    /// What's said trying the door while it won't open.
    member this.LockedLine =
        defaultArg this.Locked "It's locked."

/// Travelling on to another stage - the area one arrives in, the arrival there one arrives at, and the caption shown in
/// the dark on the way, as where and when it is.
[<SymbolicExpansion>]
type TravelData =
    { To : AreaType
      Arrival : string
      Caption : string }

/// What happens when a prop is interacted with, as Omni Blade's cues - lines said, flags set, items taken in and given
/// up, either of two cues by a condition, in turn, a door tried, the prop picked up, the typewriter's saving, the item
/// box's opening, a keypad asking its code, sounds, the dead of the area rising, travelling on to another stage, and
/// play won.
type Cue =
    | Fin // nothing more
    | Say of string list // lines shown in turn
    | SetFlag of Advent
    | If of CueCondition * Cue * Cue
    | Sequence of Cue list
    | Give of ItemType * int // put in the player's hands, if there's room, else left where it was with a word why
    | GiveAs of string * ItemType * int // given as a pickup by that name is, flagged taken once it's in hand
    | Take of ItemType // taken from the player, as a fuse seated in its panel
    | Enter of EnterData // the door interacted with tried
    | PickUp of PickUpData // the prop interacted with picked up
    | Save // the typewriter, offering to save for an ink ribbon
    | OpenBox // the item box, open to put things in and take things out
    | Keypad of KeypadData // a keypad asking its code
    | Sound of SoundType
    | Rouse // the dead lying on the area's floor get up, all at once
    | Travel of TravelData // on to another stage, as driving away in the ambulance
    | Escape // play's won

    /// A cue, then another after it.
    static member andThen cue after =
        match (cue, after) with
        | (_, Fin) -> cue
        | (Fin, _) -> after
        | (_, _) -> Sequence [cue; after]

    /// A cue and every cue within it, in order.
    static member getAll cue =
        cue ::
        match cue with
        | If (_, consequent, alternative) -> Cue.getAll consequent @ Cue.getAll alternative
        | Sequence cues -> List.collect Cue.getAll cues
        | PickUp pickUp -> pickUp.Then |> Option.map Cue.getAll |> Option.defaultValue []
        | Keypad keypad -> Cue.getAll keypad.Then
        | Fin | Say _ | SetFlag _ | Give _ | GiveAs _ | Take _ | Enter _ | Save | OpenBox | Sound _ | Rouse | Travel _ | Escape -> []

    /// The conditions a cue asks, anywhere within it - its ifs', and the doors' it tries.
    static member getConditions cue =
        Cue.getAll cue |>
        List.choose (function
            | If (condition, _, _) -> Some condition
            | Enter enter -> enter.Opens
            | Fin | Say _ | SetFlag _ | Sequence _ | Give _ | GiveAs _ | Take _ | PickUp _ | Save | OpenBox | Keypad _ | Sound _ | Rouse | Travel _ | Escape -> None)

/// Picking up the prop interacted with, something lying about - the item and how many, and what happens once it's
/// taken, if anything, as the morgue's dead getting up when its fuse is. What's taken of it is gone for good, as the
/// flag Taken keeps, so its prop's name is unique in the game.
and [<SymbolicExpansion>] PickUpData =
    { Item : ItemType
      Count : int
      Then : Cue option }

/// A keypad asking its code - the code, and what happens once it's entered, as the records drawer coming open. A wrong
/// code only buzzes.
and [<SymbolicExpansion>] KeypadData =
    { Code : string
      Then : Cue }

/// A wall of an area, by the way it faces from the area's middle.
type WallType =
    | North
    | South
    | East
    | West

/// Where a prop or enemy goes in a scene - one way only, at a point, against a wall, or before or above another prop -
/// and which way it faces. Positions are on the ground in meters (x east, z south), angles in degrees, as the scene file
/// states them.
[<SymbolicExpansion>]
type Placement =
    { At : Vector2 option // the point on the ground it stands at, facing as Facing says
      Against : WallType option // set against a wall of the area, facing into it
      Along : single // where along that wall - its x for a north or south wall, its z for an east or west one
      Gap : single // how far it stands out from the wall or prop it's placed by
      Before : string option // stands before the named prop, facing the way it does
      Above : string option // hangs above the named prop, facing the way it does
      Height : single option // how high above the floor, where its appearance's own height won't do
      Facing : single } // the way a prop placed at a point faces, in degrees turned from south toward east

/// A slab of a surface, of any size, solid to walkers or not - a partition wall, a cabinet, a board.
[<SymbolicExpansion>]
type SlabData =
    { Size : Vector3 // its length along the way it's turned, its height and its depth
      Surface : SurfaceType
      Tint : string
      Solid : bool }

/// What a scene sets of an appearance composed of parts, where not as the appearance has it - colors for its color
/// slots by the palette's names, its lights' settings, how many copies it arranges and in how many columns, and how
/// much it's stretched.
[<SymbolicExpansion>]
type ComposedSettings =
    { Colors : Map<string, string> option
      Brightness : single option
      Reach : single option
      Shadowed : bool option
      Count : int option // how many of its arranged copies there are, as chairs along a rail
      Columns : int option // how many columns its copies arranged in a grid have
      Stretch : Vector3 option } // how much it's stretched along each axis - its pieces' positions, the sizes of its parts that stretch, and its colliders

/// How a prop appears - a slab, a variant, or composed of parts - each with its own attributes.
type Appearance =
    | Slab of SlabData
    | Variant of string // a variant of an appearance, by its name in an appearances file
    | Composed of string * ComposedSettings // an appearance an appearances file composes of parts, by its name, and what's set of it

/// How a part of a composed appearance reaches out to what's about the prop - down to the floor beneath it, up to a
/// height above the floor, or back to the wall behind it (leaving the part out where there's no wall). The part reaches
/// from its own top, bottom or front face, and is meant to be unturned.
type PartReach =
    | DownToFloor
    | UpToHeight of single
    | BackToWall

/// A primitive part of a composed appearance, in the appearance's own frame - its origin at the appearance's feet, its
/// front facing +z.
[<SymbolicExpansion>]
type PartData =
    { Surface : SurfaceType
      Shape : PartShapeType
      At : Vector3
      Size : Vector3
      Turn : Vector3 // yaw, pitch and roll, in degrees
      Color : string // a color slot - set by the scene, or the appearance's own color, or a color of the scene's palette by that name
      Jitter : single // how much its shade strays by chance
      Flat : bool // casting no shadow, as a thin sign needn't - never a floor, which must
      Standing : bool // standing on its position, rather than centered on it, so it stays on the ground however it's sized
      Stretches : bool // sized by the appearance's stretch, as well as placed by it
      FromFloor : bool // placed at a height above the floor, rather than above the prop's origin
      Reaches : PartReach option
      Glow : single option // how brightly a part of light glows, where the surface's own glow won't do
      Powered : bool // a glowing part's glow following the power - dim and flickering until it's restored - drawn every frame rather than frozen, so only where it's needed
      Swings : bool } // a door's leaf, or what rolls up of a shutter, which opens as the door does when it's gone through

/// A light of a composed appearance.
[<SymbolicExpansion>]
type ShineData =
    { At : Vector3
      Color : string
      Brightness : single
      Reach : single
      Shadowed : bool
      Powered : bool } // dim and flickering until the power's restored

/// How copies of a composed appearance's pieces are arranged - in a row along x a spacing apart, around a circle of a
/// radius from an angle in degrees turned from east toward south, tiled in columns a spacing apart, or filling the
/// appearance's stretched length along x, as many as fit about a spacing apart (an appearance meant to be stretched
/// being a meter long).
type Arrangement =
    | Row of single
    | Around of single * single
    | Tiled of int * single
    | Fill of single

/// How several copies of a composed appearance's pieces are each placed - arranged or not, a little apart, sized and
/// turned by chance - or one copy, if only sized or turned by chance.
[<SymbolicExpansion>]
type CopiesData =
    { Count : int option // one by default - or as many as the scene sets, if they're arranged
      Extra : int // up to this many more, by chance
      Arrange : Arrangement option
      Spread : Vector3 // how far each strays from where it's arranged, either way, along each axis
      Grow : Vector2 option // the range of each one's size
      Vary : Vector3 // how much each one's parts' sizes stray by chance along each axis, as a share of them, either way
      Spin : bool // turned all the way round by chance
      Tilt : Vector3 } // how far, in degrees, each tilts by chance, either way - yaw, pitch and roll

/// A piece of a composed appearance.
and Piece =
    | Part of PartData
    | SolidBox of Vector2 * Vector2 // a box walkers can't pass - its middle and half extents on the ground
    | SolidCircle of Vector2 * single // a circle walkers can't pass - its middle and radius
    | Shine of ShineData
    | Copies of CopiesData * Piece list
    | Chance of single * Piece list // pieces there by a chance

/// Converts SlotColor types - a color of the appearance's own written as its components, or a palette color written as its
/// name.
type SlotColorConverter () =
    inherit TypeConverter ()

    override this.CanConvertTo (_, destType) =
        destType = typeof<Symbol> ||
        destType = typeof<SlotColor>

    override this.ConvertTo (_, _, source, destType) =
        if destType = typeof<Symbol> then
            match source :?> SlotColor with
            | OwnColor color -> ColorConverter().ConvertTo (color, typeof<Symbol>)
            | PaletteColor name -> Atom (name, ValueNone) :> obj
        elif destType = typeof<SlotColor> then source
        else failconv "Invalid SlotColorConverter conversion to source." None

    override this.CanConvertFrom (_, sourceType) =
        sourceType = typeof<Symbol> ||
        sourceType = typeof<SlotColor>

    override this.ConvertFrom (_, _, source) =
        match source with
        | :? Symbol as symbol ->
            match symbol with
            | Atom (name, _) when not (name.StartsWith "#") -> PaletteColor name :> obj
            | _ -> OwnColor (ColorConverter().ConvertFrom symbol :?> Color) :> obj
        | :? SlotColor -> source
        | _ -> failconv "Invalid SlotColorConverter conversion from source." None

/// The color a composed appearance gives one of its slots - its own, or one of the scene's palette by name.
and [<TypeConverter (typeof<SlotColorConverter>)>] SlotColor =
    | OwnColor of Color
    | PaletteColor of string

/// An appearance composed of parts in an appearances file - its colors by slot, its own or the palette's, how far it
/// reaches behind its origin and before it (what sets it out from a wall, and where a prop before it stands), how high
/// it stands where its placement doesn't say, and its pieces.
[<SymbolicExpansion>]
type ComposedData =
    { Colors : Map<string, SlotColor>
      Depth : Vector2
      Height : single
      Pieces : Piece list }

/// A statement of an appearances file - an appearance composed of parts, a named variant of an appearance with its
/// attributes set as a look of it, or colors every scene can name.
type AppearancesStatement =
    | Compose of string * ComposedData
    | Define of string * Appearance
    | SharedPalette of Map<string, Color>

/// What acting on something is called, as the prompt shows it.
type InteractionType =
    | Examine
    | Open
    | Take
    | Use
    | Drive

/// Interacting with a prop - what the prompt calls it, and the cue run when it is, as trying a door or picking up what
/// lies about.
[<SymbolicExpansion>]
type InteractData =
    { InteractionType : InteractionType
      Cue : Cue }

/// A prop of a scene - anything placed in an area - its name, how it appears, if it's seen at all, as a pickup drawn as
/// its item isn't, where it goes, the spot it's interacted with at - the middle of the thing interacted with, from its
/// origin as it faces, its origin by default - and its interaction, if it has one.
[<SymbolicExpansion>]
type PropData =
    { Name : string
      Appearance : Appearance option
      Place : Placement
      Spot : Vector2
      Interact : InteractData option }

/// A chamber - a floor walled all round to the ceiling, a band of darker paint along the walls' feet, and the ceiling
/// over it, unless it's outdoors.
[<SymbolicExpansion>]
type ChamberData =
    { Size : Vector2 // width and depth, in meters
      Floor : string // palette names
      Walls : string // the ceiling's too
      Dado : string
      FloorSurface : SurfaceType option // linoleum by default
      WallSurface : SurfaceType option // paint by default, the ceiling's too
      Outdoors : bool } // open to the sky, with no ceiling

/// What a scene stands on.
type Layout =
    | Chamber of ChamberData

/// A song an area plays, as its scene gives it.
type SongType =
    | HospitalSong
    | HospitalPoweredSong
    | SafeRoomSong
    | BossSong
    | DormSong
    | DormBossSong
    | PlantSong
    | PlantBossSong
    | FinalBossSong
    | FinalBossRageSong

    /// The song itself.
    member this.Song =
        match this with
        | HospitalSong -> Assets.Gameplay.HospitalSong
        | HospitalPoweredSong -> Assets.Gameplay.HospitalPoweredSong
        | SafeRoomSong -> Assets.Gameplay.SafeRoomSong
        | BossSong -> Assets.Gameplay.BossSong
        | DormSong -> Assets.Gameplay.DormSong
        | DormBossSong -> Assets.Gameplay.DormBossSong
        | PlantSong -> Assets.Gameplay.PlantSong
        | PlantBossSong -> Assets.Gameplay.PlantBossSong
        | FinalBossSong -> Assets.Gameplay.FinalBossSong
        | FinalBossRageSong -> Assets.Gameplay.FinalBossRageSong

/// A song an area plays - the song, and the condition it plays only while, if any.
[<SymbolicExpansion>]
type SongData =
    { Song : SongType
      When : CueCondition option }

/// What makes a scene an area - which area it is, what it's called, how bright its light probe's ambient light is,
/// where the probe stands and the bounds it covers, and its songs, the first whose condition holds playing - the last
/// with none.
[<SymbolicExpansion>]
type AreaData =
    { Type : AreaType
      Name : string
      Ambient : single
      Probe : Vector3
      Bounds : Box3
      Songs : SongData list }

/// A named spot of an area where one arrives other than through a door, as at the start, and the way one faces there,
/// in degrees.
[<SymbolicExpansion>]
type ArrivalData =
    { Name : string
      At : Vector2
      Facing : single }

/// The fixed eye watching a stretch of an area, as the old games' set angles - the stretch of floor it watches, as its
/// least corner and size, where the eye stands, what it looks at, and how wide it sees, in degrees. The player's
/// standing in a stretch cuts to its eye.
[<SymbolicExpansion>]
type CameraData =
    { Zone : Box2
      At : Vector3
      LookAt : Vector3
      FieldOfView : single }

/// A creature placed in a scene - its name, unique in the game, what kind of creature it is, where it stands and
/// faces, and whether it starts lying on the floor as though dead.
[<SymbolicExpansion>]
type EnemyData =
    { Name : string
      Creature : CreatureType
      Place : Placement
      Lying : bool }

/// A statement of a scene file.
type SceneStatement =
    | Palette of Map<string, Color> // the scene's colors, by name
    | Layout of Layout
    | Prop of PropData
    | Enemy of EnemyData
    | Include of string // every statement of another scene but its Area, by name, as if written here - a scene built on another
    | Area of AreaData // what makes the scene an area
    | Arrival of ArrivalData
    | Camera of CameraData
    | While of CueCondition * SceneStatement list // props and enemies there only while a condition holds, as the generator's lamp turning green or the dogs once the power's back

/// A scene - a scene file's statements, in order.
type SceneData =
    { SceneName : string
      Statements : SceneStatement list }