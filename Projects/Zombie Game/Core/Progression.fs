// Zombie Game.
// Copyright (C) Bryan Edds.

namespace ZombieGame
open System
open System.IO
open System.Numerics
open Prime
open Nu

/// What the player means to do this frame - read from the keyboard, or given by a test in its place. The presses are
/// true only on the frame the key goes down.
type Intent =
    { Forward : single // 1 walks forward, -1 backs up
      Turn : single // 1 turns right, -1 left
      Run : bool
      Aim : bool
      Act : bool // pressed - examine, open, take, fire, page on, confirm
      QuickTurn : bool // pressed - spin round
      AutoAim : bool // pressed - aim at the next creature in sight, a weapon raised
      ToggleInventory : bool // pressed
      Back : bool // pressed - back out of a menu
      Move : Vector2i } // pressed - a step through a menu, x across and y down

    /// An axis of movement from two keys held - one each way.
    static let getAxis positive negative =
        (if positive then 1.0f else 0.0f) -
        (if negative then 1.0f else 0.0f)

    /// An intent of nothing, as the player does nothing.
    static member empty =
        { Forward = 0.0f
          Turn = 0.0f
          Run = false
          Aim = false
          Act = false
          QuickTurn = false
          AutoAim = false
          ToggleInventory = false
          Back = false
          Move = v2iZero }

    /// What the keyboard says the player means - W and S walk, A and D turn, shift runs, backslash aims, right bracket
    /// auto-aims, space or enter acts, tab opens the inventory, escape backs out, and S with shift spins round. WASD
    /// moves a choice.
    static member ofKeyboard world =
        { Forward = getAxis (World.isKeyboardKeyDown KeyboardKey.W world) (World.isKeyboardKeyDown KeyboardKey.S world)
          Turn = getAxis (World.isKeyboardKeyDown KeyboardKey.D world) (World.isKeyboardKeyDown KeyboardKey.A world)
          Run = World.isKeyboardShiftDown world
          Aim = World.isKeyboardKeyDown KeyboardKey.Backslash world
          Act = World.isKeyboardKeyPressed KeyboardKey.Space world || World.isKeyboardKeyPressed KeyboardKey.Enter world
          QuickTurn =
            World.isKeyboardShiftDown world && World.isKeyboardKeyPressed KeyboardKey.S world ||
            World.isKeyboardKeyDown KeyboardKey.S world && World.isKeyboardKeyPressed KeyboardKey.RShift world
          AutoAim = World.isKeyboardKeyPressed KeyboardKey.RightBracket world
          ToggleInventory = World.isKeyboardKeyPressed KeyboardKey.Tab world
          Back = World.isKeyboardKeyPressed KeyboardKey.Escape world
          Move =
            v2i
                ((if World.isKeyboardKeyPressed KeyboardKey.D world then 1 else 0) - (if World.isKeyboardKeyPressed KeyboardKey.A world then 1 else 0))
                ((if World.isKeyboardKeyPressed KeyboardKey.S world then 1 else 0) - (if World.isKeyboardKeyPressed KeyboardKey.W world then 1 else 0)) }

/// How the player's health stands, as the status screen shows it.
type Health =
    | Fine
    | Caution
    | Danger

/// What the player is doing.
type PlayerAction =
    | Free // walking, turning or standing
    | Aiming of GameTime // the weapon raised since this time
    | Slashing of GameTime // slashing with the knife since this time
    | Grabbed of Entity * GameTime // seized by this creature since this time
    | Reeling of GameTime // staggered by a blow, or freed of a grab, since this time
    | QuickTurning of GameTime * single // spinning round since this time, from this yaw
    | Dying of GameTime // fallen since this time

/// A creature's state.
type CreatureState =
    | Dormant // lying on the floor as though dead, until the player comes near
    | Rising of GameTime // heaving itself up off the floor since this time
    | Waiting // on its feet, unaware of the player
    | Chasing // coming for the player
    | Grabbing of GameTime // lunged at the player since this time, biting if it caught it
    | Swiping of GameTime // swinging at the player since this time
    | Crouching of GameTime // gathering to spring since this time
    | Leaping of GameTime * Vector2 * Vector2 // flying since this time, from one point toward another on the floor
    | Staggered of GameTime // staggered by a hit since this time
    | Downed of GameTime // knocked to the floor since this time, to heave back up
    | Dead of GameTime // fallen dead since this time

/// A blow a creature lands on the player - its harm, whether it seizes it, as a zombie's grab does, and who landed it.
type Attack =
    { AttackDamage : int
      Seizes : bool
      Attacker : Entity }

/// A shot or slash the player lands on a creature - its harm, whether it knocks it down, whether it's a shot to the head
/// or one aimed down, and the creature it lands on.
type Hit =
    { HitDamage : int
      KnocksDown : bool
      HeadShot : bool // a shot fired aimed up, to the head of what's standing
      LowShot : bool // a shot or slash aimed down, as at what's on the floor or a dog getting up off it
      Struck : Entity }

/// A door swinging open in the dark - since when, the door's prop, as it stands in the area it's opened from, where it
/// leads, where one arrives there, how it opens, and whether the next area's been come into yet, as it is halfway
/// through.
type Opening =
    { OpeningStart : GameTime
      Door : Prop
      Destination : AreaType
      Arrival : Vector3
      ArrivalYaw : single
      Style : DoorStyle
      Entered : bool }

/// Travelling on to another stage in the dark - since when, the area and where in it one arrives, the caption shown on
/// the way, and whether the area's been come into yet, as it is halfway through.
type Travelling =
    { TravelStart : GameTime
      TravelDestination : AreaType
      TravelArrival : Vector3
      TravelArrivalYaw : single
      Caption : string
      Arrived : bool }

/// Lines being read, a page at a time, the cue that goes on once they're read, and the name of the prop interacted with
/// that it goes on for, if it goes on for one.
type Reading =
    { Lines : string list
      Page : int
      After : Cue
      ActedOnOpt : string option }

/// What's being chosen on the inventory screen - a slot, an action for the slot chosen, a slot to combine it with, or
/// nothing, its description showing.
type InventoryChoice =
    | ChoosingSlot
    | ChoosingAction of int // the action highlighted for the slot chosen
    | ChoosingPartner // the slot to combine the chosen one with
    | Describing // the chosen slot's description shown

/// Where the inventory screen stands - the slot highlighted, the slot chosen, and what's being chosen.
type InventoryMenu =
    { Cursor : int
      Chosen : int
      Choice : InventoryChoice }

/// What can be done with an item on the inventory screen.
type InventoryAction =
    | UseItem // a remedy taken
    | EquipItem // a weapon taken in hand, or put away
    | CombineItem
    | ExamineItem

    /// What can be done on the inventory screen with an item of a kind.
    static member forKind kind =
        match kind with
        | Weapon _ -> [EquipItem; CombineItem; ExamineItem]
        | Remedy _ -> [UseItem; CombineItem; ExamineItem]
        | Ammunition | Ingredient | Ribbon | KeyItem -> [CombineItem; ExamineItem]

/// A side of the item box screen - what's carried, or what's boxed.
type BoxSide =
    | Carried
    | Boxed

/// Where the item box screen stands - the side and the line highlighted.
type BoxMenu =
    { BoxSide : BoxSide
      BoxCursor : int }

/// Where a keypad screen stands - the digits entered so far and the key highlighted.
type Keypad =
    { Entered : string
      Key : int }

/// What's going on in play - exploring, or a screen up over play that holds it still, or a door opening, or
/// the end of it one way or the other.
type Mode =
    | Exploring
    | Reading of Reading
    | Opening of Opening
    | Travelling of Travelling
    | Browsing // the inventory up
    | Boxing // the item box open
    | Prompting // the typewriter asking whether to save
    | Keypadding of KeypadData * string option // a keypad up - its code and what follows it, for the prop interacted with, if any
    | Fallen of GameTime // the player dead since this time
    | Escaped of GameTime // play won since this time

/// A choice of the title, game over and ending menus.
type MenuChoice =
    | NewGameChoice
    | ContinueChoice
    | QuitChoice
    | TitleChoice