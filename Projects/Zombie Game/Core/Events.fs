// Zombie Game.
// Copyright (C) Bryan Edds.

namespace ZombieGame
open System
open System.Numerics
open Prime
open Nu

/// The events the game's screens and entities raise to one another.
[<RequireQualifiedAccess>]
module Events =

    (* Play *)

    /// Raised on what the player faces when it acts on it.
    let InteractEvent = stoa<unit> "Interact/Event"

    /// Raised on a creature when it strikes a blow, carrying the blow.
    let AttackEvent = stoa<Attack> "Attack/Event"

    /// Raised on the player when its shot or slash lands, carrying what it hit.
    let HitEvent = stoa<Hit> "Hit/Event"

    /// Raised on the player when its gun goes off.
    let ShotEvent = stoa<unit> "Shot/Event"

    (* Screens Over Play *)

    /// Raised by a screen over play - the inventory, item box, keypad or save prompt - when it's closed.
    let CloseEvent = stoa<unit> "Close/Event"

    /// Raised by the inventory when an item is used, carrying the item.
    let UseItemEvent = stoa<ItemType> "UseItem/Event"

    /// Raised by the inventory when an item is equipped, carrying the item.
    let EquipItemEvent = stoa<ItemType> "EquipItem/Event"

    /// Raised by the inventory when two items are combined, carrying their slots.
    let CombineItemsEvent = stoa<int * int> "CombineItems/Event"

    /// Raised by the item box when an item is put away, carrying its slot.
    let StoreEvent = stoa<int> "Store/Event"

    /// Raised by the item box when an item is taken out, carrying the item.
    let RetrieveEvent = stoa<ItemType> "Retrieve/Event"

    /// Raised by the keypad when a code is entered, carrying the code.
    let CodeEvent = stoa<string> "Code/Event"

    /// Raised by the save prompt when the player chooses to save.
    let SaveEvent = stoa<unit> "Save/Event"

    /// Raised by what's being read when its page is turned.
    let PageEvent = stoa<unit> "Page/Event"

    (* The Gameplay Screen *)

    /// Raised by the gameplay screen when the player has fallen and the game's over.
    let FallenEvent = stoa<unit> "Fallen/Event"

    /// Raised by the gameplay screen when the player has escaped, carrying the play time the escape took.
    let EscapedEvent = stoa<single> "Escaped/Event"

    (* Menus *)

    /// Raised by the title menu when a choice is made, carrying the choice.
    let ChooseEvent = stoa<MenuChoice> "Choose/Event"

    /// Raised by the credits when they've rolled through or been skipped.
    let FinishedEvent = stoa<unit> "Finished/Event"