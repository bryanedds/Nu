// Zombie Game.
// Copyright (C) Bryan Edds.

namespace ZombieGame.Tests
open System
open System.Numerics
open NUnit.Framework
open Prime
open Nu
open ZombieGame

/// A bot played by the tests against the game running headless - setting the player down at the spots it acts at,
/// as a shortcut for walking, but doing everything else through the player's intent, frame by frame: fighting,
/// opening doors, reading, taking things, using the inventory, the item box, the keypad and the typewriter.
type Bot =
    { World : World
      mutable Shots : Map<ItemType, int> // rounds or slashes spent, by weapon
      mutable HurtTaken : int
      mutable Healed : int
      mutable Log : string list }

[<RequireQualifiedAccess>]
module Bot =

    let make world =
        { World = world; Shots = Map.empty; HurtTaken = 0; Healed = 0; Log = [] }

    let private log line (bot : Bot) =
        bot.Log <- line :: bot.Log

    let fail message (bot : Bot) =
        let world = bot.World
        Assert.Fail
            (message + "\nIn " + scstring (Simulants.Gameplay.GetAreaType world) + " at " + string (Simulants.GameplayPlayer.GetPosition world) + ", " +
             string (Simulants.GameplayPlayer.GetHealth world) + " hp, " + scstring (Simulants.Gameplay.GetMode world) + ".\nLog:\n" + String.Join ("\n", bot.Log |> List.truncate 500 |> List.rev))

    let private getArea (bot : Bot) = Simulants.Gameplay.GetAreaType bot.World
    let private getMode (bot : Bot) = Simulants.Gameplay.GetMode bot.World
    let private getInventory (bot : Bot) = Simulants.GameplayPlayer.GetInventory bot.World

    /// Whether the player could stand at a point of the area, clear of everything solid.
    let private canStand (point : Vector2) world =
        Simulants.Gameplay.GetColliders world |> Array.forall (fun collider -> not (Collider.touches Constants.Gameplay.PlayerRadius point collider))

    /// Give an intent for a frame, keeping count of harm taken.
    let step intent (bot : Bot) =
        let world = bot.World
        let before = Simulants.GameplayPlayer.GetHealth world
        intend intent world
        advance world
        let after = Simulants.GameplayPlayer.GetHealth world
        if after < before then
            bot.HurtTaken <- bot.HurtTaken + before - after
            log ("  hurt to " + string after + ", " + scstring (Simulants.GameplayPlayer.GetAction world)) bot
        match getMode bot with
        | Fallen _ -> fail "The player died." bot
        | _ -> ()

    /// Press, then let go - a key down for a frame and up for the next, as a press is.
    let press intent (bot : Bot) =
        step intent bot
        step Intent.empty bot

    let act (bot : Bot) = press { Intent.empty with Act = true } bot

    /// Read through whatever's being read.
    let readThrough (bot : Bot) =
        let mutable pages = 0
        while (match getMode bot with Reading _ -> true | _ -> false) do
            match getMode bot with
            | Reading reading when reading.Page < reading.Lines.Length -> log ("  \"" + reading.Lines[reading.Page] + "\"") bot
            | _ -> ()
            act bot
            pages <- inc pages
            if pages > 40 then fail "Reading never ended." bot

    /// Wait till the player's free to act - a reel, a slash or a turn played out, as a person would wait - since a
    /// press lasts only a moment.
    let private awaitFree (bot : Bot) =
        let world = bot.World
        let start = world.UnscaledTime
        while getMode bot = Exploring && Simulants.GameplayPlayer.GetAction world <> Free do
            step Intent.empty bot
            if since start world > 10.0 then fail ("The player never came free, but stayed " + scstring (Simulants.GameplayPlayer.GetAction world) + ".") bot

    /// Set the player down at a spot facing a way, once it's free to act - the shortcut for walking there.
    let placeAt (spot : Vector2) yaw (bot : Bot) =
        awaitFree bot
        Simulants.GameplayPlayer.SetPosition (v3 spot.X 0.0f spot.Y) bot.World
        Simulants.GameplayPlayer.SetYaw yaw bot.World
        Simulants.GameplayPlayer.SetWalkStartOpt None bot.World

    let private ground (position : Vector3) = Collider.toFloor position

    (* The Inventory *)

    let private findSlot predicate (bot : Bot) =
        (getInventory bot).Slots |> Array.tryFindIndex (function Some slot -> predicate slot.SlotItem | None -> false)

    /// Move a menu's cursor on a grid of columns from where it is to where it's wanted, a press at a time.
    let private moveCursor columns (from : int) (target : int) (bot : Bot) =
        let dx = target % columns - from % columns
        let dy = target / columns - from / columns
        for _ in 1 .. abs dx do press { Intent.empty with Move = v2i (sign dx) 0 } bot
        for _ in 1 .. abs dy do press { Intent.empty with Move = v2i 0 (sign dy) } bot

    let private getMenu (bot : Bot) =
        match getMode bot with
        | Browsing -> Simulants.GameplayInventory.GetInventoryMenu bot.World
        | _ -> fail "The inventory screen isn't up." bot; Unchecked.defaultof<_>

    let private getBoxMenu (bot : Bot) =
        match getMode bot with
        | Boxing -> Simulants.GameplayItemBox.GetBoxMenu bot.World
        | _ -> { BoxSide = Carried; BoxCursor = 0 }

    /// Open the inventory screen, do something to a slot - pick it, then the action at an index - and close it.
    let private useSlot slot (action : InventoryAction) partnerOpt (bot : Bot) =
        while not (getMode bot = Exploring && (match Simulants.GameplayPlayer.GetAction bot.World with Free | Aiming _ -> true | _ -> false)) do
            step Intent.empty bot // waiting out a stagger or a grab
        press { Intent.empty with ToggleInventory = true } bot
        moveCursor Constants.Gui.SlotColumns (getMenu bot).Cursor slot bot
        act bot
        let item = ((getInventory bot).Slots[slot]).Value.SlotItem
        let index = List.findIndex ((=) action) (InventoryAction.forKind Data.Items[item].ItemKind)
        for _ in 1 .. index do press { Intent.empty with Move = v2i 0 1 } bot
        act bot
        match partnerOpt with
        | Some partner ->
            moveCursor Constants.Gui.SlotColumns (getMenu bot).Cursor partner bot
            act bot
        | None -> ()
        press { Intent.empty with ToggleInventory = true } bot
        if getMode bot <> Exploring then fail "The inventory screen didn't close." bot

    /// Take a weapon in hand through the inventory screen.
    let equip weapon (bot : Bot) =
        if (getInventory bot).EquippedOpt <> Some weapon then
            match findSlot ((=) weapon) bot with
            | Some slot ->
                useSlot slot EquipItem None bot
                log ("  equips " + scstring weapon) bot
            | None -> ()

    /// Mix what mixes, through the inventory screen.
    let mix (bot : Bot) =
        let mutable mixed = true
        while mixed do
            mixed <- false
            let antiseptics = (getInventory bot).Slots |> Array.indexed |> Array.choose (fun (i, slot) -> match slot with Some slot when slot.SlotItem = Antiseptic -> Some i | _ -> None)
            let pairOpt =
                match findSlot ((=) Coagulant) bot with
                | Some coagulant when antiseptics.Length > 0 -> Some (antiseptics[0], coagulant)
                | _ when antiseptics.Length > 1 -> Some (antiseptics[0], antiseptics[1])
                | _ -> None
            match pairOpt with
            | Some (first, second) ->
                useSlot first CombineItem (Some second) bot
                log "  mixes medicine" bot
                mixed <- true
            | None -> ()

    let private hasRemedy (bot : Bot) =
        findSlot (fun item -> match Data.Items[item].ItemKind with Remedy _ -> true | _ -> false) bot |> Option.isSome

    /// Heal, if hurt badly enough, with the weakest remedy that does the job.
    let heal (bot : Bot) =
        if Simulants.GameplayPlayer.GetHealth bot.World <= Constants.Gameplay.CautionHitPoints && getMode bot = Exploring then
            mix bot
            let missing = Constants.Gameplay.PlayerHitPoints - Simulants.GameplayPlayer.GetHealth bot.World
            let remedies =
                (getInventory bot).Slots |> Array.indexed |>
                Array.choose (fun (i, slot) -> match slot with Some slot -> (match Data.Items[slot.SlotItem].ItemKind with Remedy amount -> Some (i, slot.SlotItem, amount) | _ -> None) | None -> None) |>
                Array.sortBy (fun (_, _, amount) -> amount)
            match remedies |> Array.tryFind (fun (_, _, amount) -> amount >= missing) |> Option.orElse (Array.tryLast remedies) with
            | Some (slot, item, _) ->
                let before = Simulants.GameplayPlayer.GetHealth bot.World
                useSlot slot UseItem None bot
                bot.Healed <- bot.Healed + Simulants.GameplayPlayer.GetHealth bot.World - before
                log ("  uses " + scstring item + ", " + string before + " -> " + string (Simulants.GameplayPlayer.GetHealth bot.World) + " hp") bot
            | None -> ()

    (* Fighting *)

    let private getCreatures (bot : Bot) =
        getAreaEntities bot.World |> List.filter (fun entity -> entity.Is<CreatureDispatcher> bot.World)

    /// The creatures worth fighting - up, rising, or knocked down to rise again.
    let private getThreats (bot : Bot) =
        getCreatures bot |> List.filter (fun creature ->
            creature.GetUp bot.World || (match creature.GetCreatureState bot.World with Rising _ | Downed _ -> true | _ -> false))

    /// The weapon best for a creature among those carried that can be fired - against a brute, the hardest-hitting that
    /// knocks down; against anything else, the lightest gun, saving the heavy rounds; or else whatever's left to swing.
    let private getWeaponFor (target : Entity) (bot : Bot) =
        let inventory = getInventory bot
        let ready =
            Data.Weapons |> Map.toList |>
            List.filter (fun (item, weapon) -> Inventory.has item inventory && (match weapon.AmmunitionOpt with Some ammunition -> Inventory.has ammunition inventory | None -> false))
        let force (weapon : WeaponData) = weapon.Damage * weapon.Pellets
        let brute = Data.Creatures[target.GetCreatureType bot.World].Behavior = Brute
        let pickOpt =
            if brute then ready |> List.sortByDescending (fun (_, weapon) -> (weapon.KnocksDown, force weapon)) |> List.tryHead
            else ready |> List.sortBy (fun (_, weapon) -> (weapon.KnocksDown, force weapon)) |> List.tryHead
        match pickOpt with
        | Some (item, _) -> item
        | None -> Data.Weapons |> Map.toList |> List.tryFind (fun (item, weapon) -> weapon.AmmunitionOpt.IsNone && Inventory.has item inventory) |> Option.map fst |> Option.defaultValue Knife

    /// Whether a shot from the gun in hand would reach a point - along the line from where the shot springs, the barrel's
    /// side of the player and a little ahead, as the game casts it, so a body with a clear view but a barrel against a
    /// corner doesn't count.
    let private hasClearShot (point : Vector2) (bot : Bot) =
        let world = bot.World
        let player = ground (Simulants.GameplayPlayer.GetPosition world)
        let facing = Math.YawFacing (Simulants.GameplayPlayer.GetYaw world)
        let right = v2 -facing.Y facing.X
        let side =
            match (Simulants.GameplayPlayer / "Puppet").GetMuzzleOpt world with
            | Some muzzle -> right * Vector2.Dot (ground muzzle - player, right)
            | None -> v2Zero
        Simulants.Gameplay.HasLineOfSight (player + side + facing * 0.2f) point world

    /// Whether the player could flee to a point - somewhere to stand, with nothing up near it.
    let private canFlee (point : Vector2) (bot : Bot) =
        canStand point bot.World &&
        getCreatures bot |> List.forall (fun creature -> not (creature.GetUp bot.World) || (ground (creature.GetPosition bot.World) - point).Length () > 1.5f)

    /// Fight every creature that's up until none are - turning toward the nearest, raising the weapon best for it and
    /// firing as it's ready, closing in or walking round what's in the way, running clear when it's too close.
    let rec fight (bot : Bot) =
        let world = bot.World
        let start = world.UnscaledTime
        let mutable detourLeft = true
        let mutable detourEnd = world.UnscaledTime
        while not (List.isEmpty (getThreats bot)) do
            let at = ground (Simulants.GameplayPlayer.GetPosition world)
            let target = getThreats bot |> List.minBy (fun creature -> (not (creature.GetUp world), (ground (creature.GetPosition world) - at).LengthSquared ())) // what's up before what's rising
            let weapon = getWeaponFor target bot
            if (getInventory bot).EquippedOpt <> Some weapon && Simulants.GameplayPlayer.GetAction world = Free && getMode bot = Exploring then equip weapon bot
            let toTarget = ground (target.GetPosition world) - at
            let error = MathF.IEEERemainder (Math.YawOf toTarget - Simulants.GameplayPlayer.GetYaw world, MathF.Tau)
            let autoTurning = (Simulants.GameplayPlayer.GetAimTurnOpt world).IsSome // letting auto-aim turn onto its mark, as a person would
            let turn = if abs error < 0.05f || autoTurning then 0.0f else -(single (sign error))
            let pitch = if Data.Creatures[target.GetCreatureType world].Behavior = Brute then 1.0f elif target.GetLow world then -1.0f else 0.0f
            // shooting from no farther than it aims true - a single shot's line at most nine metres off, and a fan of
            // pellets only where most of them strike a creature's body
            let range = min (Data.Weapons[weapon].Range * 0.85f) (if Data.Weapons[weapon].Pellets > 1 then 0.6f / Constants.Gameplay.PelletSpread else 9.0f)
            let inSight = toTarget.Length () < range && Simulants.Gameplay.HasLineOfSight at (ground (target.GetPosition world)) world
            if Simulants.GameplayPlayer.GetHealth world <= Constants.Gameplay.DangerHitPoints && Simulants.GameplayPlayer.GetAction world = Free && hasRemedy bot then heal bot
            else
                match Simulants.GameplayPlayer.GetAction world with
                | Aiming _ when (getInventory bot).EquippedOpt <> Some weapon || not inSight -> step Intent.empty bot
                | Aiming _ when abs error < 0.06f && not (hasClearShot (ground (target.GetPosition world)) bot) ->
                    step Intent.empty bot // the barrel's against something - lowering it to walk round for a clear shot
                    Simulants.GameplayPlayer.SetYaw (Simulants.GameplayPlayer.GetYaw world + MathF.PI * 0.5f * (if detourLeft then 1.0f else -1.0f)) world
                    detourLeft <- not detourLeft
                    detourEnd <- world.UnscaledTime + GameTime.ofSeconds (2.0 / 3.0)
                | Aiming _ ->
                    if abs error < 0.06f then
                        let shotBefore = Simulants.GameplayPlayer.GetShotTimeOpt world
                        let roundsBefore = Data.Weapons[weapon].AmmunitionOpt |> Option.map (fun ammunition -> Inventory.count ammunition (getInventory bot))
                        step { Intent.empty with Aim = true; Act = true; Forward = pitch; Turn = turn } bot
                        if Simulants.GameplayPlayer.GetShotTimeOpt world <> shotBefore && roundsBefore <> Some 0 then
                            bot.Shots <- Map.add weapon ((Map.tryFind weapon bot.Shots |> Option.defaultValue 0) + 1) bot.Shots
                            log ("  fires " + scstring weapon + " at " + target.Name + " " + string (toTarget.Length ()) + " m off, " + string (target.GetHitPoints world) + " hp left, " + scstring (target.GetCreatureState world) + ", pitch " + string (Simulants.GameplayPlayer.GetAimPitch world) + ", error " + string error) bot
                    else step { Intent.empty with Aim = true; Forward = pitch; Turn = turn } bot
                | Free when weapon = Knife && target.GetUp world && toTarget.Length () < 1.4f && canFlee (at - Vector2.Normalize toTarget * 1.5f) bot ->
                    if abs error < MathF.PI * 0.5f then step { Intent.empty with QuickTurn = true } bot
                    else step { Intent.empty with Forward = 1.0f; Run = true } bot
                | Free when world.UnscaledTime < detourEnd -> step { Intent.empty with Forward = 1.0f } bot
                | Free when weapon <> Knife && target.GetUp world && toTarget.Length () < 2.0f && inSight -> step { Intent.empty with Aim = true } bot // raising aims at it
                | Free when abs error > 0.4f -> step { Intent.empty with Turn = turn } bot
                | Free when not inSight ->
                    let before = Simulants.GameplayPlayer.GetPosition world
                    step { Intent.empty with Forward = 1.0f; Turn = turn } bot
                    if (Simulants.GameplayPlayer.GetPosition world - before).Magnitude < 0.005f then
                        Simulants.GameplayPlayer.SetYaw (Simulants.GameplayPlayer.GetYaw world + MathF.PI * 0.5f * (if detourLeft then 1.0f else -1.0f)) world
                        detourLeft <- not detourLeft
                        detourEnd <- world.UnscaledTime + GameTime.ofSeconds (2.0 / 3.0) // walking round what's in the way for a while
                | Free -> step { Intent.empty with Aim = true } bot
                | Slashing _ -> step { Intent.empty with Aim = true } bot
                | QuickTurning _ | Grabbed _ | Reeling _ | Dying _ -> step Intent.empty bot
            if since start world > 180.0 then fail ("A fight never ended, against " + String.Join (", ", getThreats bot |> List.map (fun creature -> creature.Name + " " + scstring (creature.GetCreatureState world)))) bot
        step Intent.empty bot
        step Intent.empty bot
        if not (List.isEmpty (getThreats bot)) then fight bot

    (* Getting About *)

    /// Go through a door of the area, unlocking it first if need be, and fight whatever's beyond.
    let openDoor (door : Prop) (bot : Bot) =
        let (stand, yaw) = getDoorStand door
        placeAt stand yaw bot
        act bot
        if (match getMode bot with Reading _ -> true | _ -> false) then
            readThrough bot
            act bot // unlocked, try it again
        match getMode bot with
        | Opening _ -> ()
        | _ ->
            let world = bot.World
            let actedOnOpt = Simulants.GameplayPlayer.TryGetInteractable world |> Option.map _.Name
            let present = getAreaEntities world |> Seq.filter (fun entity -> entity.Has<InteractableFacet> world) |> Seq.map (fun entity -> entity.Name + " " + string (entity.GetPosition world)) |> String.concat ", "
            fail (door.PropName + " didn't open (" + present + ") - the player " + scstring (Simulants.GameplayPlayer.GetAction world) + " at " + string (Simulants.GameplayPlayer.GetPosition world) + " facing " + string (Simulants.GameplayPlayer.GetYaw world) + ", acting on " + scstring actedOnOpt + ".") bot
        runUntil (fun world -> Simulants.Gameplay.GetMode world = Exploring) 10.0 "came through the door" bot.World
        log ("enters " + scstring (getArea bot)) bot
        fight bot
        heal bot

    /// Travel on to another stage, once what's set it going has been read, and fight whatever's waiting there.
    let travel target (bot : Bot) =
        let world = bot.World
        match getMode bot with
        | Travelling _ -> ()
        | mode -> fail ("Didn't travel on, but " + scstring mode + ".") bot
        runUntil (fun world -> Simulants.Gameplay.GetMode world = Exploring) (float Constants.Gameplay.TravelDuration + 2.0) "travelled on" world
        if getArea bot <> target then fail ("Travelled to " + scstring (getArea bot) + ", not " + scstring target + ".") bot
        log ("travels to " + scstring target) bot
        fight bot

    /// A route to an area, searching outward from the areas reached so far.
    let rec private searchRoute target frontier visited (bot : Bot) =
        let world = bot.World
        match frontier with
        | [] -> None
        | (area, path) :: _ when area = target -> Some (List.rev path)
        | (area, path) :: rest ->
            let doors =
                getDoors area |>
                List.filter (fun (door, enter) ->
                    not (Set.contains enter.To visited) &&
                    Simulants.Gameplay.EvalConditionOpt enter.Opens world &&
                    (match enter.Key with Some key -> Simulants.Gameplay.HasTranspired (Unlocked door.PropName) world || Inventory.has key (getInventory bot) | None -> true))
            searchRoute target (rest @ (doors |> List.map (fun (door, enter) -> (enter.To, door :: path)))) (Set.union visited (doors |> List.map (fun (_, enter) -> enter.To) |> Set.ofList)) bot

    /// The doors to go through from the area the player's in to another, through doors that open now.
    let private tryGetRoute target (bot : Bot) =
        searchRoute target [(getArea bot, [])] (Set.singleton (getArea bot)) bot

    /// Go to an area by way of whatever areas lie between, fighting through each.
    let goTo target (bot : Bot) =
        match tryGetRoute target bot with
        | Some route -> for door in route do openDoor door bot
        | None -> fail ("No way to " + scstring target + ".") bot

    let private findProp name (bot : Bot) =
        match getAreaEntities bot.World |> List.tryFind (fun entity -> entity.Name = name) with
        | Some prop -> prop
        | None -> fail ("No " + name + " in " + scstring (getArea bot) + ".") bot; Unchecked.defaultof<_>

    /// Set the player down somewhere it can stand and act on a prop, facing its middle across its body.
    let private approach (prop : Entity) (bot : Bot) =
        let world = bot.World
        let at = ground (prop.GetPosition world)
        let reach = prop.GetReach world
        let spots =
            [for distance in [0.0f; 0.4f; 0.55f; 0.7f; 0.85f; 1.0f] do
                for i in 0 .. 23 do
                    let angle = single i / 24.0f * MathF.Tau
                    at + v2 (sin angle) (cos angle) * (distance + reach)]
        let reached =
            spots |> List.exists (fun spot ->
                canStand spot world &&
                (placeAt spot (Math.YawOf (at - spot + v2 0.0f 0.0001f)) bot
                 Simulants.GameplayPlayer.TryGetInteractable world = Some prop))
        if not reached then fail ("Can't reach " + prop.Name + ".") bot

    /// Examine a feature of the area, standing at it, and read what it says.
    let examine name (bot : Bot) =
        approach (findProp name bot) bot
        log ("examines " + name) bot
        act bot
        readThrough bot
        match getMode bot with Exploring -> fight bot | _ -> () // a menu it opened halts the fight till it's done with it

    /// Put away all but what's kept in the item box, the player at it.
    let private stow keep (bot : Bot) =
        examine "ItemBox" bot
        match getMode bot with
        | Boxing -> ()
        | _ -> fail "The item box didn't open." bot
        for slot in 0 .. dec Constants.Gameplay.SlotCount do
            match (getInventory bot).Slots[slot] with
            | Some item when not (keep item.SlotItem) ->
                let cursor = (getBoxMenu bot).BoxCursor
                moveCursor 1 cursor slot bot
                act bot
                log ("  boxes " + scstring item.SlotItem) bot
            | _ -> ()
        press { Intent.empty with Back = true } bot

    let private isKept item =
        match Data.Items[item].ItemKind with
        | KeyItem | Ammunition | Remedy _ | Ingredient -> true
        | Weapon _ -> item <> Knife
        | Ribbon -> false

    /// The safe room of the stage the player's in - its area with the item box.
    let private getSafeRoom (bot : Bot) =
        let stage = (getArea bot).Name.Split('+')[0]
        Data.Areas.Values |>
        Seq.find (fun area -> area.AreaType.Name.Split('+')[0] = stage && area.Props |> List.exists (fun prop -> prop.PropName = "ItemBox")) |>
        _.AreaType

    /// Make room by going to the stage's safe room and boxing what's not kept - then, if that's not enough, the gun with
    /// the fewest rounds carried for it and its rounds, and then all but one remedy - then come back.
    let makeArea (bot : Bot) =
        let back = getArea bot
        log "goes to make room" bot
        goTo (getSafeRoom bot) bot
        heal bot
        stow isKept bot
        if not (Array.exists Option.isNone (getInventory bot).Slots) then
            let guns =
                Data.Weapons |> Map.toList |>
                List.choose (fun (item, weapon) ->
                    match weapon.AmmunitionOpt with
                    | Some ammunition when Inventory.has item (getInventory bot) -> Some (item, ammunition, Inventory.count ammunition (getInventory bot))
                    | Some _ | None -> None) |>
                List.sortBy (fun (_, _, rounds) -> rounds)
            match guns with
            | (scarcest, ammunition, _) :: others when not (List.isEmpty others) ->
                if (getInventory bot).EquippedOpt = Some scarcest then (let (other, _, _) = List.last others in equip other bot)
                stow (fun item -> item <> scarcest && item <> ammunition) bot
            | _ -> ()
        if not (Array.exists Option.isNone (getInventory bot).Slots) then
            let remedyKept = ref false
            stow (fun item ->
                match Data.Items[item].ItemKind with
                | Remedy _ when not remedyKept.Value -> remedyKept.Value <- true; true
                | Remedy _ | Ingredient -> false
                | _ -> isKept item) bot
        goTo back bot

    /// Make room for one more thing, if every slot's full.
    let ensureRoom (bot : Bot) =
        if not (Array.exists Option.isNone (getInventory bot).Slots) then makeArea bot

    /// Take a pickup of the area by its name, standing where it's in reach, leaving whatever it stirs for after.
    let snatch name (bot : Bot) =
        let world = bot.World
        let pickup = findProp name bot
        let item = pickup.GetItem world
        if not (Inventory.hasRoomFor item (getInventory bot)) then makeArea bot
        if not (Inventory.hasRoomFor item (getInventory bot)) then log ("leaves " + name + ", having no room for it") bot else
        approach (findProp name bot) bot
        act bot
        log ("takes " + name + " (" + scstring item + ")") bot
        readThrough bot

    /// Take a pickup of the area by its name, and fight whatever it stirs.
    let take name (bot : Bot) =
        snatch name bot
        fight bot

    /// Run for a door of the area, by the player's intent - turning toward it and running, a grab or a stagger
    /// waited out - then go through it.
    let runFor target (bot : Bot) =
        let world = bot.World
        let (door, _) = getDoors (getArea bot) |> List.find (fun (_, enter) -> enter.To = target)
        let (stand, _) = getDoorStand door
        let start = world.UnscaledTime
        log ("runs for " + door.PropName) bot
        while (stand - ground (Simulants.GameplayPlayer.GetPosition world)).Length () > 0.3f do
            let error = MathF.IEEERemainder (Math.YawOf (stand - ground (Simulants.GameplayPlayer.GetPosition world)) - Simulants.GameplayPlayer.GetYaw world, MathF.Tau)
            let turn = if abs error < 0.05f then 0.0f else -(single (sign error))
            step { Intent.empty with Turn = turn; Forward = (if abs error < 0.6f then 1.0f else 0.0f); Run = true } bot
            if since start world > 20.0 then fail ("Couldn't run for " + door.PropName + ".") bot
        while Simulants.GameplayPlayer.GetAction world <> Free do step Intent.empty bot
        openDoor door bot

    /// Enter a code on the keypad a prop of the area asks it at, with room for anything it gives.
    let enterCode name (code : string) (bot : Bot) =
        ensureRoom bot
        examine name bot
        for digit in code + "K" do
            let target = if digit = 'K' then Array.IndexOf (Constants.Gui.KeypadKeys, "OK") else Array.IndexOf (Constants.Gui.KeypadKeys, string digit)
            let key = match getMode bot with Keypadding _ -> (Simulants.GameplayKeypad.GetKeypad bot.World).Key | _ -> fail "The keypad isn't up." bot; 0
            moveCursor 3 key target bot
            act bot
        log ("  enters " + code) bot
        readThrough bot
        fight bot

    /// Take an item out of the item box, the box open.
    let private retrieveBoxed item (bot : Bot) =
        match Inventory.getBoxed (getInventory bot) |> List.tryFindIndex (fun (boxed, _) -> boxed = item) with
        | Some index ->
            if (getBoxMenu bot).BoxSide = Carried then press { Intent.empty with Move = v2i 1 0 } bot
            moveCursor 1 (getBoxMenu bot).BoxCursor index bot
            act bot
        | None -> ()

    /// Save at the nurses' station's typewriter - fetching a ribbon from the item box if none's carried.
    let save (bot : Bot) =
        let world = bot.World
        let mutable lentOpt = None
        if not (Inventory.has InkRibbon (getInventory bot)) then
            examine "ItemBox" bot
            // no room for a ribbon, so box something for a moment - the least needed thing carried
            if not (Array.exists Option.isNone (getInventory bot).Slots) then
                let (slot, item) =
                    (getInventory bot).Slots |> Array.indexed |>
                    Array.choose (fun (i, slot) -> slot |> Option.map (fun slot -> (i, slot.SlotItem))) |>
                    Array.filter (fun (_, item) -> (getInventory bot).EquippedOpt <> Some item) |>
                    Array.minBy (fun (_, item) -> match Data.Items[item].ItemKind with Ingredient -> 0 | Remedy _ -> 1 | KeyItem -> 2 | Ammunition -> 3 | Weapon _ -> 4 | Ribbon -> 5)
                moveCursor 1 (getBoxMenu bot).BoxCursor slot bot
                act bot
                lentOpt <- Some item
            retrieveBoxed InkRibbon bot
            press { Intent.empty with Back = true } bot
        let saves = Simulants.Gameplay.GetSaveCount world
        examine "Typewriter" bot
        match getMode bot with
        | Prompting -> act bot
        | _ -> fail "The typewriter didn't offer to save." bot
        if Simulants.Gameplay.GetSaveCount world <> inc saves then fail "The typewriter didn't save." bot
        log "  saves" bot
        match lentOpt with
        | Some item ->
            examine "ItemBox" bot
            retrieveBoxed item bot
            press { Intent.empty with Back = true } bot
        | None -> ()
