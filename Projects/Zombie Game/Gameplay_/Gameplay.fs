// Zombie Game.
// Copyright (C) Bryan Edds.

namespace ZombieGame
open System
open System.Numerics
open Prime
open Nu

/// The gameplay screen's state, the events it raises for the game, and starting play on it.
[<AutoOpen>]
module GameplayDispatcherExtensions =
    type Screen with
        member this.GetAdvents world : Advent Set = this.Get (nameof this.Advents) world
        member this.SetAdvents (value : Advent Set) world = this.Set (nameof this.Advents) value world
        member this.Advents = lens (nameof this.Advents) this this.GetAdvents this.SetAdvents
        member this.GetSaveCount world : int = this.Get (nameof this.SaveCount) world
        member this.SetSaveCount (value : int) world = this.Set (nameof this.SaveCount) value world
        member this.SaveCount = lens (nameof this.SaveCount) this this.GetSaveCount this.SetSaveCount
        member this.GetIntent world : Intent = this.Get (nameof this.Intent) world
        member this.SetIntent (value : Intent) world = this.Set (nameof this.Intent) value world
        member this.Intent = lens (nameof this.Intent) this this.GetIntent this.SetIntent
        member this.GetIntentOverrideOpt world : Intent option = this.Get (nameof this.IntentOverrideOpt) world
        member this.SetIntentOverrideOpt (value : Intent option) world = this.Set (nameof this.IntentOverrideOpt) value world
        member this.IntentOverrideOpt = lens (nameof this.IntentOverrideOpt) this this.GetIntentOverrideOpt this.SetIntentOverrideOpt
        member this.GetPendingStartOpt world : Save option = this.Get (nameof this.PendingStartOpt) world
        member this.SetPendingStartOpt (value : Save option) world = this.Set (nameof this.PendingStartOpt) value world
        member this.PendingStartOpt = lens (nameof this.PendingStartOpt) this this.GetPendingStartOpt this.SetPendingStartOpt
        member this.FallenEvent = Events.FallenEvent --> this
        member this.EscapedEvent = Events.EscapedEvent --> this

        (* Advents *)

        /// Whether something's been done.
        member this.HasTranspired advent world =
            Set.contains advent (this.GetAdvents world)

        /// Mark something done.
        member this.MarkTranspired advent world =
            this.Advents.Map (Set.add advent) world

        /// Whether a condition holds of play as it stands.
        member this.EvalCondition condition world =
            let player = Simulants.GameplayPlayer
            CueCondition.holdsBy (fun advent -> this.HasTranspired advent world) (fun item -> player.Carries item world) condition

        /// Whether a condition holds of play as it stands, if there is one.
        member this.EvalConditionOpt conditionOpt world =
            match conditionOpt with
            | Some condition -> this.EvalCondition condition world
            | None -> true

        (* Starting *)

        /// Start play afresh or from a save, the start applied once the player's there to take it.
        member this.Start (start : Save) world =
            this.SetMode Exploring world
            this.SetAreaType start.SaveArea world
            this.SetPendingStartOpt (Some start) world

/// Play on a stage - the area the player's in declared each frame, the player and the gui kept in the scene group from
/// area to area, the screens over play, the door opening between areas, and the fixed eyes.
type GameplayDispatcher () =
    inherit ScreenDispatcherImSim ()

    (* Utility *)

    /// The rotation of an eye looking from one point toward another.
    static let lookAt (eye : Vector3) (target : Vector3) =
        let forward = Vector3.Normalize (target - eye)
        Quaternion.CreateFromRotationMatrix (Matrix4x4.CreateLookAt (v3Zero, forward, v3Up)) |> Quaternion.Inverse

    (* Play *)

    /// Set play going from a start - a new game's or a save's - once the player's there to take it.
    static let applyStart (start : Save) (screen : Screen) world =
        let player = Simulants.GameplayPlayer
        screen.SetAreaType start.SaveArea world
        screen.SetAdvents start.SaveFlags world
        screen.SetSaveCount start.SaveCount world
        screen.SetPlayTime start.SavePlayTime world
        screen.SetGameSeed start.SaveGameSeed world
        screen.SetMode Exploring world
        player.SetHealth start.SaveHealth world
        player.SetInventory start.SaveInventory world
        player.SetShotTimeOpt None world
        player.SetFreedTimeOpt None world
        player.SetReeledTimeOpt None world
        player.Enter start.SavePosition start.SaveYaw world

    /// What a save of play now would remember.
    static let snapshot (screen : Screen) world =
        let player = Simulants.GameplayPlayer
        { SaveArea = screen.GetAreaType world
          SavePosition = player.GetPosition world
          SaveYaw = player.GetYaw world
          SaveHealth = player.GetHealth world
          SaveInventory = player.GetInventory world
          SaveFlags = screen.GetAdvents world
          SaveCount = screen.GetSaveCount world
          SavePlayTime = screen.GetPlayTime world
          SaveGameSeed = screen.GetGameSeed world }

    /// How much the power lets something running on it shine at a spot - fully once it's restored, and until then
    /// dimly, stuttering now and again, each light a little out of step with the others.
    static let getPowerScale (position : Vector3) (screen : Screen) world =
        if screen.HasTranspired PowerRestored world then 1.0f
        else
            let phase = world.ScaledTime.SecondsF * Constants.Gameplay.FlickerRate + position.X * 3.1f + position.Z * 1.7f
            let stutter = if sin phase * sin (phase * 2.3f) > Constants.Gameplay.StutterThreshold then Constants.Gameplay.StutterScale else 1.0f
            Constants.Gameplay.UnpoweredScale * stutter

    /// The area's props there only while a condition holds that does, each by its index among such props.
    static let getPresentProps (area : Area) (screen : Screen) world =
        area.Props |>
        List.filter (fun prop -> prop.PropConditionOpt.IsSome) |>
        List.indexed |>
        List.filter (fun (_, prop) -> screen.EvalConditionOpt prop.PropConditionOpt world)

    /// Whether play's lingered on something since a time for a duration, as of this frame - judged in whole game time,
    /// so no frame's rounding lets the moment pass unseen.
    static let hasLingered start (duration : single) (world : World) =
        let age = world.ScaledTime - start
        let duration = GameTime.ofSeconds (double duration)
        age >= duration && age - world.ScaledDelta < duration

    /// The music for where play is - the end's, or the first of the area's songs whose condition holds.
    static let getSong (screen : Screen) world =
        match screen.GetMode world with
        | Fallen _ -> Assets.Gui.GameOverSong
        | Escaped _ -> Assets.Gui.EndingSong
        | _ ->
            let area = Data.Areas[screen.GetAreaType world]
            match area.Songs |> List.tryFind (fun song -> screen.EvalConditionOpt song.When world) with
            | Some song -> song.Song.Song
            | None -> failwith ("No song of " + area.AreaName + " plays as play stands - its last song should have no condition.")

    (* Creatures *)

    /// The creatures of the area the player's in.
    static let getCreatures (screen : Screen) world =
        screen.GetAreaEntities world |> Array.filter (fun creature -> creature.Is<CreatureDispatcher> world)

    /// Rouse the creatures lying dormant in the area, all at once, heaving up off the floor.
    static let rouseCreatures (screen : Screen) world =
        for creature in getCreatures screen world do
            if creature.GetCreatureState world = Dormant then
                creature.SetCreatureState (Rising world.ScaledTime) world

    /// Wake the creatures of the area within hearing of the player's shot.
    static let alertCreatures (screen : Screen) world =
        let player = Simulants.GameplayPlayer
        let point = Collider.toFloor (player.GetPosition world)
        for creature in getCreatures screen world do
            if creature.GetCreatureState world = Waiting && (Collider.toFloor (creature.GetPosition world) - point).Length () < Constants.Gameplay.ShotHearingDistance then
                creature.SetCreatureState Chasing world

    /// Land a creature's blow on the player - striking, seizing or biting it - unless it's reeling or falling, or
    /// still in the grace after a reel, when it's spared, so creatures striking in turn can't wear it down at once. A
    /// creature holding it bites it all the same.
    static let takeAttack (attack : Attack) (screen : Screen) (world : World) =
        let player = Simulants.GameplayPlayer
        let now = world.ScaledTime
        match player.GetAction world with
        | Reeling _ | Dying _ -> ()
        | action when not action.IsGrabbed && player.GetReeledTimeOpt world |> Option.exists (fun reeled -> (world.LocalTime reeled).SecondsF < Puppet.getMarkTime Data.Puppets[Player] AnimationType.Hurt Mark.End + Constants.Gameplay.HurtGrace) -> () // just reeled, untouchable a while
        | action ->
            if attack.Seizes then
                match action with
                | Grabbed _ -> ()
                | _ when player.GetFreedTimeOpt world |> Option.exists (fun freed -> (world.LocalTime freed).SecondsF < Constants.Gameplay.FreedGrace) -> () // just broken free, not seized again at once
                | _ ->
                    player.SetAction (Grabbed (attack.Attacker, now)) world
                    player.SetWalkStartOpt None world
            else
                let hitPoints = max 0 (player.GetHealth world - attack.AttackDamage)
                player.SetHealth hitPoints world
                if hitPoints = 0 then
                    player.SetAction (Dying now) world
                    World.playSound 0.0f 0.0f 0.5f Assets.Gameplay.PlayerDeathSound world
                else
                    World.playSound 0.0f 0.0f 0.5f Assets.Gameplay.PlayerHurtSound world
                    match action with
                    | Grabbed _ -> () // held on to, bitten again
                    | _ ->
                        player.SetAction (Reeling now) world
                        player.SetReeledTimeOpt (Some now) world
                player.SetWalkStartOpt None world

    /// Land the player's hit on a creature - felling it if it kills, to rise once more if it's fated to, else flagged
    /// killed; knocking it down if the weapon knocks down; or else staggering it. A shambler shot dead in the head has its
    /// head burst, killed outright with nothing left to rise. A brute shrugs off a stagger from anything that doesn't
    /// knock it down, and nothing hits what's dead or rising - but a dog getting up is hit by what's aimed down at it,
    /// before it's on its feet. A dog hit lying dormant gets up. A creature hurt past its share for rage is flagged enraged, crying out and gathering itself
    /// to charge, and rousing the dead lying about the area.
    static let takeHit (hit : Hit) (screen : Screen) world =
        let creature = hit.Struck
        let data = Data.Creatures[creature.GetCreatureType world]
        match creature.GetCreatureState world with
        | Dead _ -> ()
        | Rising _ when not (hit.LowShot && data.Behavior = Hound) -> ()
        | state ->
            let now = world.ScaledTime
            let hitPoints = creature.GetHitPoints world - hit.HitDamage
            creature.SetHitPoints hitPoints world
            creature.SetLeapLanded true world
            screen.PlaySoundAt 1.0f (creature.GetPosition world) Assets.Gameplay.FleshHitSound world
            if hitPoints <= 0 then
                if hit.HeadShot && data.Behavior = Shambler then
                    let headCenter = (creature / "Puppet").GetHeadCenterOpt world |> Option.defaultValue (creature.GetPosition world + v3Up * 1.6f)
                    creature.SetHeadBurstOpt (Some (now, headCenter)) world
                    creature.SetCreatureState (Dead now) world
                    screen.MarkTranspired (Killed creature.Name) world
                    screen.PlaySoundAt 1.0f headCenter Assets.Gameplay.HeadBurstSound world
                elif creature.GetWillRise world && not (creature.GetRisen world) then
                    creature.SetCreatureState (Downed now) world
                    creature.SetRisen true world
                    creature.SetHitPoints (data.HitPoints / 2) world
                else
                    creature.SetCreatureState (Dead now) world
                    screen.MarkTranspired (Killed creature.Name) world
                    screen.PlaySoundAtOneOf data.DeathSound.Volume (creature.GetPosition world) data.DeathSound.Sounds world
            elif hit.KnocksDown && data.Behavior <> Brute then creature.SetCreatureState (Downed now) world
            elif hit.KnocksDown || data.Behavior <> Brute then
                match state with
                | Dormant when data.Behavior = Hound -> creature.SetCreatureState (Rising now) world // a dog hit where it lies gets up
                | Downed _ | Dormant | Rising _ -> () // hit where it lies or as it gets up, it carries on as it was
                | _ -> creature.SetCreatureState (Staggered now) world
            if  hitPoints > 0 &&
                single hitPoints <= single data.HitPoints * data.EnrageAt &&
                not (screen.HasTranspired (Enraged creature.Name) world) then
                screen.MarkTranspired (Enraged creature.Name) world
                creature.SetCreatureState (Crouching now) world
                screen.PlaySoundAtOneOf data.CrySound.Volume (creature.GetPosition world) data.CrySound.Sounds world
                rouseCreatures screen world

    (* Reading and Giving *)

    /// Show lines, then go on to the cue that follows them once they're read, for the prop interacted with, if any.
    static let showLines actedOnOpt lines after (screen : Screen) world =
        if List.isEmpty lines then failwith "Reading no lines - a cue with no lines goes straight on, so nothing should ask to read none."
        screen.SetMode (Reading { Lines = lines; Page = 0; After = after; ActedOnOpt = actedOnOpt }) world
        World.playSound 0.0f 0.0f 0.35f Assets.Gameplay.PageSound world

    /// Give the player some of an item, saying so - or that there's no room for it - true if it took it.
    static let giveItem actedOnOpt item count (screen : Screen) world =
        let player = Simulants.GameplayPlayer
        let name = Data.Items[item].ItemName
        match Inventory.add item count (player.GetInventory world) with
        | (inventory, 0) ->
            player.SetInventory inventory world
            World.playSound 0.0f 0.0f 0.2f Assets.Gameplay.ItemPickupSound world
            showLines actedOnOpt [(if count > 1 then "You picked up " + name + " (" + string count + ")." else "You picked up the " + name + ".")] Fin screen world
            true
        | (_, _) ->
            showLines actedOnOpt ["You have no room for the " + name + "."] Fin screen world
            false

    (* Doors *)

    /// Open a door into the area it leads to, the player stopping before it, to arrive before its partner there.
    static let openDoor door (enter : EnterData) (screen : Screen) (world : World) =
        let player = Simulants.GameplayPlayer
        let doorProp = Data.Areas[screen.GetAreaType world].Props |> List.find (fun prop -> prop.PropName = door)
        let partner = Data.Areas[enter.To].Props |> List.find (fun prop -> prop.PropName = door)
        let (arrival, arrivalYaw) = Prop.getArrival partner
        screen.SetMode (Opening { OpeningStart = world.ScaledTime; Door = doorProp; Destination = enter.To; Arrival = arrival; ArrivalYaw = arrivalYaw; Style = enter.Style; Entered = false }) world
        player.SetWalkStartOpt None world
        World.playSound 0.0f 0.0f enter.Style.Volume enter.Style.Sound world

    /// Try a door, by its prop's name - opening it into another area if it opens, with its key, the key used up once
    /// nothing else needs it, or saying why it won't.
    static let tryDoor door (enter : EnterData) (screen : Screen) world =
        let player = Simulants.GameplayPlayer
        let opens = screen.EvalConditionOpt enter.Opens world
        match enter.Key with
        | None when opens -> openDoor door enter screen world
        | Some _ when opens && screen.HasTranspired (Unlocked door) world -> openDoor door enter screen world
        | Some key when opens && player.Carries key world ->
            screen.MarkTranspired (Unlocked door) world
            player.Take key world
            World.playSound 0.0f 0.0f 0.4f Assets.Gameplay.KeyUnlockSound world
            showLines (Some door) ["You used the " + Data.Items[key].ItemName + "."] Fin screen world
        | Some _ | None ->
            World.playSound 0.0f 0.0f 0.25f Assets.Gameplay.DoorLockedSound world
            showLines (Some door) [enter.LockedLine] Fin screen world

    /// The blocks of the door opening in the dark - the wall, its face where the door's origin is, its doorway open into
    /// the dark beyond, a little wider than the door so its edges hide behind the door's frame, and the floor - each its
    /// name, position, rotation, scale, surface and color.
    static let getOpeningBlocks (opening : Opening) =
        let center = Constants.Gameplay.DoorScenePosition
        let doorway = opening.Style.Doorway + v2 (Constants.Gameplay.DoorSceneHoleTuck * 2.0f) Constants.Gameplay.DoorSceneHoleTuck
        let wallColor = Color (0.2f, 0.24f, 0.22f, 1.0f)
        [("WallLeft", center + v3 (-(4.0f + doorway.X * 0.5f) * 0.5f) 1.5f -0.1f, quatIdentity, v3 (4.0f - doorway.X * 0.5f) 3.0f 0.2f, Paint, wallColor)
         ("WallRight", center + v3 ((4.0f + doorway.X * 0.5f) * 0.5f) 1.5f -0.1f, quatIdentity, v3 (4.0f - doorway.X * 0.5f) 3.0f 0.2f, Paint, wallColor)
         ("WallTop", center + v3 0.0f ((3.0f + doorway.Y) * 0.5f) -0.1f, quatIdentity, v3 doorway.X (3.0f - doorway.Y) 0.2f, Paint, wallColor)
         ("Floor", center + v3 0.0f -0.05f 1.5f, quatIdentity, v3 8.0f 0.1f 4.0f, Linoleum, Color (0.18f, 0.2f, 0.19f, 1.0f))]

    /// The door's parts as it opens in the dark part way through - its frame standing and its leaves swinging - each as
    /// it stands in its area, brought round into the door scene to face the eye.
    static let getOpeningParts (opening : Opening) progress =
        let center = Constants.Gameplay.DoorScenePosition
        let swing = Math.Smoothstep (progress * 1.6f - 0.2f) * 1.6f
        let framing = opening.Door.PropFraming
        let unturn = Quaternion.Inverse (Quaternion.CreateFromYawPitchRoll (framing.Yaw, 0.0f, 0.0f))
        [for part in opening.Door.PropParts do
            let position = Vector3.Transform (part.PartPosition - framing.Origin, unturn) / framing.Scale
            let rotation = Quaternion.Concatenate (part.PartRotation, unturn)
            let (position, rotation) = if Array.contains part opening.Door.PropLeaves then opening.Style.Swing swing position rotation else (position, rotation)
            { part with PartPosition = center + position; PartRotation = rotation; PartScale = part.PartScale / framing.Scale }]

    /// Travel on to another stage, the player stopping where it stands, to arrive at a named arrival there.
    static let travelOn (travel : TravelData) (screen : Screen) (world : World) =
        let player = Simulants.GameplayPlayer
        let (arrival, arrivalYaw) = Area.getArrival travel.Arrival Data.Areas[travel.To]
        screen.SetMode (Travelling { TravelStart = world.ScaledTime; TravelDestination = travel.To; TravelArrival = arrival; TravelArrivalYaw = arrivalYaw; Caption = travel.Caption; Arrived = false }) world
        player.SetWalkStartOpt None world

    (* Cues *)

    /// Run a cue for the prop interacted with, by name, if any, then the cue after it - what follows lines waiting till
    /// they're read.
    static let rec runCueThen actedOnOpt cue after (screen : Screen) world =
        let player = Simulants.GameplayPlayer
        match cue with
        | Fin -> match after with Fin -> () | _ -> runCueThen actedOnOpt after Fin screen world
        | Say [] -> runCueThen actedOnOpt Fin after screen world
        | Say lines -> showLines actedOnOpt lines after screen world
        | SetFlag flag ->
            screen.MarkTranspired flag world
            runCueThen actedOnOpt Fin after screen world
        | If (condition, consequent, alternative) -> runCueThen actedOnOpt (if screen.EvalCondition condition world then consequent else alternative) after screen world
        | Sequence [] -> runCueThen actedOnOpt Fin after screen world
        | Sequence (first :: rest) -> runCueThen actedOnOpt first (Cue.andThen (Sequence rest) after) screen world
        | Give (item, count) ->
            giveItem actedOnOpt item count screen world |> ignore
            goOnAfterReading actedOnOpt after screen world
        | GiveAs (name, item, count) ->
            if giveItem actedOnOpt item count screen world then screen.MarkTranspired (Taken name) world
            goOnAfterReading actedOnOpt after screen world
        | Cue.Take item ->
            player.Take item world
            runCueThen actedOnOpt Fin after screen world
        | Enter enter ->
            match actedOnOpt with
            | Some door -> tryDoor door enter screen world
            | None -> failwith "A door tried with no prop to be the door - an Enter cue runs only for the door interacted with."
        | PickUp pickUp ->
            let pickup = match actedOnOpt with Some pickup -> pickup | None -> failwith "Something picked up with no prop to be it - a PickUp cue runs only for the pickup interacted with."
            if giveItem actedOnOpt pickUp.Item pickUp.Count screen world then
                screen.MarkTranspired (Taken pickup) world
                goOnAfterReading actedOnOpt (Cue.andThen (defaultArg pickUp.Then Fin) after) screen world
            else goOnAfterReading actedOnOpt after screen world
        | Save -> screen.SetMode Prompting world
        | OpenBox ->
            screen.SetMode Boxing world
            World.playSound 0.0f 0.0f 0.25f Assets.Gameplay.BoxOpenSound world
        | Keypad keypad -> screen.SetMode (Keypadding (keypad, actedOnOpt)) world
        | Sound sound ->
            World.playSound 0.0f 0.0f sound.Volume sound.Sound world
            runCueThen actedOnOpt Fin after screen world
        | Rouse ->
            rouseCreatures screen world
            runCueThen actedOnOpt Fin after screen world
        | Cue.Travel travel -> travelOn travel screen world
        | Escape -> screen.SetMode (Escaped world.ScaledTime) world

    /// Go on to the cue after one, once what's being read is read, if anything is.
    and goOnAfterReading actedOnOpt after (screen : Screen) world =
        match screen.GetMode world with
        | Reading reading -> screen.SetMode (Reading { reading with After = Cue.andThen reading.After after }) world
        | _ -> runCueThen actedOnOpt Fin after screen world

    /// Go through a door opening - into the next area halfway through, and on exploring once it's done.
    static let advanceOpening (opening : Opening) (screen : Screen) (world : World) =
        let player = Simulants.GameplayPlayer
        let age = (world.LocalTime opening.OpeningStart).SecondsF
        if not opening.Entered && age >= Constants.Gameplay.DoorDuration * 0.5f then
            screen.SetAreaType opening.Destination world
            player.Enter opening.Arrival opening.ArrivalYaw world
            screen.SetMode (Opening { opening with Entered = true }) world
        elif age >= Constants.Gameplay.DoorDuration then
            screen.SetMode Exploring world

    /// Travel on in the dark - into the other stage's area halfway through, and on exploring once it's done.
    static let advanceTravelling (travelling : Travelling) (screen : Screen) (world : World) =
        let player = Simulants.GameplayPlayer
        let age = (world.LocalTime travelling.TravelStart).SecondsF
        if not travelling.Arrived && age >= Constants.Gameplay.TravelDuration * 0.5f then
            screen.SetAreaType travelling.TravelDestination world
            player.Enter travelling.TravelArrival travelling.TravelArrivalYaw world
            screen.SetMode (Travelling { travelling with Arrived = true }) world
        elif age >= Constants.Gameplay.TravelDuration then
            screen.SetMode Exploring world

    (* Screens Over Play *)

    /// Open the inventory when the player asks for it while exploring, free or aiming, the player stopping where it
    /// stands - true if it opened, so play itself doesn't act on the intent too.
    static let tryOpenInventory (intent : Intent) (screen : Screen) world =
        let player = Simulants.GameplayPlayer
        if intent.ToggleInventory && screen.GetMode world = Exploring && (match player.GetAction world with Free | Aiming _ -> true | _ -> false) then
            player.SetAction Free world
            player.SetWalkStartOpt None world
            screen.SetMode Browsing world
            World.playSound 0.0f 0.0f 0.2f Assets.Gameplay.MenuConfirmSound world
            true
        else false

    /// Use an item from the inventory - a remedy taken, if the player's hurt.
    static let useItem item (_ : Screen) world =
        let player = Simulants.GameplayPlayer
        match Data.Items[item].ItemKind with
        | Remedy amount when player.GetHealth world < Constants.Gameplay.PlayerHitPoints ->
            player.Take item world
            player.SetHealth (min Constants.Gameplay.PlayerHitPoints (player.GetHealth world + amount)) world
            World.playSound 0.0f 0.0f 0.2f Assets.Gameplay.HealSound world
        | _ -> ()

    /// Take a weapon in hand from the inventory, or put it away.
    static let equipItem item (_ : Screen) world =
        let player = Simulants.GameplayPlayer
        let inventory = Inventory.equip item (player.GetInventory world)
        player.SetInventory inventory world
        if inventory.EquippedOpt = Some item && Data.Weapons[item].AmmunitionOpt.IsSome then World.playSound 0.0f 0.0f 0.4f Assets.Gameplay.GunReadySound world

    /// Save at the typewriter, spending a ribbon, if there's one to spend - once, as the prompt's up, however many times
    /// it's raised saving.
    static let save (screen : Screen) world =
        let player = Simulants.GameplayPlayer
        match screen.GetMode world with
        | Prompting ->
            screen.SetMode Exploring world
            match Data.Items |> Map.tryFindKey (fun item data -> data.ItemKind = Ribbon && player.Carries item world) with
            | Some ribbon ->
                player.Take ribbon world
                screen.SaveCount.Map inc world
                World.playSound 0.0f 0.0f 0.25f Assets.Gameplay.TypewriterSound world
                Save.write (snapshot screen world)
            | None -> ()
        | _ -> ()

    /// Try a code at the keypad that's up - the right one going on to what follows it.
    static let tryCode code (screen : Screen) world =
        match screen.GetMode world with
        | Keypadding (keypad, actedOnOpt) when code = keypad.Code ->
            screen.SetMode Exploring world
            runCueThen actedOnOpt keypad.Then Fin screen world
        | _ -> World.playSound 0.0f 0.0f 0.1f Assets.Gameplay.KeypadWrongSound world

    /// Turn the page of what's being read, or finish reading it and go on to what follows.
    static let pageOn (screen : Screen) world =
        match screen.GetMode world with
        | Reading reading ->
            if reading.Page + 1 < reading.Lines.Length then
                screen.SetMode (Reading { reading with Page = inc reading.Page }) world
                World.playSound 0.0f 0.0f 0.35f Assets.Gameplay.PageSound world
            else
                screen.SetMode Exploring world
                runCueThen reading.ActedOnOpt reading.After Fin screen world
        | _ -> ()

    static member Properties =
        [define Screen.AreaType Data.StartArea
         define Screen.Mode Exploring
         define Screen.Advents Set.empty
         define Screen.PlayTime 0.0f
         define Screen.SaveCount 0
         define Screen.Intent Intent.empty
         define Screen.IntentOverrideOpt None
         define Screen.GameSeed Constants.Gameplay.FixedGameSeed
         define Screen.Colliders [||]
         define Screen.SightBlockers [||]
         define Screen.PendingStartOpt None]

    override this.Process (selectionResults, screen, world) =

        // only process when selected
        if screen.GetSelected world then

            // process intent - a test's in place of the keyboard's, if it gives one
            let intent =
                match screen.GetIntentOverrideOpt world with
                | Some intent -> intent
                | None -> Intent.ofKeyboard world
            screen.SetIntent intent world

            // advance play time - how long's been played, for the controls hint and the ending, never a clock for play
            screen.PlayTime.Map ((+) (single world.ScaledDelta.SecondsF)) world

            // begin scene declaration
            // NOTE: unlike MMCC, ImSim works more smoothly by placing gui entities in the default Scene group instead
            // of in their own separate Gui group. This is because, by default, ImSim ties simulant declaration
            // order together with simulant processing order. While common scene content in ImSim is still saved in /
            // loaded from a .nugroup file, you might instead consider saving / loading additive content in .nuentity
            // files. Lastly, note that you can save multiple entities to a single .nuentity file by parenting them
            // under a single root entity.
            World.beginGroup Simulants.GameplayScene.Name [] world

            // declare sky box
            World.doSkyBox "SkyBox"
                [Entity.CubeMap .= Assets.Gameplay.InteriorSkyBoxMap
                 Entity.AmbientColor .= Constants.Gameplay.AmbientColor
                 Entity.Brightness .= 0.2f] world

            // declare lighting config
            World.doLighting3dConfig "Lighting3dConfig"
                [Entity.Lighting3dConfig .= Constants.Gameplay.LightingConfig] world

            // declare player before anything reads it, so nothing reads it before it's there
            World.doEntity<PlayerDispatcher> Simulants.GameplayPlayer.Name
                [Entity.Intent @= intent] world
            let player = world.DeclaredEntity

            // process pending start, a new game's or a save's
            match screen.GetPendingStartOpt world with
            | Some start ->
                applyStart start screen world
                screen.SetPendingStartOpt None world
            | None -> ()

            // process the screens over play
            let browsing = screen.GetMode world = Browsing
            let boxing = screen.GetMode world = Boxing
            for item in World.doSubscription "UseItem" Simulants.GameplayInventory.UseItemEvent world do if browsing then useItem item screen world
            for item in World.doSubscription "EquipItem" Simulants.GameplayInventory.EquipItemEvent world do if browsing then equipItem item screen world
            for (slot, partner) in World.doSubscription "CombineItems" Simulants.GameplayInventory.CombineItemsEvent world do
                if browsing then
                    match Inventory.tryCombine slot partner (player.GetInventory world) with
                    | Some combined -> player.SetInventory combined world
                    | None -> ()
            for slot in World.doSubscription "Store" Simulants.GameplayItemBox.StoreEvent world do if boxing then player.Inventory.Map (Inventory.store slot) world
            for item in World.doSubscription "Retrieve" Simulants.GameplayItemBox.RetrieveEvent world do if boxing then player.Inventory.Map (Inventory.retrieve item) world
            for code in World.doSubscription "Code" Simulants.GameplayKeypad.CodeEvent world do tryCode code screen world
            if World.doSubscriptionAny "Save" Simulants.GameplaySavePrompt.SaveEvent world then save screen world
            if World.doSubscriptionAny "Page" Simulants.GameplayReading.PageEvent world then pageOn screen world
            for (name, closer) in
                [("InventoryClose", Simulants.GameplayInventory)
                 ("ItemBoxClose", Simulants.GameplayItemBox)
                 ("KeypadClose", Simulants.GameplayKeypad)
                 ("SavePromptClose", Simulants.GameplaySavePrompt)] do
                if World.doSubscriptionAny name closer.CloseEvent world then
                    screen.SetMode Exploring world

            // process inventory opening, keeping its intent from play and from the inventory itself
            if world.TimeAdvancing && tryOpenInventory intent screen world then
                screen.SetIntent Intent.empty world
                player.SetIntent Intent.empty world

            // advance mode - a door opening, travelling on, the player falling, and the end of play
            match screen.GetMode world with
            | Opening opening -> advanceOpening opening screen world
            | Travelling travelling -> advanceTravelling travelling screen world
            | Exploring ->
                match player.GetAction world with
                | Dying start when (world.LocalTime start).SecondsF >= Puppet.getMarkTime Data.Puppets[Player] AnimationType.Wound Mark.End + Constants.Gameplay.WoundPause ->
                    screen.SetMode (Fallen world.ScaledTime) world
                | _ -> ()
            | Fallen start when hasLingered start Constants.Gameplay.FallenDuration world -> World.publish () screen.FallenEvent screen world
            | Escaped start when hasLingered start Constants.Gameplay.EscapedDuration world -> World.publish (screen.GetPlayTime world) screen.EscapedEvent screen world
            | _ -> ()

            // update colliders and sight blockers, read only once a door's done changing the area
            let area = Data.Areas[screen.GetAreaType world]
            let present = getPresentProps area screen world
            screen.SetColliders (Array.append area.Colliders (present |> Array.ofList |> Array.collect (fun (_, prop) -> prop.PropColliders))) world
            screen.SetSightBlockers Data.SightBlockers[area.AreaType] world

            // process player hits and shots
            for hit in World.doSubscription "Hit" player.HitEvent world do takeHit hit screen world
            if World.doSubscriptionAny "Shot" player.ShotEvent world then alertCreatures screen world

            // begin area declaration - everything of the area under an entity named for it, so declaring another
            // replaces it
            World.beginEntity<Entity3dDispatcher> area.AreaType.Name [] world

            // declare scenery, its static parts built and frozen by the scenery entity itself
            World.doEntity<SceneryDispatcher> "Scenery" [Entity.AreaTypeOpt .= Some area.AreaType] world

            // declare parts - those glowing with the power, and those of the props there only while a condition holds
            let parts =
                [for (index, glowing) in area.DynamicParts do
                    ("Dynamic+" + string index, glowing)
                 for (index, prop) in present do
                    for (subindex, shown) in Array.indexed prop.PropParts do
                        ("Conditional+" + string index + "+" + string subindex, shown)]
            for (name, part) in parts do
                World.doEntity<PartDispatcher> name
                    [Entity.Position .= part.PartPosition
                     Entity.Rotation .= part.PartRotation
                     Entity.Scale .= part.PartScale
                     Entity.Shape .= part.PartShape
                     Entity.SurfaceType .= part.PartSurface
                     Entity.Color .= part.PartColor
                     Entity.Glow .= part.PartGlow
                     Entity.Unreflected .= part.PartUnreflected
                     Entity.PowerScale @= (if part.PartPowered then getPowerScale part.PartPosition screen world else 1.0f)
                     Entity.CastShadow .= part.CastsShadow] world

            // declare light probe
            World.doLightProbe "Probe"
                [Entity.Position .= area.ProbePosition
                 Entity.AmbientColor .= Constants.Gameplay.AmbientColor
                 Entity.AmbientBrightness .= area.AmbientBrightness
                 Entity.ProbeBounds .= area.Bounds] world

            // declare lights - the area's, and those of the props there only while a condition holds
            let lights =
                [for (index, shining) in List.indexed area.Lights do
                    ("Light+" + string index, shining)
                 for (index, prop) in present do
                    for (subindex, shining) in List.indexed prop.PropLights do
                        ("ConditionalLight+" + string index + "+" + string subindex, shining)]
            for (name, light) in lights do
                World.doEntity<LightDispatcher> name
                    [Entity.Position .= light.LightPosition
                     Entity.LightColor .= light.LightColor
                     Entity.LightBrightness .= light.LightBrightness
                     Entity.LightCutoff .= light.LightReach
                     Entity.ShadowingType .= (if light.LightShadowed then DynamicShadowing else NoShadowing)
                     Entity.PowerScale @= (if light.LightPowered then getPowerScale light.LightPosition screen world else 1.0f)] world

            // declare fixed eyes
            for (index, camera) in List.indexed area.Cameras do
                World.doEntity<CameraZoneDispatcher> ("Eye+" + string index)
                    [Entity.Zone .= box2 camera.CameraMin (camera.CameraMax - camera.CameraMin)
                     Entity.Eye .= camera.CameraEye
                     Entity.LookAt .= camera.CameraTarget
                     Entity.FieldOfView .= Math.RadiansToDegreesF camera.CameraFieldOfView] world

            // declare interactables, each while it's there - a door, a pickup while it's not been taken, or a feature -
            // running its cue when it's interacted with
            for prop in area.Props do
                match prop.PropInteractOpt with
                | Some interact when screen.EvalConditionOpt prop.PropConditionOpt world ->
                    let declared =
                        match interact.Cue with
                        | Enter _ ->
                            World.doEntity<DoorDispatcher> prop.PropName
                                [Entity.Position .= prop.PropSpot
                                 Entity.Reach .= prop.PropReach
                                 Entity.InteractionType .= interact.InteractionType] world
                            true
                        | PickUp pickUp ->
                            if not (screen.HasTranspired (Taken prop.PropName) world) then
                                World.doEntity<PickupDispatcher> prop.PropName
                                    [Entity.Position .= prop.PropFraming.Origin
                                     Entity.Item .= pickUp.Item
                                     Entity.Count .= pickUp.Count
                                     Entity.InteractionType .= interact.InteractionType] world
                                true
                            else false
                        | _ ->
                            World.doEntity<FeatureDispatcher> prop.PropName
                                [Entity.Position .= prop.PropSpot
                                 Entity.Reach .= prop.PropReach
                                 Entity.InteractionType .= interact.InteractionType] world
                            true
                    if declared && World.doSubscriptionAny "Interact" world.DeclaredEntity.InteractEvent world then
                        runCueThen (Some prop.PropName) interact.Cue Fin screen world
                | Some _ | None -> ()

            // declare creatures, each while it's not been killed - or, killed while the player's here, its body staying
            // where it fell until the player leaves - each fated by the game seed to rise once more after it's first
            // felled, or not - landing their attacks on the player
            for enemy in area.Enemies do
                let name = enemy.EnemyName
                if screen.EvalConditionOpt enemy.EnemyConditionOpt world && not (screen.HasTranspired (Killed name) world) || (screen.GetArea world / name).GetExists world then
                    let data = Data.Creatures[enemy.EnemyCreatureType]
                    let fate = RandomKey.make (screen.GetGameSeed world) |> RandomKey.withName "rise" |> RandomKey.withName name |> RandomKey.toSingle
                    World.doEntity<CreatureDispatcher> name
                        [Entity.CreatureType .= enemy.EnemyCreatureType
                         Entity.Position |= enemy.EnemyPosition
                         Entity.Yaw |= enemy.EnemyYaw
                         Entity.CreatureState |= (if enemy.EnemyLying then Dormant else Waiting)
                         Entity.HitPoints |= data.HitPoints
                         Entity.WillRise |= (fate < data.RiseChance)
                         Entity.LastCry |= world.ScaledTime + GameTime.ofSeconds (double (name.Length % 5))
                         Entity.Enraged @= screen.HasTranspired (Enraged name) world] world
                    for attack in World.doSubscription "Attack" world.DeclaredEntity.AttackEvent world do takeAttack attack screen world

            // end area declaration
            World.endEntity world

            // declare gui, once play this frame is settled
            let intent = screen.GetIntent world
            match screen.GetMode world with
            | Reading reading ->
                World.doEntity<ReadingDispatcher> Simulants.GameplayReading.Name
                    [Entity.Reading @= reading
                     Entity.Intent @= intent
                     Entity.Elevation .= Constants.Gui.Elevation] world
            | Prompting ->
                World.doEntity<SavePromptDispatcher> Simulants.GameplaySavePrompt.Name
                    [Entity.Intent @= intent
                     Entity.Elevation .= Constants.Gui.Elevation] world
            | Browsing ->
                World.doEntity<InventoryDispatcher> Simulants.GameplayInventory.Name
                    [Entity.Inventory @= player.GetInventory world
                     Entity.Health @= player.GetHealth world
                     Entity.Intent @= intent
                     Entity.Elevation .= Constants.Gui.Elevation] world
            | Boxing ->
                World.doEntity<ItemBoxDispatcher> Simulants.GameplayItemBox.Name
                    [Entity.Inventory @= player.GetInventory world
                     Entity.Intent @= intent
                     Entity.Elevation .= Constants.Gui.Elevation] world
            | Keypadding (keypad, _) ->
                World.doEntity<KeypadDispatcher> Simulants.GameplayKeypad.Name
                    [Entity.Digits .= keypad.Code.Length
                     Entity.Intent @= intent
                     Entity.Elevation .= Constants.Gui.Elevation] world
            | Exploring ->
                match player.TryGetInteractable world with
                | Some interactable ->
                    World.doEntity<ActionPromptDispatcher> "ActionPrompt"
                        [Entity.InteractionType @= interactable.GetInteractionType world
                         Entity.Elevation .= Constants.Gui.Elevation] world
                | None -> ()
                if screen.GetAreaType world = Data.StartArea && screen.GetPlayTime world < Constants.Gameplay.ControlsHintDuration then
                    World.doEntity<ControlsDispatcher> "Controls"
                        [Entity.Elevation .= Constants.Gui.Elevation] world
                if player.GetCondition world = Danger then
                    World.doEntity<DangerDispatcher> "Danger"
                        [Entity.Elevation .= Constants.Gui.Elevation] world
            | Travelling travelling ->
                World.doText "Caption"
                    [Entity.Position .= v3 0.0f 0.0f 0.0f
                     Entity.Size .= v3 900.0f 60.0f 0.0f
                     Entity.Text .= travelling.Caption
                     Entity.TextColor .= Constants.Gui.DimColor
                     Entity.Elevation .= Constants.Gui.Elevation] world
            | Opening _ | Fallen _ | Escaped _ -> ()

            // end scene declaration
            World.endGroup world

            // declare door scene - the door opening in the dark between areas
            match screen.GetMode world with
            | Opening opening ->
                World.beginGroup Simulants.GameplayDoorScene.Name [] world
                let progress = min 1.0f ((world.LocalTime opening.OpeningStart).SecondsF / Constants.Gameplay.DoorDuration)
                for (name, position, rotation, scale, surface, color) in getOpeningBlocks opening do
                    World.doEntity<BlockDispatcher> name
                        [Entity.Position .= position
                         Entity.Rotation .= rotation
                         Entity.Scale .= scale
                         Entity.Static .= false
                         Entity.MaterialProperties .= SurfaceData.makeProperties Data.Surfaces[surface] color 0.0f] world
                for (index, part) in List.indexed (getOpeningParts opening progress) do
                    World.doEntity<PartDispatcher> ("Door+" + string index)
                        [Entity.Position @= part.PartPosition
                         Entity.Rotation @= part.PartRotation
                         Entity.Scale .= part.PartScale
                         Entity.Shape .= part.PartShape
                         Entity.SurfaceType .= part.PartSurface
                         Entity.Color .= part.PartColor
                         Entity.Glow .= part.PartGlow
                         Entity.Unreflected .= part.PartUnreflected
                         Entity.PowerScale @= (if part.PartPowered then getPowerScale opening.Door.PropFraming.Origin screen world else 1.0f)
                         Entity.CastShadow .= part.CastsShadow] world
                World.doLight3d "Lamp"
                    [Entity.Position .= Constants.Gameplay.DoorScenePosition + v3 1.4f 2.4f 1.2f
                     Entity.Color .= Color (1.0f, 0.9f, 0.75f, 1.0f)
                     Entity.Brightness @= 3.0f * (1.0f - Math.Smoothstep (progress * 1.4f - 0.4f))
                     Entity.LightCutoff .= 6.0f
                     Entity.Static .= false] world
                World.endGroup world
            | _ -> ()

            // update eye while time's advancing, leaving it to the editor while time's halted
            if world.TimeAdvancing then
                match screen.GetMode world with
                | Travelling _ ->
                    let center = Constants.Gameplay.DoorScenePosition
                    World.setEye3dCenter (center + v3 0.0f 1.5f 3.0f) world
                    World.setEye3dRotation (lookAt (center + v3 0.0f 1.5f 3.0f) center) world
                | Opening opening ->
                    let progress = min 1.0f ((world.LocalTime opening.OpeningStart).SecondsF / Constants.Gameplay.DoorDuration)
                    let center = Constants.Gameplay.DoorScenePosition
                    let eye = center + v3 0.0f 1.5f (3.2f - 3.6f * Math.Smoothstep progress)
                    World.setEye3dCenter eye world
                    World.setEye3dRotation (lookAt eye (center + v3 0.0f 1.35f -2.0f)) world
                    World.setEye3dFieldOfView (Math.DegreesToRadiansF Constants.Gameplay.DoorSceneFieldOfView) world
                | _ ->
                    let point = Collider.toFloor (player.GetPosition world)
                    let zones = screen.GetAreaEntities world |> Array.filter (fun zone -> zone.Is<CameraZoneDispatcher> world)
                    let zoneOpt =
                        match Array.tryFind (fun (zone : Entity) -> (zone.GetZone world).Contains point <> ContainmentType.Disjoint) zones with
                        | Some zone -> Some zone
                        | None -> zones |> Array.sortBy (fun zone -> let box = zone.GetZone world in (Vector2.Clamp (point, box.Min, box.Min + box.Size) - point).LengthSquared ()) |> Array.tryHead
                    match zoneOpt with
                    | Some zone ->
                        World.setEye3dCenter (zone.GetEye world) world
                        World.setEye3dRotation (lookAt (zone.GetEye world) (player.GetBounds world).Center) world
                        World.setEye3dFieldOfView (Math.DegreesToRadiansF (zone.GetFieldOfView world)) world
                    | None -> ()

            // request song
            screen.SetSongRequest (RequestSong (getSong screen world)) world

            // scale time by mode, holding everything of play still while a screen's up over play
            let timeScalar =
                match screen.GetMode world with
                | Exploring | Opening _ | Travelling _ | Fallen _ | Escaped _ -> 1.0f
                | Reading _ | Browsing | Boxing | Prompting | Keypadding _ -> 0.0f
            World.setTimeScalar timeScalar world

        // restore time scale when deselected
        if FQueue.contains Deselecting selectionResults then
            World.setTimeScalar 1.0f world
