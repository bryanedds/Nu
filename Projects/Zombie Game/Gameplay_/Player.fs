// Zombie Game.
// Copyright (C) Bryan Edds.

namespace ZombieGame
open System
open System.Numerics
open Prime
open Nu

/// What the player carries and how it is, its intent, the events it raises, and what a player does with what it
/// carries.
[<AutoOpen>]
module PlayerDispatcherExtensions =
    type Entity with
        member this.GetHealth world : int = this.Get (nameof this.Health) world
        member this.SetHealth (value : int) world = this.Set (nameof this.Health) value world
        member this.Health = lens (nameof this.Health) this this.GetHealth this.SetHealth
        member this.GetInventory world : Inventory = this.Get (nameof this.Inventory) world
        member this.SetInventory (value : Inventory) world = this.Set (nameof this.Inventory) value world
        member this.Inventory = lens (nameof this.Inventory) this this.GetInventory this.SetInventory
        member this.GetBacking world : bool = this.Get (nameof this.Backing) world
        member this.SetBacking (value : bool) world = this.Set (nameof this.Backing) value world
        member this.Backing = lens (nameof this.Backing) this this.GetBacking this.SetBacking
        member this.GetAimPitch world : single = this.Get (nameof this.AimPitch) world
        member this.SetAimPitch (value : single) world = this.Set (nameof this.AimPitch) value world
        member this.AimPitch = lens (nameof this.AimPitch) this this.GetAimPitch this.SetAimPitch
        member this.GetShotTimeOpt world : GameTime option = this.Get (nameof this.ShotTimeOpt) world
        member this.SetShotTimeOpt (value : GameTime option) world = this.Set (nameof this.ShotTimeOpt) value world
        member this.ShotTimeOpt = lens (nameof this.ShotTimeOpt) this this.GetShotTimeOpt this.SetShotTimeOpt
        member this.GetAimMarkOpt world : Entity option = this.Get (nameof this.AimMarkOpt) world
        member this.SetAimMarkOpt (value : Entity option) world = this.Set (nameof this.AimMarkOpt) value world
        member this.AimMarkOpt = lens (nameof this.AimMarkOpt) this this.GetAimMarkOpt this.SetAimMarkOpt
        member this.GetAimTurnOpt world : (GameTime * single) option = this.Get (nameof this.AimTurnOpt) world
        member this.SetAimTurnOpt (value : (GameTime * single) option) world = this.Set (nameof this.AimTurnOpt) value world
        member this.AimTurnOpt = lens (nameof this.AimTurnOpt) this this.GetAimTurnOpt this.SetAimTurnOpt
        member this.GetFreedTimeOpt world : GameTime option = this.Get (nameof this.FreedTimeOpt) world
        member this.SetFreedTimeOpt (value : GameTime option) world = this.Set (nameof this.FreedTimeOpt) value world
        member this.FreedTimeOpt = lens (nameof this.FreedTimeOpt) this this.GetFreedTimeOpt this.SetFreedTimeOpt
        member this.GetReeledTimeOpt world : GameTime option = this.Get (nameof this.ReeledTimeOpt) world
        member this.SetReeledTimeOpt (value : GameTime option) world = this.Set (nameof this.ReeledTimeOpt) value world
        member this.ReeledTimeOpt = lens (nameof this.ReeledTimeOpt) this this.GetReeledTimeOpt this.SetReeledTimeOpt
        member this.GetSteps world : int = this.Get (nameof this.Steps) world
        member this.SetSteps (value : int) world = this.Set (nameof this.Steps) value world
        member this.Steps = lens (nameof this.Steps) this this.GetSteps this.SetSteps
        member this.GetIntent world : Intent = this.Get (nameof this.Intent) world
        member this.SetIntent (value : Intent) world = this.Set (nameof this.Intent) value world
        member this.Intent = lens (nameof this.Intent) this this.GetIntent this.SetIntent
        member this.HitEvent = Events.HitEvent --> this
        member this.ShotEvent = Events.ShotEvent --> this

        /// Whether a player carries an item.
        member this.Carries item world =
            Inventory.has item (this.GetInventory world)

        /// Take an item from a player, as a key turned in its lock for good.
        member this.Take item world =
            this.Inventory.Map (Inventory.remove item 1) world

        /// How a player's health stands.
        member this.GetCondition world =
            let hitPoints = this.GetHealth world
            if hitPoints > Constants.Gameplay.CautionHitPoints then Fine
            elif hitPoints > Constants.Gameplay.DangerHitPoints then Caution
            else Danger

        /// What the player would act on - the nearest interactable of the area it's in before it, within reach.
        member this.TryGetInteractable world =
            let at = Collider.toFloor (this.GetPosition world)
            let yaw = this.GetYaw world
            this.Screen.GetAreaEntities world |>
            Array.choose (fun interactable ->
                if interactable.Has<InteractableFacet> world then
                    let target = Collider.toFloor (interactable.GetPosition world)
                    let reach = interactable.GetReach world
                    if Collider.reaches at yaw target reach then Some ((target - at).Length () - reach, interactable) else None
                else None) |>
            Array.sortBy fst |>
            Array.tryHead |>
            Option.map snd

        /// Bring the player into an area at a spot facing a way.
        member this.Enter (position : Vector3) yaw world =
            this.SetPosition position world
            this.SetYaw yaw world
            this.SetAction Free world
            this.SetWalkStartOpt None world

/// The player - walked with tank controls by intent, aiming and firing at what's before it, examining
/// and opening what it acts on, seized, bitten and struck by what reaches it. It keeps its health, inventory and
/// action on itself, and lives in the scene group so it goes on from area to area.
type PlayerDispatcher () =
    inherit Entity3dDispatcherImSim (false, false, false)

    /// The creatures of the area the player's in.
    static let getCreatures (entity : Entity) world =
        entity.Screen.GetAreaEntities world |> Array.filter (fun creature -> creature.Is<CreatureDispatcher> world)

    /// The creatures of the area standing, each where it stands and how wide it is - what the player walks around.
    static let getStanding (entity : Entity) world =
        getCreatures entity world |> Array.filter (fun creature -> creature.GetUp world) |> Array.map (fun creature -> (Collider.toFloor (creature.GetPosition world), Data.Creatures[creature.GetCreatureType world].Radius))

    /// The weapon in the player's hand, if any.
    static let tryGetWeapon (entity : Entity) world =
        (entity.GetInventory world).EquippedOpt |> Option.map (fun item -> Data.Weapons[item])

    /// Fire a ray along the floor from the player, finding the first creature it reaches before anything standing tall
    /// enough to block sight - passing over a desk or a gurney - those lying low only when aimed down, and a hound or one
    /// getting up either way, as it's low enough to hit aimed down. It springs from the player as far to the side as the
    /// gun in its hand is, so it runs along the barrel as it's drawn.
    static let cast yaw range (entity : Entity) world =
        let player = Collider.toFloor (entity.GetPosition world)
        let facing = Math.YawFacing (entity.GetYaw world)
        let right = v2 -facing.Y facing.X
        let side =
            match (entity / "Puppet").GetMuzzleOpt world with
            | Some muzzle -> right * Vector2.Dot (Collider.toFloor muzzle - player, right)
            | None -> v2Zero
        let start = player + side + Math.YawFacing yaw * 0.2f
        let stop = start + Math.YawFacing yaw * range
        let wall = entity.Screen.GetSightBlockers world |> Array.choose (Collider.tryCast start stop) |> Array.fold min 1.0f
        let aimedDown = entity.GetAimPitch world < -Constants.Gameplay.AimTilt
        getCreatures entity world |>
        Array.filter (fun creature ->
            creature.GetAlive world &&
            (if creature.GetLow world then aimedDown
             else not aimedDown || Data.Creatures[creature.GetCreatureType world].Behavior = Hound || (creature.GetCreatureState world).IsRising)) |>
        Array.choose (fun creature ->
            let circle = CircleCollider (Collider.toFloor (creature.GetPosition world), Data.Creatures[creature.GetCreatureType world].Radius)
            match Collider.tryCast start stop circle with
            | Some t when t <= wall -> Some (t, creature)
            | Some _ | None -> None) |>
        Array.sortBy fst |>
        Array.tryHead |>
        Option.map snd

    /// Land a hit on a creature - a shot aimed up going to its head, harder to a brute's, and a pellet's to the head from
    /// up close harder still.
    static let hit (weapon : WeaponData) (creature : Entity) (entity : Entity) world =
        let aimedUp = entity.GetAimPitch world > Constants.Gameplay.AimTilt
        let distance = (Collider.toFloor (creature.GetPosition world) - Collider.toFloor (entity.GetPosition world)).Length ()
        let closeHead = aimedUp && weapon.Pellets > 1 && distance <= Constants.Gameplay.PelletCloseRange
        let damage = if closeHead then weapon.Damage * Constants.Gameplay.PelletCloseHeadFactor else weapon.Damage
        let damage = if aimedUp && Data.Creatures[creature.GetCreatureType world].Behavior = Brute then damage * 3 / 2 else damage
        let headShot = aimedUp && weapon.AmmunitionOpt.IsSome
        let lowShot = entity.GetAimPitch world < -Constants.Gameplay.AimTilt
        World.publish { HitDamage = damage; KnocksDown = weapon.KnocksDown; HeadShot = headShot; LowShot = lowShot; Struck = creature } entity.HitEvent entity world

    /// Fire the gun in hand, or slash with the knife, if it's ready - a gun out of ammunition only clicks.
    static let attack (entity : Entity) world =
        match (entity.GetAction world, tryGetWeapon entity world) with
        | (Aiming aimStart, Some weapon) when
            (world.LocalTime aimStart).SecondsF >= Constants.Gameplay.AimRaiseDuration &&
            (entity.GetShotTimeOpt world |> Option.forall (fun shot -> (world.LocalTime shot).SecondsF >= weapon.Cooldown)) ->
            let now = world.ScaledTime
            match weapon.AmmunitionOpt with
            | Some ammunition when Inventory.has ammunition (entity.GetInventory world) ->
                entity.SetShotTimeOpt (Some now) world
                entity.Take ammunition world
                let yaw = entity.GetYaw world
                World.playSound 0.0f 0.0f weapon.Sound.Volume weapon.Sound.Sound world
                for pellet in 0 .. dec weapon.Pellets do
                    let spread = if weapon.Pellets > 1 then (single pellet / single (weapon.Pellets - 1) * 2.0f - 1.0f) * Constants.Gameplay.PelletSpread else 0.0f
                    match cast (yaw + spread) weapon.Range entity world with
                    | Some creature -> hit weapon creature entity world
                    | None -> ()
                World.publish () entity.ShotEvent entity world
            | Some _ ->
                // only clicking - no shot, so no flash or kick
                World.playSound 0.0f 0.0f 0.5f Assets.Gameplay.EmptyClickSound world
            | None ->
                entity.SetAction (Slashing now) world
                entity.SetShotTimeOpt (Some now) world
                World.playSound 0.0f 0.0f weapon.Sound.Volume weapon.Sound.Sound world
        | (_, _) -> ()

    /// The creatures standing or getting up in sight within the weapon's reach, nearest first - what auto-aim turns
    /// toward.
    static let getMarks (weapon : WeaponData) (entity : Entity) world =
        let player = Collider.toFloor (entity.GetPosition world)
        getCreatures entity world |>
        Array.filter (fun creature ->
            (creature.GetUp world || (creature.GetCreatureState world).IsRising) &&
            (let at = Collider.toFloor (creature.GetPosition world)
             let distance = (at - player).Length ()
             distance < weapon.Range && distance > 0.01f && entity.Screen.HasLineOfSight player at world)) |>
        Array.sortBy (fun creature -> (Collider.toFloor (creature.GetPosition world) - player).LengthSquared ())

    /// Begin turning toward a creature, aiming level at it and keeping it as the mark auto-aim cycles on from.
    static let aimAt (creature : Entity) (entity : Entity) (world : World) =
        entity.SetAimTurnOpt (Some (world.ScaledTime, entity.GetYaw world)) world
        entity.SetAimPitch 0.0f world
        entity.SetAimMarkOpt (Some creature) world

    /// Turn on toward the mark auto-aim found, the short way round, easing onto it over a moment, following it as it
    /// moves - done once the turn's taken its time, or its mark's gone.
    static let turnToMark (start : GameTime) fromYaw (entity : Entity) world =
        match entity.GetAimMarkOpt world with
        | Some mark when mark.GetExists world ->
            let progress = min 1.0f ((world.LocalTime start).SecondsF / Constants.Gameplay.AimTurnDuration)
            let markYaw = Math.YawOf (Collider.toFloor (mark.GetPosition world) - Collider.toFloor (entity.GetPosition world))
            let turn = MathF.IEEERemainder (markYaw - fromYaw, MathF.Tau)
            entity.SetYaw (fromYaw + turn * Math.Smoothstep progress) world
            if progress >= 1.0f then entity.SetAimTurnOpt None world
        | Some _ | None -> entity.SetAimTurnOpt None world

    /// Raise the weapon in hand - turning toward the nearest creature in sight, any way round, as the old games' aim found
    /// its mark.
    static let raiseWeapon (entity : Entity) world =
        match tryGetWeapon entity world with
        | Some weapon ->
            entity.SetAimMarkOpt None world
            entity.SetAimTurnOpt None world
            entity.SetAimPitch 0.0f world
            getMarks weapon entity world |> Array.tryHead |> Option.iter (fun creature -> aimAt creature entity world)
            entity.SetAction (Aiming (world.ScaledTime)) world
            entity.SetWalkStartOpt None world
        | None -> ()

    /// Auto-aim, the weapon raised - beginning to turn toward the next creature in sight clockwise from the one aimed at,
    /// or the nearest if none is.
    static let autoAim (weapon : WeaponData) (entity : Entity) world =
        let player = Collider.toFloor (entity.GetPosition world)
        let marks = getMarks weapon entity world
        let nextOpt =
            match entity.GetAimMarkOpt world with
            | Some mark when Array.contains mark marks ->
                // turning right turns clockwise, lessening the yaw - so the next clockwise is the least yaw turned through
                let markYaw = Math.YawOf (Collider.toFloor (mark.GetPosition world) - player)
                marks |>
                Array.filter (fun creature -> creature <> mark) |>
                Array.sortBy (fun creature ->
                    let turned = MathF.IEEERemainder (markYaw - Math.YawOf (Collider.toFloor (creature.GetPosition world) - player), MathF.Tau)
                    if turned <= 0.0f then turned + MathF.Tau else turned) |>
                Array.tryHead |>
                Option.orElse (Some mark)
            | Some _ | None -> Array.tryHead marks
        nextOpt |> Option.iter (fun creature -> aimAt creature entity world)

    /// Free the player of a creature that's let go of it - its grab done, or broken by a blow - reeling a moment where
    /// it stands, when nothing can seize or hurt it.
    static let release (entity : Entity) world =
        match entity.GetAction world with
        | Grabbed (holder, _) ->
            let holding = holder.GetExists world && (holder.GetCreatureState world).IsGrabbing
            if not holding then
                entity.SetAction (Reeling (world.ScaledTime)) world
                entity.SetFreedTimeOpt (Some world.ScaledTime) world
        | _ -> ()

    /// Move by the player's intent - tank controls, the player walking the way it faces.
    static let move delta (intent : Intent) (entity : Entity) (world : World) =
        let now = world.ScaledTime
        match entity.GetAction world with
        | Free when intent.Aim && (entity.GetInventory world).EquippedOpt.IsSome -> raiseWeapon entity world
        | Free when intent.QuickTurn -> entity.SetAction (QuickTurning (now, entity.GetYaw world)) world
        | Free ->
            let running = intent.Run && intent.Forward > Constants.Gameplay.IntentDeadZone
            let scalar = if entity.GetHealth world <= Constants.Gameplay.DangerHitPoints then Constants.Gameplay.DangerSpeedScalar else 1.0f
            let turnRate = if running then Constants.Gameplay.RunTurnRate else Constants.Gameplay.TurnRate
            let yaw = entity.GetYaw world - intent.Turn * turnRate * delta // turning right turns clockwise, seen from above
            entity.SetYaw yaw world
            if abs intent.Forward > Constants.Gameplay.IntentDeadZone then
                let speed = if intent.Forward > 0.0f then (if running then Constants.Gameplay.RunSpeed else Constants.Gameplay.WalkSpeed) else Constants.Gameplay.BackSpeed
                let speed = speed * scalar
                let direction = Math.YawFacing yaw * MathF.CopySign (1.0f, intent.Forward)
                let others = getStanding entity world
                let point = Collider.settle Constants.Gameplay.PlayerRadius (Collider.toFloor (entity.GetPosition world) + direction * (speed * delta)) (entity.Screen.GetColliders world) others
                entity.SetPosition (v3 point.X 0.0f point.Y) world
                if (entity.GetWalkStartOpt world).IsNone then entity.SetWalkStartOpt (Some now) world
                entity.SetBacking (intent.Forward < 0.0f) world
                entity.SetStepRate (speed / Constants.Gameplay.WalkSpeed) world
            else entity.SetWalkStartOpt None world
        | Aiming _ when not intent.Aim ->
            entity.SetAimTurnOpt None world
            entity.SetAction Free world
        | Aiming _ when intent.AutoAim -> tryGetWeapon entity world |> Option.iter (fun weapon -> autoAim weapon entity world)
        | Aiming _ ->
            // turning onto the mark auto-aim found, or slowly by hand, turning by hand taking over; and tipping the aim
            // up or down
            match entity.GetAimTurnOpt world with
            | Some (start, fromYaw) when abs intent.Turn <= Constants.Gameplay.IntentDeadZone -> turnToMark start fromYaw entity world
            | Some _ | None ->
                entity.SetAimTurnOpt None world
                entity.SetYaw (entity.GetYaw world - intent.Turn * Constants.Gameplay.AimTurnRate * delta) world
            entity.SetAimPitch (if intent.Forward > Constants.Gameplay.IntentDeadZone then 1.0f elif intent.Forward < -Constants.Gameplay.IntentDeadZone then -1.0f else 0.0f) world
        | Slashing start ->
            // the swung weapon's sweep hits what it reaches - once, as its sweep passes
            let sweep = Puppet.getMarkTime Data.Puppets[Player] AnimationType.Swing Mark.Sweep
            match tryGetWeapon entity world with
            | Some weapon when (world.LocalTime start).SecondsF - delta < sweep && (world.LocalTime start).SecondsF >= sweep ->
                let player = Collider.toFloor (entity.GetPosition world)
                let ahead = Math.YawFacing (entity.GetYaw world)
                let targetOpt =
                    getCreatures entity world |>
                    Array.filter (fun creature -> creature.GetAlive world) |>
                    Array.tryFind (fun creature ->
                        let delta = Collider.toFloor (creature.GetPosition world) - player
                        let distance = delta.Length ()
                        distance < weapon.Range + Data.Creatures[creature.GetCreatureType world].Radius &&
                        (distance < 0.3f || Vector2.Dot (delta / distance, ahead) > 0.7f))
                match targetOpt with
                | Some creature -> hit weapon creature entity world
                | None -> ()
            | Some _ | None -> ()
            if (world.LocalTime start).SecondsF >= Puppet.getMarkTime Data.Puppets[Player] AnimationType.Swing Mark.End then
                entity.SetAction (if intent.Aim then Aiming (now - GameTime.ofSeconds (double Constants.Gameplay.AimRaiseDuration)) else Free) world
        | Reeling start -> if (world.LocalTime start).SecondsF >= Puppet.getMarkTime Data.Puppets[Player] AnimationType.Hurt Mark.End then entity.SetAction Free world
        | QuickTurning (start, fromYaw) ->
            let progress = min 1.0f ((world.LocalTime start).SecondsF / Constants.Gameplay.QuickTurnDuration)
            entity.SetYaw (fromYaw + MathF.PI * Math.Smoothstep progress) world
            if progress >= 1.0f then entity.SetAction Free world
        | Grabbed _ | Dying _ -> ()

    /// How the player's puppet poses now - its animation, how far into it, and what's in hand.
    static let getPose (entity : Entity) world =
        let screen = entity.Screen
        let armed = (entity.GetInventory world).EquippedOpt
        match entity.GetAction world with
        | Dying start -> (WoundAnimation, (world.LocalTime start).SecondsF, None)
        | Reeling start -> (HurtAnimation, (world.LocalTime start).SecondsF, None)
        | Grabbed (_, start) -> (HurtAnimation, min 0.12f ((world.LocalTime start).SecondsF), None)
        | Slashing start -> (SwingAnimation, (world.LocalTime start).SecondsF, armed)
        | Aiming start ->
            let sinceShot = match entity.GetShotTimeOpt world with Some shot -> (world.LocalTime shot).SecondsF | None -> Single.MaxValue
            let gun = (tryGetWeapon entity world |> Option.bind _.AmmunitionOpt).IsSome
            let fired = gun && sinceShot < Constants.Gameplay.ShootDuration
            let pitch = entity.GetAimPitch world
            let upShown = if pitch > 0.0f then pitch * Constants.Gameplay.AimUpShown else pitch // aimed up, drawn only partway up
            let shownPitch = if pitch < 0.0f then pitch * Constants.Gameplay.AimDownShown else upShown // a gun aimed down drawn only partway down too
            if fired then (ShootAnimation shownPitch, sinceShot, armed)
            elif gun then (AimAnimation shownPitch, (world.LocalTime start).SecondsF, armed)
            else (ReadyAnimation upShown, (world.LocalTime start).SecondsF, armed) // a knife held out as it always was, in one hand
        | QuickTurning _ -> (WalkAnimation, world.ScaledTime.SecondsF * 2.0f, None)
        | Free ->
            match entity.GetWalkStartOpt world with
            | Some start -> ((if entity.GetBacking world then WalkBackAnimation else WalkAnimation), (world.LocalTime start).SecondsF * entity.GetStepRate world, None)
            | None -> (RelaxAnimation, 0.0f, None)

    static member Properties =
        [define Entity.Health Constants.Gameplay.PlayerHitPoints
         define Entity.Inventory Inventory.Initial
         define Entity.Action Free
         define Entity.Yaw 0.0f
         define Entity.WalkStartOpt None
         define Entity.Backing false
         define Entity.StepRate 1.0f
         define Entity.AimPitch 0.0f
         define Entity.ShotTimeOpt None
         define Entity.AimMarkOpt None
         define Entity.AimTurnOpt None
         define Entity.FreedTimeOpt None
         define Entity.ReeledTimeOpt None
         define Entity.Steps 0
         define Entity.Intent Intent.empty
         define Entity.Size (v3 0.8f 1.8f 0.8f)
         define Entity.Offset (v3 0.0f 0.5f 0.0f)]

    override this.Process (entity, world) =

        // get free of whatever's let go of it
        release entity world

        // act on the player's intent, while play goes on
        if entity.Screen.GetExploring world then
            let intent = entity.GetIntent world
            let delta = single world.ScaledDelta.SecondsF
            match entity.GetAction world with
            | Aiming _ when intent.Act ->
                // the aim turning and tipping on the frame the trigger's pulled too
                move delta intent entity world
                attack entity world
            | Free when intent.Act ->
                match entity.TryGetInteractable world with
                | Some interactable ->
                    entity.SetYaw (Math.YawOf (Collider.toFloor (interactable.GetPosition world) - Collider.toFloor (entity.GetPosition world))) world
                    entity.SetWalkStartOpt None world
                    World.publish () interactable.InteractEvent entity world
                | None -> ()
            | _ -> move delta intent entity world

            // footsteps, a foot falling at the end of each swing
            match (entity.GetAction world, entity.GetWalkStartOpt world) with
            | (Free, Some start) ->
                let steps = int ((world.LocalTime start).SecondsF * entity.GetStepRate world * Constants.Puppet.StepRate + 0.5f)
                if steps > entity.GetSteps world then World.playSound 0.0f 0.0f 0.5f Assets.Gameplay.FootstepSound world
                entity.SetSteps steps world
            | (_, _) -> entity.SetSteps 0 world

        // its body, its weapon in hand as it's raised
        let (animation, localTime, heldOpt) = getPose entity world
        World.doEntity<PuppetDispatcher> "Puppet"
            [Entity.CharacterType .= Player
             Entity.Position @= entity.GetPosition world
             Entity.Yaw @= entity.GetYaw world
             Entity.CharacterAnimation @= animation
             Entity.PuppetTime @= localTime
             Entity.IdleTime @= world.ScaledTime.SecondsF
             Entity.HeldOpt @= heldOpt
             Entity.MountOpt .= None
             Entity.Pickable .= false] world

    override this.Render (renderPass, entity, world) =

        // a muzzle flash, lighting the area for an instant
        match (entity / "Puppet").GetMuzzleOpt world with
        | Some muzzle when entity.GetShotTimeOpt world |> Option.exists (fun shot -> (world.LocalTime shot).SecondsF < Constants.Gameplay.ShotFlashDuration) ->
            Draw.flash renderPass muzzle world
            Draw.shape renderPass Light PartBall muzzle quatIdentity (v3Dup 0.12f) (Color (1.0f, 0.85f, 0.55f, 1.0f)) 8.0f false world
        | Some _ | None -> ()
