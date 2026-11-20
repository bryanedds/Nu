// Zombie Game.
// Copyright (C) Bryan Edds.

namespace ZombieGame
open System
open System.Numerics
open Prime
open Nu

/// What the creature reads of the player - whether it's down or held, and whether it's walking and how fast - before
/// the creature, the first dispatcher to read them.
[<AutoOpen>]
module PlayerDispatcherEarlyExtensions =
    type Entity with
        member this.GetAction world : PlayerAction = this.Get (nameof this.Action) world
        member this.SetAction (value : PlayerAction) world = this.Set (nameof this.Action) value world
        member this.Action = lens (nameof this.Action) this this.GetAction this.SetAction
        member this.GetWalkStartOpt world : GameTime option = this.Get (nameof this.WalkStartOpt) world
        member this.SetWalkStartOpt (value : GameTime option) world = this.Set (nameof this.WalkStartOpt) value world
        member this.WalkStartOpt = lens (nameof this.WalkStartOpt) this this.GetWalkStartOpt this.SetWalkStartOpt
        member this.GetStepRate world : single = this.Get (nameof this.StepRate) world
        member this.SetStepRate (value : single) world = this.Set (nameof this.StepRate) value world
        member this.StepRate = lens (nameof this.StepRate) this this.GetStepRate this.SetStepRate

/// What the gameplay screen's children read of it - the area and its colliders, the play clock and how far it moved this
/// frame, the game seed, whether play's going on, sounds placed in the area and lines of sight across it - before the
/// creature, the first dispatcher to read them.
[<AutoOpen>]
module GameplayDispatcherEarlyExtensions =
    type Screen with
        member this.GetAreaType world : AreaType = this.Get (nameof this.AreaType) world
        member this.SetAreaType (value : AreaType) world = this.Set (nameof this.AreaType) value world
        member this.AreaType = lens (nameof this.AreaType) this this.GetAreaType this.SetAreaType
        member this.GetMode world : Mode = this.Get (nameof this.Mode) world
        member this.SetMode (value : Mode) world = this.Set (nameof this.Mode) value world
        member this.Mode = lens (nameof this.Mode) this this.GetMode this.SetMode
        member this.GetPlayTime world : single = this.Get (nameof this.PlayTime) world
        member this.SetPlayTime (value : single) world = this.Set (nameof this.PlayTime) value world
        member this.PlayTime = lens (nameof this.PlayTime) this this.GetPlayTime this.SetPlayTime
        member this.GetGameSeed world : uint64 = this.Get (nameof this.GameSeed) world
        member this.SetGameSeed (value : uint64) world = this.Set (nameof this.GameSeed) value world
        member this.GameSeed = lens (nameof this.GameSeed) this this.GetGameSeed this.SetGameSeed
        member this.GetColliders world : Collider array = this.Get (nameof this.Colliders) world
        member this.SetColliders (value : Collider array) world = this.Set (nameof this.Colliders) value world
        member this.Colliders = lens (nameof this.Colliders) this this.GetColliders this.SetColliders
        member this.GetSightBlockers world : Collider array = this.Get (nameof this.SightBlockers) world
        member this.SetSightBlockers (value : Collider array) world = this.Set (nameof this.SightBlockers) value world
        member this.SightBlockers = lens (nameof this.SightBlockers) this this.GetSightBlockers this.SetSightBlockers

        /// The entity the area the player's in is declared under, in the scene group.
        member this.GetArea world =
            Simulants.GameplayArea (this.GetAreaType world).Name

        /// What's declared in the area the player's in.
        member this.GetAreaEntities world =
            let area = this.GetArea world
            if area.GetExists world then Array.ofSeq (area.GetChildren world) else [||]

        (* Time *)

        /// Whether the player and the creatures act - only while exploring, with time passing.
        member this.GetExploring (world : World) =
            world.TimeAdvancing && this.GetMode world = Exploring

        (* Sound *)

        /// Play a sound from somewhere in the area - panned by where it is through the eye, fainter the further it is
        /// from the player.
        member this.PlaySoundAt volume (position : Vector3) (sound : Sound AssetTag) (world : World) =
            if world.TimeAdvancing then
                let toward = position - world.Eye3dCenter
                let panning = if toward.MagnitudeSquared > 0.0001f then Vector3.Dot (Vector3.Normalize toward, world.Eye3dRotation.Right) * 0.8f else 0.0f
                let distance = (position - Simulants.GameplayPlayer.GetPosition world).Magnitude
                World.playSound 1.0f panning (volume / (1.0f + 0.12f * distance)) sound world

        /// Play one of several sounds from somewhere in the area, picked by chance keyed to the game seed, the spot and
        /// the moment, so a run plays the same sounds every time.
        member this.PlaySoundAtOneOf volume (position : Vector3) (sounds : Sound AssetTag array) world =
            let index =
                RandomKey.make (this.GetGameSeed world) |> RandomKey.withName "sound" |>
                RandomKey.withIndex (int world.ScaledTime.Updates) |> RandomKey.withIndex (int (position.X * 10.0f) + 1000 * int (position.Z * 10.0f)) |>
                RandomKey.toIndex sounds.Length
            this.PlaySoundAt volume position sounds[index] world

        (* Collision *)

        /// Whether nothing tall stands between two points on the floor of the area.
        member this.HasLineOfSight (start : Vector2) (stop : Vector2) world =
            this.GetSightBlockers world |> Array.forall (fun collider -> not (Collider.crosses start stop collider))

/// What a creature is and how it stands, and the event it raises on itself.
[<AutoOpen>]
module CreatureDispatcherExtensions =
    type Entity with
        member this.GetCreatureType world : CreatureType = this.Get (nameof this.CreatureType) world
        member this.SetCreatureType (value : CreatureType) world = this.Set (nameof this.CreatureType) value world
        member this.CreatureType = lens (nameof this.CreatureType) this this.GetCreatureType this.SetCreatureType
        member this.GetCreatureState world : CreatureState = this.Get (nameof this.CreatureState) world
        member this.SetCreatureState (value : CreatureState) world = this.Set (nameof this.CreatureState) value world
        member this.CreatureState = lens (nameof this.CreatureState) this this.GetCreatureState this.SetCreatureState
        member this.GetHitPoints world : int = this.Get (nameof this.HitPoints) world
        member this.SetHitPoints (value : int) world = this.Set (nameof this.HitPoints) value world
        member this.HitPoints = lens (nameof this.HitPoints) this this.GetHitPoints this.SetHitPoints
        member this.GetNextAttack world : GameTime = this.Get (nameof this.NextAttack) world
        member this.SetNextAttack (value : GameTime) world = this.Set (nameof this.NextAttack) value world
        member this.NextAttack = lens (nameof this.NextAttack) this this.GetNextAttack this.SetNextAttack
        member this.GetWillRise world : bool = this.Get (nameof this.WillRise) world
        member this.SetWillRise (value : bool) world = this.Set (nameof this.WillRise) value world
        member this.WillRise = lens (nameof this.WillRise) this this.GetWillRise this.SetWillRise
        member this.GetRisen world : bool = this.Get (nameof this.Risen) world
        member this.SetRisen (value : bool) world = this.Set (nameof this.Risen) value world
        member this.Risen = lens (nameof this.Risen) this this.GetRisen this.SetRisen
        member this.GetLeapLanded world : bool = this.Get (nameof this.LeapLanded) world
        member this.SetLeapLanded (value : bool) world = this.Set (nameof this.LeapLanded) value world
        member this.LeapLanded = lens (nameof this.LeapLanded) this this.GetLeapLanded this.SetLeapLanded
        member this.GetLastCry world : GameTime = this.Get (nameof this.LastCry) world
        member this.SetLastCry (value : GameTime) world = this.Set (nameof this.LastCry) value world
        member this.LastCry = lens (nameof this.LastCry) this this.GetLastCry this.SetLastCry
        member this.GetEnraged world : bool = this.Get (nameof this.Enraged) world
        member this.SetEnraged (value : bool) world = this.Set (nameof this.Enraged) value world
        member this.Enraged = lens (nameof this.Enraged) this this.GetEnraged this.SetEnraged
        member this.GetHeadBurstOpt world : (GameTime * Vector3) option = this.Get (nameof this.HeadBurstOpt) world
        member this.SetHeadBurstOpt (value : (GameTime * Vector3) option) world = this.Set (nameof this.HeadBurstOpt) value world
        member this.HeadBurstOpt = lens (nameof this.HeadBurstOpt) this this.GetHeadBurstOpt this.SetHeadBurstOpt
        member this.AttackEvent = Events.AttackEvent --> this

        /// Whether a creature's up on its feet and in the fight.
        member this.GetUp world =
            match this.GetCreatureState world with
            | Dormant | Rising _ | Downed _ | Dead _ -> false
            | Waiting | Chasing | Grabbing _ | Swiping _ | Crouching _ | Leaping _ | Staggered _ -> true

        /// Whether a creature's alive, up or not.
        member this.GetAlive world =
            match this.GetCreatureState world with
            | Dead _ -> false
            | _ -> true

        /// Whether a creature's lying on the floor, alive - where only a gun aimed down hits it.
        member this.GetLow world =
            match this.GetCreatureState world with
            | Dormant | Downed _ -> true
            | _ -> false

/// A creature - waiting until it notices the player, then coming for it and attacking in its own way:
/// a shambler grabs and bites, a hound crouches and leaps, a brute swipes and charges - all the faster once it's driven
/// to rage. It keeps its state on itself,
/// walks the floor among the area's colliders, and raises its attacks on itself for the gameplay screen to land - which
/// lands the player's hits on it in turn, a shambler shot dead in the head losing its head in a burst.
type CreatureDispatcher () =
    inherit Entity3dDispatcherImSim (false, false, false)

    /// How a creature fights, by what it is.
    static let getData (entity : Entity) world =
        Data.Creatures[entity.GetCreatureType world]

    /// How much faster than its wont a creature comes on and strikes - the faster once it's driven to rage.
    static let getPace (entity : Entity) world =
        if entity.GetEnraged world then Constants.Gameplay.EnragedPace else 1.0f

    /// A yaw turned a step toward another, the short way round, stopping on it.
    static let turnToward (yaw : single) (goal : single) step =
        let difference = atan2 (sin (goal - yaw)) (cos (goal - yaw))
        if abs difference <= step then goal else yaw + MathF.CopySign (step, difference)

    /// Turn a creature a step toward a direction, at some rate of its turning.
    static let face delta (toward : Vector2) rate (entity : Entity) world =
        entity.SetYaw (turnToward (entity.GetYaw world) (Math.YawOf toward) (Constants.Gameplay.CreatureTurnRate * rate * delta)) world

    /// The cry a creature gives noticing the player, coming on, or gathering itself to spring or charge.
    static let cry (entity : Entity) world =
        let sound = (getData entity world).CrySound
        entity.Screen.PlaySoundAtOneOf sound.Volume (entity.GetPosition world) sound.Sounds world

    /// The sound of a creature's attack landing or sweeping through.
    static let strike (entity : Entity) world =
        let sound = (getData entity world).AttackSound
        entity.Screen.PlaySoundAtOneOf sound.Volume (entity.GetPosition world) sound.Sounds world

    /// Walk a step toward a point, sliding along whatever's in the way, among the others of the area.
    static let walk delta speed (entity : Entity) world =
        let data = getData entity world
        let position = Collider.toFloor (entity.GetPosition world)
        let step = Math.YawFacing (entity.GetYaw world) * (speed * delta)
        let player = Simulants.GameplayPlayer
        let others =
            [|yield (Collider.toFloor (player.GetPosition world), Constants.Gameplay.PlayerRadius)
              for other in entity.Screen.GetAreaEntities world do
                if other <> entity && other.Is<CreatureDispatcher> world && other.GetUp world then
                    yield (Collider.toFloor (other.GetPosition world), Data.Creatures[other.GetCreatureType world].Radius)|]
        let point = Collider.settle data.Radius (position + step) (entity.Screen.GetColliders world) others
        entity.SetPosition (v3 point.X 0.0f point.Y) world

    /// How a creature's puppet poses now - its animation, how far into it, and how high it's lifted, as a dog mid-leap.
    static let getPose (entity : Entity) world =
        let character = Creature (entity.GetCreatureType world)
        let data = getData entity world
        match entity.GetCreatureState world with
        | Dormant -> (SprawlAnimation, 0.5f, 0.0f) // lying still, face down
        | Rising start -> (SprawlAnimation, Puppet.getMarkTime Data.Puppets[character] AnimationType.Sprawl Mark.Release + (world.LocalTime start).SecondsF, 0.0f)
        | Waiting -> (IdleAnimation, 0.0f, 0.0f)
        | Chasing -> (WalkAnimation, world.ScaledTime.SecondsF * data.ChaseSpeed * getPace entity world / Constants.Gameplay.WalkSpeed, 0.0f)
        | Grabbing start -> (GrabAnimation, (world.LocalTime start).SecondsF, 0.0f)
        | Swiping start -> (SwingAnimation, (world.LocalTime start).SecondsF, 0.0f)
        | Crouching start -> (CrouchAnimation, (world.LocalTime start).SecondsF, 0.0f)
        | Leaping (start, from, toward) ->
            let speed = data.Behavior.LungeSpeed
            let progress = min 1.0f ((world.LocalTime start).SecondsF * speed / max 0.0001f ((toward - from).Length ()))
            let height = if data.Behavior = Hound then 0.5f else 0.0f
            (TackleAnimation, (world.LocalTime start).SecondsF, 4.0f * height * progress * (1.0f - progress))
        | Staggered start -> (HurtAnimation, (world.LocalTime start).SecondsF, 0.0f)
        | Downed start -> (SprawlAnimation, (world.LocalTime start).SecondsF, 0.0f)
        | Dead start -> (WoundAnimation, (world.LocalTime start).SecondsF, 0.0f)

    static member Properties =
        [define Entity.CreatureType Patient
         define Entity.CreatureState Waiting
         define Entity.HitPoints 50
         define Entity.Yaw 0.0f
         define Entity.NextAttack GameTime.zero
         define Entity.WillRise false
         define Entity.Risen false
         define Entity.LeapLanded false
         define Entity.LastCry GameTime.zero
         define Entity.Enraged false
         define Entity.HeadBurstOpt None
         define Entity.Size (v3 1.0f 2.0f 1.0f)
         define Entity.Offset (v3 0.0f 0.9f 0.0f)]

    override this.Process (entity, world) =

        // act, while play goes on
        if entity.Screen.GetExploring world then

            // answer the creature's intent
            let delta = single world.ScaledDelta.SecondsF
            let data = getData entity world
            let character = Creature (entity.GetCreatureType world)
            let now = world.ScaledTime
            let player = Simulants.GameplayPlayer
            let playerAt = Collider.toFloor (player.GetPosition world)
            let position = Collider.toFloor (entity.GetPosition world)
            let toPlayer = playerAt - position
            let distance = toPlayer.Length ()
            let playerDown = (player.GetAction world).IsDying
            let pace = getPace entity world
            let cooldown = data.AttackCooldown / pace
            match entity.GetCreatureState world with
            | Dead _ ->
                ()
            | Dormant ->
                // stirring as the player steps near
                if distance < Constants.Gameplay.DormantWakeDistance then
                    entity.SetCreatureState (Rising now) world
                    cry entity world
            | Rising start ->
                // heaving up off the floor - the tail of its sprawl, from where it starts to rise
                let riseStart = Puppet.getMarkTime Data.Puppets[character] AnimationType.Sprawl Mark.Release
                let riseEnd = Puppet.getMarkTime Data.Puppets[character] AnimationType.Sprawl Mark.End
                if (world.LocalTime start).SecondsF >= riseEnd - riseStart then entity.SetCreatureState Chasing world
            | Downed start ->
                if (world.LocalTime start).SecondsF >= Puppet.getMarkTime Data.Puppets[character] AnimationType.Sprawl Mark.End then
                    entity.SetCreatureState Chasing world
            | Staggered start ->
                if (world.LocalTime start).SecondsF >= Constants.Gameplay.StaggerDuration then
                    entity.SetCreatureState Chasing world
            | Waiting ->
                // noticing the player when it comes into view, or near enough to be heard
                let running = (player.GetWalkStartOpt world).IsSome && player.GetStepRate world > 1.5f
                let hearing = if running then Constants.Gameplay.RunHearingDistance else Constants.Gameplay.HearingDistance
                let sees =
                    distance < data.SightDistance &&
                    Vector2.Dot (toPlayer / max distance 0.0001f, Math.YawFacing (entity.GetYaw world)) > data.SightCone &&
                    entity.Screen.HasLineOfSight position playerAt world
                if not playerDown && (sees || distance < hearing) then
                    entity.SetCreatureState Chasing world
                    cry entity world
            | Grabbing start ->
                // lunging - seizing the player if it's still in reach as the grab begins, biting as the bite lands, then
                // letting go
                let seizing = (world.LocalTime start).SecondsF < Constants.Gameplay.GrabSeizeWindow
                let holding = match player.GetAction world with Grabbed (holder, _) -> holder = entity | _ -> false
                if seizing && not holding && distance < data.AttackReach + data.Radius + Constants.Gameplay.PlayerRadius + Constants.Gameplay.AttackSlack then
                    World.publish { AttackDamage = 0; Seizes = true; Attacker = entity } entity.AttackEvent entity world
                let impact = Puppet.getMarkTime Data.Puppets[character] AnimationType.Grab Mark.Impact
                if holding && (world.LocalTime start).SecondsF - delta < impact && (world.LocalTime start).SecondsF >= impact then
                    World.publish { AttackDamage = data.AttackDamage; Seizes = false; Attacker = entity } entity.AttackEvent entity world
                    strike entity world
                if (world.LocalTime start).SecondsF >= Puppet.getMarkTime Data.Puppets[character] AnimationType.Grab Mark.End then
                    entity.SetNextAttack (now + GameTime.ofSeconds (double cooldown)) world
                    entity.SetCreatureState Chasing world
                else face delta toPlayer 1.0f entity world
            | Swiping start ->
                let sweep = Puppet.getMarkTime Data.Puppets[character] AnimationType.Swing Mark.Sweep
                if (world.LocalTime start).SecondsF - delta < sweep && (world.LocalTime start).SecondsF >= sweep then
                    strike entity world
                    if distance < data.AttackReach + data.Radius + Constants.Gameplay.PlayerRadius + Constants.Gameplay.AttackSlack &&
                       (distance < 0.5f || Vector2.Dot (toPlayer / distance, Math.YawFacing (entity.GetYaw world)) > 0.3f) then
                        World.publish { AttackDamage = data.AttackDamage; Seizes = false; Attacker = entity } entity.AttackEvent entity world
                if (world.LocalTime start).SecondsF >= Puppet.getMarkTime Data.Puppets[character] AnimationType.Swing Mark.End then
                    entity.SetNextAttack (now + GameTime.ofSeconds (double cooldown)) world
                    entity.SetCreatureState Chasing world
            | Crouching start ->
                // gathering to spring, eyes on the player - then flying at where it stands, and on past it
                face delta toPlayer 2.0f entity world
                let (windup, reach) =
                    match data.Behavior with
                    | Brute -> (Constants.Gameplay.ChargeWindup, Constants.Gameplay.ChargeReach)
                    | Shambler | Hound -> (Constants.Gameplay.LeapWindup, Constants.Gameplay.LeapReach)
                if (world.LocalTime start).SecondsF >= windup then
                    let direction = if distance > 0.0001f then toPlayer / distance else Math.YawFacing (entity.GetYaw world)
                    let length = min (distance + 1.0f) (reach + 1.0f)
                    entity.SetCreatureState (Leaping (now, position, position + direction * length)) world
                    entity.SetYaw (Math.YawOf direction) world
                    entity.SetLeapLanded false world
                    if data.Behavior = Hound then strike entity world
            | Leaping (start, from, toward) ->
                let speed = data.Behavior.LungeSpeed
                let length = (toward - from).Length ()
                let progress = min 1.0f ((world.LocalTime start).SecondsF * speed / max length 0.0001f)
                let intended = Vector2.Lerp (from, toward, progress)
                let point = Collider.slide data.Radius position intended (entity.Screen.GetColliders world) []
                let blocked = (point - intended).Length () > 0.05f
                entity.SetPosition (v3 point.X 0.0f point.Y) world
                if not (entity.GetLeapLanded world) && (point - playerAt).Length () < data.Radius + Constants.Gameplay.PlayerRadius + 0.2f then
                    World.publish { AttackDamage = data.AttackDamage; Seizes = false; Attacker = entity } entity.AttackEvent entity world
                    entity.SetLeapLanded true world
                if progress >= 1.0f || blocked then
                    entity.SetNextAttack (now + GameTime.ofSeconds (double cooldown)) world
                    entity.SetCreatureState Chasing world
            | Chasing ->
                if not playerDown then
                    face delta toPlayer 1.0f entity world
                    let ready = now >= entity.GetNextAttack world
                    let free = match player.GetAction world with Grabbed _ | Dying _ | Reeling _ -> false | _ -> true
                    let reach = data.AttackReach + data.Radius + Constants.Gameplay.PlayerRadius
                    match data.Behavior with
                    | Shambler when ready && free && distance < reach -> entity.SetCreatureState (Grabbing now) world
                    | Hound when ready && distance < Constants.Gameplay.LeapReach ->
                        entity.SetCreatureState (Crouching now) world
                        cry entity world
                    | Brute when ready && distance < reach -> entity.SetCreatureState (Swiping now) world
                    | Brute when ready && distance > Constants.Gameplay.ChargeMinDistance && distance < Constants.Gameplay.ChargeReach && now >= entity.GetNextAttack world + GameTime.ofSeconds (double (Constants.Gameplay.ChargeCooldown / pace)) ->
                        entity.SetCreatureState (Crouching now) world
                        cry entity world
                    | _ ->
                        // coming on, stopping just short of the player, crying out now and again
                        if distance > reach * 0.85f then walk delta (data.ChaseSpeed * pace) entity world
                        let period = match data.Behavior with Shambler -> 5.0f | Hound -> 4.0f | Brute -> 9.0f
                        if (world.LocalTime (entity.GetLastCry world)).SecondsF > period then
                            let jitter = RandomKey.make (entity.Screen.GetGameSeed world) |> RandomKey.withName "cry" |> RandomKey.withName entity.Name |> RandomKey.withIndex (int now.SecondsF) |> RandomKey.toSingle
                            entity.SetLastCry (now + GameTime.ofSeconds (double (jitter * 2.0f))) world
                            cry entity world

        // its body
        let (animation, localTime, lift) = getPose entity world
        World.doEntity<PuppetDispatcher> "Puppet"
            [Entity.CharacterType .= Creature (entity.GetCreatureType world)
             Entity.Size .= fst (Puppet.getBounds Data.Puppets[Creature (entity.GetCreatureType world)])
             Entity.Offset .= snd (Puppet.getBounds Data.Puppets[Creature (entity.GetCreatureType world)])
             Entity.Position @= entity.GetPosition world + v3Up * lift
             Entity.Yaw @= entity.GetYaw world
             Entity.CharacterAnimation @= animation
             Entity.PuppetTime @= localTime
             Entity.IdleTime @= world.ScaledTime.SecondsF + (RandomKey.make 0UL |> RandomKey.withName "idle" |> RandomKey.withName entity.Name |> RandomKey.toSingle) * 7.0f
             Entity.Enemy .= true
             Entity.Headless @= (entity.GetHeadBurstOpt world).IsSome
             Entity.MountOpt .= None
             Entity.Pickable .= false] world

        // its head bursting, if it was shot off
        match entity.GetHeadBurstOpt world with
        | Some (start, origin) ->
            World.doEntity<HeadBurstDispatcher> "HeadBurst"
                [Entity.BurstStart .= start
                 Entity.Position .= origin
                 Entity.FloorHeight .= (entity.GetPosition world).Y
                 Entity.MountOpt .= None
                 Entity.Pickable .= false] world
        | None -> ()
