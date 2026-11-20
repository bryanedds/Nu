// Zombie Game.
// Copyright (C) Bryan Edds.

namespace ZombieGame
open System
open System.Numerics
open Prime
open Nu

/// How a creature fights.
type Behavior =
    | Shambler // comes on slow, grabs and bites
    | Hound // runs, crouches and leaps
    | Brute // swipes, and charges from afar

    /// How fast a creature of the behavior lunges - a brute charging, others leaping.
    member this.LungeSpeed =
        match this with
        | Brute -> Constants.Gameplay.ChargeSpeed
        | Shambler | Hound -> Constants.Gameplay.LeapSpeed

/// A creature type, each with its puppet file named for it.
type CreatureType =
    | Patient // a patient risen in a gown
    | Nurse
    | Orderly // a big orderly, slower and harder to put down
    | Dog // the security patrol's dog
    | Surgeon // the swollen thing waiting in the ambulance bay
    | Student // a student risen in what they slept in
    | Sprinter // a runner from the track team, still fast, that crouches and pounces
    | Linebacker // the football team's linebacker, swollen huge in his pads, waiting in the gym
    | Worker // a line worker risen in his apron and boots
    | Butcher // a butcher still swinging his cleaver
    | Frozen // a worker frozen stiff in cold storage, slow and hard to put down
    | Sow // a sow from the pens, swollen to monstrous size, that charges
    | MeatKing // the giant mass of rotten meat waiting in the pit, a cleaver in each hand

/// A creature type - its name, how it fights, its health, how fast it walks and chases, how far and wide it sees
/// (the cosine of the half angle of its sight), the harm its attack does, how near it must be to attack and the seconds
/// between attacks, its chance of rising again once felled, how wide it is, the share of its health left at which it's
/// driven to rage - none, if 0 - and the sounds of its crying out, its attacking and its dying.
type CreatureData =
    { CreatureType : CreatureType
      CreatureName : string
      Behavior : Behavior
      HitPoints : int
      WalkSpeed : single
      ChaseSpeed : single
      SightDistance : single
      SightCone : single
      AttackDamage : int
      AttackReach : single
      AttackCooldown : single
      RiseChance : single
      Radius : single
      EnrageAt : single
      CrySound : SoundType
      AttackSound : SoundType
      DeathSound : SoundType }

/// A character - the player, or a creature of a type - each with its puppet file named for it.
type CharacterType =
    | Player
    | Creature of CreatureType

    /// The character's name, as its puppet file is named.
    member this.Name =
        match this with
        | Player -> "Player"
        | Creature creatureType -> getCaseName creatureType