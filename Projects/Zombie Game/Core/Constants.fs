// Zombie Game.
// Copyright (C) Bryan Edds.

namespace ZombieGame
open System
open System.IO
open System.Numerics
open Prime
open Nu

/// The game's tuning values that aren't data, by subject.
[<RequireQualifiedAccess>]
module Constants =

    /// Puppet tuning.
    [<RequireQualifiedAccess>]
    module Puppet =

        let StepRate = 1.7f // strides per second at walking speed, so the feet keep pace with the floor

    /// Scenery tuning - walls, the eye under ceilings, and glowing parts.
    [<RequireQualifiedAccess>]
    module Scenery =

        let WallHeight = 3.0f
        let CameraCeilingDistance = 0.25f // how far under a ceiling every area's eye stands, so the ceiling doesn't fill the view
        let WallThickness = 0.3f
        let DadoHeight = 1.0f // the band of darker paint along the walls' feet
        let DoorwayDadoTuck = 0.02f // how far past a doorway's sides the dado's break in it reaches, so its ends hide in the frame
        let PartGlow = 6.0f // above 1, so forward rendered - deferred caps emission at 1

    /// Gui layout and colors shared across the screens.
    [<RequireQualifiedAccess>]
    module Gui =

        let SmallFontSizing = Some 6.0f
        let LineLengthMax = 125 // the most characters a line read on screen can hold before it's clipped
        let SlotColumns = 2
        let KeypadKeys = [|"1"; "2"; "3"; "4"; "5"; "6"; "7"; "8"; "9"; "C"; "0"; "OK"|] // left to right, top to bottom
        let DimColor = Color (0.62f, 0.6f, 0.56f, 1.0f)
        let BrightColor = Color (1.0f, 0.86f, 0.5f, 1.0f)
        let Elevation = 10.0f // above everything in play
        let CreditsSpeed = 34.0f // how fast the credits roll up the screen
        let CreditsSpacing = 20.0f // from one line of the credits to the next

    /// Gameplay tuning - the game, the inventory, the player's controls, aiming and shooting, examining and doors, creatures, the power and the light.
    [<RequireQualifiedAccess>]
    module Gameplay =

        // the game
        let FixedGameSeed = 0UL // the game seed for the editor and tests, turning cross-game variation off
        let SaveFilePath = Path.Combine (AppContext.BaseDirectory, "ZombieGame.save")

        // the inventory
        let SlotCount = 6 // how many slots the player has

        // the player's tank controls - up walks the way it faces, down backs up, left and right turn it on the spot
        let WalkSpeed = 1.9f // meters per second
        let RunSpeed = 3.6f
        let BackSpeed = 1.1f
        let DangerSpeedScalar = 0.6f // limping, badly hurt
        let TurnRate = 3.2f // radians per second, turning on the spot
        let RunTurnRate = 3.8f
        let QuickTurnDuration = 0.35f // seconds a quick turn takes to spin the player round
        let PlayerRadius = 0.28f
        let PlayerHitPoints = 100
        let CautionHitPoints = 66 // at or below this the player's in caution, holding its side
        let DangerHitPoints = 25 // at or below this the player's in danger, limping
        let IntentDeadZone = 0.1f // how far an intent's axis must lean before it counts

        // aiming and shooting
        let AimRaiseDuration = 0.2f // seconds raising a gun takes before it can fire
        let AimTurnRate = 1.6f // radians per second, turning while aiming
        let AimTurnDuration = 0.3f // seconds auto-aim takes to turn the aim onto its mark
        let PelletSpread = 0.12f // radians either side a shot's pellets fan out
        let PelletCloseRange = 2.0f // metres to a creature within which a pellet aimed up at its head hits harder
        let PelletCloseHeadFactor = 2 // how many times harder a pellet hits a head from up close
        let ShotFlashDuration = 0.08f
        let ShootDuration = 0.35f // seconds the gun's kick shows after a shot
        let AimTilt = 0.5f // how far the aim must tip up or down to count as aimed up or down
        let AimUpShown = 0.6f // how far up an aim up is drawn, as a share of the way - the hit is the same
        let AimDownShown = 0.6f // how far down a gun aimed down is drawn, likewise - a knife held low isn't

        // examining and doors
        let InteractDistance = 1.1f // how near something must be before the player to examine it
        let InteractCone = 0.5f // the cosine of the half angle before the player it must lie within
        let DoorDuration = 2.6f // seconds a door takes to creak open and the dark beyond to give way to the next area
        let TravelDuration = 6.0f // seconds travelling on to another stage takes, its caption shown in the dark
        let DoorArrivalDistance = 0.9f // how far before a door one arrives through it, facing away from it
        let FallenDuration = 2.5f // seconds play lingers on the fallen player before the game's over
        let EscapedDuration = 1.0f // seconds play lingers on the ambulance pulling away before the ending
        let DoorScenePosition = v3 0.0f -60.0f 0.0f // where the door opening plays, far beneath the areas
        let DoorSceneFieldOfView = 50.0f // degrees
        let DoorSceneHoleTuck = 0.01f // how far past the doorway the door scene's hole in its wall reaches, so its edges hide behind the door's frame
        let WoundPause = 1.2f // seconds the fallen player lies still before play lingers on its fall
        let ControlsHintDuration = 45.0f // seconds of play the controls show for in the first area

        // creatures
        let SightBlockHeight = 1.5f // what stands at least this tall blocks sight, as a wall or cabinet
        let HearingDistance = 2.2f // a creature hears the player this near, whichever way it faces
        let RunHearingDistance = 5.0f // and running, farther
        let ShotHearingDistance = 12.0f // and a shot fired, across any area
        let DormantWakeDistance = 2.4f // a creature lying dormant stirs as the player steps this near
        let ArrivalClearance = 3.5f // how far from where one comes into an area a creature on its feet starts, so the player has time to react
        let HoundArrivalClearance = 5.0f // and one that springs, farther
        let LyingArrivalClearance = 3.0f // and one lying dormant, nearer, since it must stir and rise first
        let CreatureTurnRate = 2.6f
        let StaggerDuration = 0.3f // seconds a creature reels from a hit before coming on again
        let HeadBurstBits = 16 // how many bits fly from a head shot off - of blood, then flesh, then bone
        let HeadBurstBloodBits = 10
        let HeadBurstBoneBits = 2
        let HeadBurstGravity = 9.8f // how fast the bits fall, in metres a second each second
        let HeadBurstPuffDuration = 0.15f // seconds the spray of blood from a head shot off swells and is gone
        let HeadBurstPuffSize = 0.4f // how wide the spray is at its widest
        let HeadBurstBloodColor = Color (0.3f, 0.02f, 0.02f, 1.0f)
        let HeadBurstFleshColor = Color (0.4f, 0.16f, 0.13f, 1.0f)
        let HeadBurstBoneColor = Color (0.85f, 0.82f, 0.74f, 1.0f)
        let NeckStumpSize = v3 0.1f 0.05f 0.1f // the bloody stump of a neck whose head's been shot off
        let NeckStumpColor = Color (0.36f, 0.03f, 0.03f, 1.0f)
        let GrabSeizeWindow = 0.4f // seconds into a grab it can still seize the player
        let FreedGrace = 3.0f // seconds after breaking free of a grab that nothing can seize the player again
        let HurtGrace = 2.0f // seconds after a reel's played out that no blow can touch the player
        let LeapReach = 3.2f // how far off a dog springs from
        let LeapSpeed = 7.5f // meters per second a dog flies at
        let LeapWindup = 0.35f // seconds a dog crouches before springing
        let ChargeReach = 6.0f // how far off the surgeon charges from
        let ChargeSpeed = 6.5f
        let ChargeWindup = 0.7f
        let ChargeCooldown = 5.0f // seconds between the surgeon's charges
        let ChargeMinDistance = 3.0f // nearer than this, the surgeon swipes rather than charges
        let AttackSlack = 0.3f // how far past its reach a creature's blow still lands
        let EnragedPace = 1.5f // how much faster a creature driven to rage comes on and strikes

        // the power - what runs on it shines dimly and flickers until it's restored
        let UnpoweredScale = 0.45f // how much of its light what runs on the power gives before then
        let FlickerRate = 7.0f
        let StutterThreshold = 0.85f // how often the flicker stutters - the higher, the rarer
        let StutterScale = 0.15f // how far a stutter dims it

        // lighting
        let AmbientColor = Color (0.78f, 0.84f, 1.0f, 1.0f) // a cold cast to the dark
        let LightingConfig =
            { Lighting3dConfig.defaultConfig with
                LightCutoffMargin = 0.5f
                LightExposure = 0.8f
                ToneMapType = LottesToneMap
                ToneMapSlope = v3Dup 1.05f
                ToneMapPower = v3Dup 1.15f
                ToneMapSaturation = 0.8f // drained, as fluorescent light at night
                ToneMapWhitePoint = 4.0f
                FogEnabled = true
                FogType = ExponentialSquaredFog
                FogDensity = 0.012f
                FogColor = Color (0.04f, 0.05f, 0.06f, 1.0f)
                SsrlIntensity = 0.15f
                SsaoEnabled = true
                SsaoIntensity = 2.0f
                SsaoBias = 0.01f
                SsaoRadius = 0.25f
                SsaoDistanceMax = 0.05f
                ChromaticAberrationEnabled = true }
