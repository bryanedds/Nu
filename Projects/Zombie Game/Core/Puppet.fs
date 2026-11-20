// Zombie Game.
// Copyright (C) Bryan Edds.

namespace ZombieGame
open System
open System.Numerics
open Prime
open Nu

/// A side of a body, as of a wing or a spider's leg.
type Side =
    | LeftSide
    | RightSide

/// The bones of a puppet, of every body plan - each plan builds the ones it has, parents first. Bones a body has many
/// of are indexed, counting out from the body.
type Bone =
    | Pelvis
    | Torso
    | Head
    | ArmLeft
    | ArmRight
    | HandLeft
    | HandRight
    | LegLeft
    | LegRight
    | Tail of int // a beast's tail, its segments counted from the root
    | Wing of Side * int // a beast's wing, its segments counted from the shoulder
    | Abdomen // a spider's
    | SpiderLeg of Side * int * int // a spider's leg - which pair, front to back, and which segment, from the body
    | Spine of int // a snake's length, its segments counted from the head
    | Jaw // a snake's

/// The runs of bones a body plan declares, so a pose can turn a whole run at once.
type Chain =
    | TailChain
    | SpineChain
    | WingChain of Side
    | LegChain of Side * int
    | Legs

/// A step of a turn, in degrees, as the puppet code's own turns - a turn is a list of steps, multiplied in the order
/// written, as `lean a * spread b` is written `[[Lean a] [Spread b]]`.
type Turn =
    | Twist of single // about the vertical
    | Lean of single // an upright bone forward (positive) or back
    | Swing of single // a hanging limb forward and up (positive) or back
    | Spread of single // a limb out to its side (positive raises the left limb outward)

/// The color and surface of a standard limb.
type Clothing =
    string * SurfaceType // a palette name and a surface

/// The colors and surfaces of a biped's standard limbs - its arms, hands, legs and feet.
[<SymbolicExpansion>]
type ClothesData =
    { Arms : Clothing
      Hands : Clothing
      Legs : Clothing
      Feet : Clothing }

/// A two-legged body, as a person's - its proportions in meters, before the puppet's scale.
[<SymbolicExpansion>]
type BipedPlan =
    { Legs : single // from the hips to the ground
      Torso : single // from the hips' top to the neck
      Shoulders : single // from the middle out to each shoulder
      Hips : single // from the middle out to each hip
      Arms : single // from each shoulder to its hand
      Limbs : single // how thick its standard limbs are
      Clothes : ClothesData option } // its standard limbs, if it has them rather than parts of its own

/// A tail of bones, each hanging from the one before.
[<SymbolicExpansion>]
type TailPlan =
    { Segments : int
      Root : Vector3 // where the first segment hangs from the hips
      Step : Vector3 } // from each segment's root to the next's

/// A four-legged body, as a beast's - its hips at the back of its body, its body reaching forward along +z to its
/// shoulders, its head up and ahead, its front legs (the arm bones) hanging from its shoulders and its hind legs from
/// its hips, all of a length so all four feet stand on the ground.
[<SymbolicExpansion>]
type QuadrupedPlan =
    { Legs : single
      Body : single // from the hips to the shoulders
      Shoulders : single
      Hips : single
      HeadRise : single // the head's pivot above the body's
      HeadForward : single // and ahead of it
      Tail : TailPlan option }

/// The body a puppet is built on, with its proportions.
type BodyPlan =
    | Biped of BipedPlan
    | Quadruped of QuadrupedPlan

/// A primitive shape attached to a bone, as a puppet file states it.
[<SymbolicExpansion>]
type PuppetPartData =
    { Bone : Bone
      Shape : PartShapeType
      At : Vector3 // from the bone's pivot
      Size : Vector3
      Turn : Turn list
      Color : string // a palette name
      Surface : SurfaceType // Cloth if not given
      Emission : single // a faint sheen, or with a Light surface, a glow
      Pulse : Pulse option // its glow swelling and fading, as burning eyes
      Mirror : bool } // has a twin on the other side, mirrored across the body's middle

/// The key poses a pose set can hold, named by what they're for - each body plan's set has its own version of those
/// it uses, as a beast's Coil and a person's.
type PoseType =
    | Stance // on guard, as a creature's stand while it waits
    | Coil // a spring's or a blow's anticipation, wound and sunk
    | Leap // in the air, flung headlong
    | Cut // a blow landed, the arm thrown across the body
    | Aim // a gun held out level before the body in both hands
    | AimUp // a gun held out aimed up, as at a tall creature's head
    | AimDown // a gun held out aimed down, as at a dog or a crawling creature
    | Ready // a knife held out level before the body in one hand, the other come across to guard
    | ReadyUp // a knife held out high, as at a tall creature's head
    | ReadyDown // a knife held out low, as at a dog or a crawling creature
    | Struck // knocked back by a blow
    | Grab // lunged in, arms around a foe
    | Felled // fallen
    | Prone // face down, arms flung out ahead
    | Whirling // the weapon arm held straight out to the side, as through a slash
    | Relaxed // at ease
    | Bite // a beast's blow, head driven down into the foe

/// The cycles a pose set can hold - poses that move with a phase, as a stride.
type CycleType =
    | Stride // a step of a walk, a full cycle two steps
    | Swish // a tail's slow swish from side to side, laid over whatever else the body does
    | Breath // a beast's slow, heavy breaths, its head rising and falling with them

/// The shape a swaying value follows over a cycle's phase.
type Wave =
    | Sine // rising and falling about its base
    | Lift // rising from its base on the first half of the cycle, resting on it the second
    | Rebound // rising from its base on both halves, as a bounce

/// A value swaying over a cycle - Base + Amplitude * Wave (phase + Offset), the offset in degrees of phase.
[<SymbolicExpansion>]
type Sway =
    { Base : single
      Amplitude : single
      Wave : Wave // Sine if not given
      Offset : single }

/// The axis of a step of a cycle's turn, as a still turn's steps.
type Axis =
    | Twisting
    | Leaning
    | Swinging
    | Spreading

/// A still key pose - its root's offset in the puppet's own space (+z its front, +x its left), how far it's fallen
/// forward about its feet (in degrees), its bones' turns, and its chains' - each bone of a chain turned alike, so a
/// curl of a tail is the sum of its bones'. Bones it leaves out take its set's rest turns; chains its body hasn't are
/// passed over.
[<SymbolicExpansion>]
type PoseData =
    { Root : Vector3
      Fall : single
      Turns : Map<Bone, Turn list>
      Chains : Map<Chain, Turn list> }

/// A cycle - its root's bob up and down (in meters), and its bones' and chains' turns, each step's angle (in degrees)
/// swaying. Bones it leaves out take its set's rest turns; chains its body hasn't are passed over.
[<SymbolicExpansion>]
type CycleData =
    { Bob : Sway option
      Turns : Map<Bone, (Axis * Sway) list>
      Chains : Map<Chain, (Axis * Sway) list> }

/// The pose sets, one per body plan, each a file in Assets/Gameplay named for its case.
type PoseSetType =
    | BipedPoses
    | QuadrupedPoses

/// A statement of a pose set, or of a puppet's own poses overriding its set's.
type PoseStatement =
    | Rest of Map<Bone, Turn list> // the turns of bones a pose leaves out
    | Pose of PoseType * PoseData
    | Cycling of CycleType * CycleData

/// The types of animation, one per case of CharacterAnimation without its arguments - an animation set has a timeline for
/// each.
[<RequireQualifiedAccess>]
type AnimationType =
    | Idle
    | Relax
    | Walk
    | WalkBack
    | Aim
    | Shoot
    | Ready
    | Swing
    | Hurt
    | Wound
    | Grab
    | Crouch
    | Tackle
    | Sprawl

/// The moments of an animation the game acts on - when a bite lands, a shot fires, a slash sweeps, an animation is done.
[<RequireQualifiedAccess>]
type Mark =
    | Impact // a bite or a blow lands
    | Follow // a slash's sweep is through
    | Release // a shot fires
    | Sweep // a slash starts its sweep
    | End // the animation's work is done

/// How a blend, a move or an angle eases over its segment.
[<RequireQualifiedAccess>]
type Ease =
    | Smooth // easing in and out, as a smoothstep
    | Linear
    | In // starting slow
    | Out // finishing slow
    | InOut // starting and finishing slow, faster in the middle than Smooth
    | Snap // fast through the middle, as a blow
    | Pulse // there and back again
    | Late // smooth, but held back at the start

/// What decides between two targets - and how far, part way up or down blending them.
type Condition =
    | AimingUp // a gun's aimed up, as at a tall creature's head
    | AimingDown // a gun's aimed down, as at a dog or a crawling creature

/// How fast a cycle or sway turns over - per second of the animation, per step of a walk at the game's step rate, or
/// per its segment.
type Pace =
    | PerSecond of single
    | PerStep of single
    | PerSegment of single

/// What a segment blends from or toward.
type Target =
    | Idle // the animation set's idle, breathing and all
    | Still of PoseType
    | Cycled of CycleType * Pace
    | Sided of CycleType * Pace // a cycle toward the right, played mirrored when the animation goes left, as a side step
    | Between of PoseType * PoseType * Sway * Pace // the first blended toward the second by a swaying weight
    | Mixed of Target * Target * Ease // the first blended toward the second over its segment by an ease
    | Choose of Condition * Target * Target // the first if the condition holds, else the second - blended so far as it holds

/// A move's hop - a hop of a height, higher by a share of the distance moved, and none at all if it moves nowhere.
[<SymbolicExpansion>]
type HopData =
    { Hop : single
      HopPerMeter : single }

/// A move up or down to a height - meters, plus a share of the animation's height (as a slam's).
[<SymbolicExpansion>]
type RiseData =
    { Up : single
      OfHeight : single
      Ease : Ease }

/// A move once around the animation's center, bobbing as the body turns.
[<SymbolicExpansion>]
type CircleData =
    { Bob : single
      Bobs : single } // how many bobs while circling

/// How a segment carries the body, its root moving from wherever the last move left it.
type Move =
    | Travel of HopData // to the animation's offset, easing smoothly
    | Return of HopData // back home, easing smoothly
    | Rise of RiseData // up or down to a height
    | Circle of CircleData // around the animation's center and back to where it began
    | Sink of single // an enemy sinks into the ground this fast - an ally stays where it fell

/// An angle of the whole body - its fall, roll or spin - in degrees, eased from where the last segment left it (or
/// from a given angle) to another, added to the poses' own.
[<SymbolicExpansion>]
type AngleData =
    { From : single option
      To : single
      Ease : Ease }

/// The clock a layer's sway runs by.
[<RequireQualifiedAccess>]
type Clock =
    | Seconds // seconds into its segment, its rate in cycles per second
    | Progress // its segment's eased progress, its rate in cycles per segment
    | Idle // the idle time, its rate in cycles per second

/// What a waver sways.
type Channel =
    | OnRise // the torso's lift over the hips
    | OnRoot of Vector3 // the root, along a direction in the puppet's own space (+z its front, +x its left)
    | OnBone of Bone * Axis // a bone, turned about an axis in degrees after its pose's turn

/// A sway laid over a pose - its amplitude ramped in over a time, or faded out over its span.
[<SymbolicExpansion>]
type WaverData =
    { On : Channel
      Sway : Sway
      Rate : single
      Clock : Clock
      RampIn : single // seconds to reach its full amplitude
      Fade : bool } // fading its amplitude out over its span

/// A cycle laid over a pose, its bones turned as the cycle has them in place of the pose's.
[<SymbolicExpansion>]
type OverlayData =
    { Cycle : CycleType
      Rate : single
      Clock : Clock }

/// What a layer lays over a pose.
type LayerKind =
    | Waver of WaverData
    | Overlay of OverlayData

/// A layer, from the start of its segment for its span - its segment's, or the given seconds, or on to the end of the
/// animation and beyond, as the last segment's always does.
[<SymbolicExpansion>]
type LayerData =
    { Lay : LayerKind
      Over : single option
      Onward : bool }

/// A segment of an animation - a blend from a target (the last segment's target, if not given) toward another, over a
/// time, carrying the body along a move, with its angles, its layers and its marks, each at a share of its time.
[<SymbolicExpansion>]
type Segment =
    { To : Target
      From : Target option
      Over : single // seconds
      Reach : single option // seconds into the segment by which the blend arrives, holding after
      Ease : Ease // Smooth if not given
      Partway : single option // blends only this far toward its target
      Move : Move option
      Fall : AngleData option
      Roll : AngleData option
      Spin : AngleData option
      Layers : LayerData list
      Marks : (Mark * single) list }

/// The animation sets, one per body plan, each a file in Assets/Gameplay named for its case.
type AnimationSetType =
    | BipedAnimations
    | QuadrupedAnimations

/// A statement of an animation set, or of a puppet's own animations overriding its set's.
type AnimationStatement =
    | Animation of AnimationType * Segment list // a timeline for an animation type
    | Always of LayerData list // layers laid over every animation

/// An animation as it plays - its arguments, the character playing it and its idle time, as its timeline reads them.
type AnimationPlayback =
    { Forward : Vector3
      Enemy : bool // a creature, rather than the player
      Offset : Vector3 // where a move travels to, from where it starts
      Center : Vector3 // what a circling move circles, from where it starts
      Height : single // how high a rising move goes
      AimPitch : single // how a gun's aimed, up (1), level (0) or down (-1)
      Mirrored : bool // goes left rather than right
      IdleTime : single }

/// The animations a character can play.
type CharacterAnimation =
    | IdleAnimation // stand ready, as a creature waiting
    | RelaxAnimation // stand at ease, as the player does unarmed
    | WalkAnimation // walk, timed by how long the walk has gone on so the stride never hitches - a run is a walk stepped faster
    | WalkBackAnimation // walk backward - the walk played in reverse
    | AimAnimation of single // hold a gun out before the body, aimed up (1), level (0) or down (-1)
    | ShootAnimation of single // fire the gun aimed so, kicking back with it
    | ReadyAnimation of single // hold a knife out before the body, high (1), level (0) or low (-1)
    | SwingAnimation // a quick horizontal slash of the knife across the front, or a creature's swipe
    | HurtAnimation // reel from a blow or a shot
    | WoundAnimation // fall back and lie there, dead
    | GrabAnimation // lunge in with both arms, seize, and bite, as a creature that's caught the player
    | CrouchAnimation // gather low to spring, as a dog about to leap
    | TackleAnimation // dive headlong through the air, as a leaping dog or the hulk's charge
    | SprawlAnimation // drop face first and lie there a while before heaving back up, as a creature knocked down

/// A primitive shape placed in the world - a part of a scene's prop, or of a puppet.
type Part =
    { PartShape : PartShapeType
      PartPosition : Vector3
      PartRotation : Quaternion
      PartScale : Vector3
      PartColor : Color
      PartSurface : SurfaceType // what it's made of
      PartShadowed : bool // whether it casts a shadow; a thin sign needn't
      PartGlow : single // how brightly it glows, if it's made of light
      PartPowered : bool // its glow following the power - dim and flickering until it's restored
      PartUnreflected : bool } // drawn forward, so screen-space reflections leave it out, as a ceiling

    /// Whether the part's look changes from frame to frame - a glow following the power - so it can't be frozen with
    /// the rest of the scenery.
    member this.Dynamic =
        this.PartPowered

    /// How far the part reaches up and down from its middle, however it's turned.
    member this.HalfHeight =
        let rotation = Matrix4x4.CreateFromQuaternion this.PartRotation
        let half = this.PartScale * 0.5f
        abs rotation.M12 * half.X + abs rotation.M22 * half.Y + abs rotation.M32 * half.Z

    /// The highest the part reaches, however it's turned.
    member this.Top =
        this.PartPosition.Y + this.HalfHeight

    /// The lowest the part reaches, however it's turned.
    member this.Bottom =
        this.PartPosition.Y - this.HalfHeight

    /// Whether the part casts a shadow - nothing that glows or that's see-through does.
    member this.CastsShadow =
        this.PartShadowed && this.PartColor.A >= 1.0f && this.PartSurface <> Light

/// A primitive shape attached to a bone.
type PuppetPart =
    { Bone : Bone
      Shape : PartShapeType
      Offset : Vector3 // from the bone's pivot
      Size : Vector3
      Tilt : Quaternion
      Color : Color
      Surface : SurfaceType // what it's made of
      Emission : single
      Pulse : Pulse option // its glow swelling and fading
      BoneIndex : int // where its bone is among its puppet's bones
      Local : Matrix4x4 // its size, tilt and offset, as a transform in its bone's frame
      Properties : MaterialProperties } // how it's drawn, its glow unpulsed

    /// How a part is drawn at a time in seconds, its glow pulsing by it.
    static member getProperties (time : single) (part : PuppetPart) =
        match part.Pulse with
        | Some pulse -> { part.Properties with EmissionOpt = ValueSome (part.Emission * Pulse.getScale time 0.0f pulse) }
        | None -> part.Properties

/// A puppet's pose.
type Pose =
    { RootOffset : Vector3 // world offset
      Fall : single // rotation about the feet, positive is forward
      Roll : single // rotation about the forward axis through the body's middle, as when flipped onto one's head
      Spin : single // extra turn about the vertical, as in a whirl
      Rise : single // the torso's lift above the hips, as in breathing
      Rotations : Quaternion array } // each bone's turn, by where it is among its puppet's bones - never changed once posed

/// One of a puppet's animations - its segments, and when each starts and how long it lasts, in seconds.
type PuppetAnimation =
    { Segments : Segment array
      Spans : (single * single) array }

/// A character's puppet as its file in Assets/Gameplay states it - the file named for its character.
[<SymbolicExpansion>]
type PuppetData =
    { Body : BodyPlan
      Scale : single
      Palette : Map<string, Color>
      Parts : PuppetPartData list
      Poses : PoseStatement list // its own poses, in place of its pose set's
      Animations : AnimationStatement list } // its own animations, in place of its animation set's

/// A puppet made of primitive parts, built from its puppet file. Puppets face +Z in their local space and stand on
/// y = 0.
type Puppet =
    { Name : string // the character it's the puppet of, for what's said of it
      Scale : single
      Bones : Bone array // the bones its body plan builds, parents before their children
      BoneIndices : Map<Bone, int> // where each bone is among its bones
      Parents : int array // where each bone's parent is among its bones, by where the bone is, -1 for the root
      Pivots : Vector3 array // offset of each bone's pivot from its parent's pivot, by where the bone is
      Chains : Map<Chain, Bone array> // the runs of bones its body plan declares, each from its root out
      Feet : int array // where the legs hanging from the root that keep the body on the ground however they spread are among its bones
      Quadruped : bool // goes about on four legs
      Parts : PuppetPart array
      Rest : Quaternion array // the turns of bones a pose leaves out, by where each bone is
      Poses : Map<PoseType, Pose> // its key poses, from its pose set and its own, their roots in its own space
      Cycles : Map<CycleType, CycleData> // its cycles, from its pose set and its own
      Animations : Map<AnimationType, PuppetAnimation> // its animations, from its animation set and its own
      Always : LayerData list } // the layers laid over all its animations

    /// Rotation that swings a hanging limb (an arm, thigh or shin) forward and up (positive) or backward (negative).
    static let swing angle = Quaternion.CreateFromAxisAngle (v3Right, -angle)

    /// Rotation that bends an upright bone (the pelvis, torso or head) forward (positive) or backward (negative).
    static let lean angle = Quaternion.CreateFromAxisAngle (v3Right, angle)

    /// Rotation that raises a limb out to its side (positive raises the left limb outward).
    static let spread angle = Quaternion.CreateFromAxisAngle (v3Back, angle)

    /// Rotation about the vertical axis.
    static let twist angle = Quaternion.CreateFromAxisAngle (v3Up, angle)

    /// Linearly interpolate between two values.
    static let lerp (a : single) b t = a + (b - a) * t

    (* Rigs *)

    /// A pose with nothing turned.
    static let emptyPose =
        { RootOffset = v3Zero; Fall = 0.0f; Roll = 0.0f; Spin = 0.0f; Rise = 0.0f; Rotations = [||] }

    /// The rotation a turn's steps make, in the order written, its angles in degrees.
    static let getTurn (turn : Turn list) =
        turn |>
        List.fold (fun rotation step ->
            rotation *
            match step with
            | Twist degrees -> twist (Math.DegreesToRadiansF degrees)
            | Lean degrees -> lean (Math.DegreesToRadiansF degrees)
            | Swing degrees -> swing (Math.DegreesToRadiansF degrees)
            | Spread degrees -> spread (Math.DegreesToRadiansF degrees))
            quatIdentity

    /// Each of a puppet's bones' turns, by where the bone is among its bones, from turns by bone - a bone not turned
    /// left unturned, and a turned bone it hasn't left out.
    static let getRotations (bones : Bone array) (rotations : Map<Bone, Quaternion>) =
        bones |> Array.map (fun bone -> match rotations.TryGetValue bone with (true, rotation) -> rotation | (false, _) -> quatIdentity)

    /// The pose set of a body plan.
    static let getPoseSetType body =
        match body with
        | Biped _ -> BipedPoses
        | Quadruped _ -> QuadrupedPoses

    /// A puppet's rest turns, key poses and cycles, from its pose set and then its own poses, a later statement taking the
    /// place of an earlier one - its rest turns bone by bone. A pose's chains turn each of their bones the puppet has, and
    /// its bones' own turns then take the place of their chains'.
    static let getPoses name (data : PuppetData) (bones : Bone array) (chains : Map<Chain, Bone array>) (poseSets : Map<PoseSetType, PoseStatement list>) =
        let poseSetType = getPoseSetType data.Body
        let statements =
            match Map.tryFind poseSetType poseSets with
            | Some statements -> statements @ data.Poses
            | None -> failwith ("No pose file for " + scstring poseSetType + ", which " + name + "'s puppet needs, in " + Assets.Gameplay.PuppetDirPath + ".")
        let rest =
            statements |>
            List.fold (fun rest statement ->
                match statement with
                | Rest turns -> Map.fold (fun rest bone turn -> Map.add bone (getTurn turn) rest) rest turns
                | Pose _ | Cycling _ -> rest)
                Map.empty
        let poses =
            statements |>
            List.fold (fun poses statement ->
                match statement with
                | Pose (poseType, pose) ->
                    let rotations =
                        Map.fold (fun rotations chain turn ->
                            match chains.TryGetValue chain with
                            | (true, bones) -> Array.fold (fun rotations bone -> Map.add bone (getTurn turn) rotations) rotations bones
                            | (false, _) -> rotations)
                            rest pose.Chains
                    let rotations = Map.fold (fun rotations bone turn -> Map.add bone (getTurn turn) rotations) rotations pose.Turns
                    Map.add poseType { emptyPose with RootOffset = pose.Root; Fall = Math.DegreesToRadiansF pose.Fall; Rotations = getRotations bones rotations } poses
                | Rest _ | Cycling _ -> poses)
                Map.empty
        let cycles =
            statements |>
            List.fold (fun cycles statement ->
                match statement with
                | Cycling (cycleType, cycle) -> Map.add cycleType cycle cycles
                | Rest _ | Pose _ -> cycles)
                Map.empty
        (getRotations bones rest, poses, cycles)

    /// The animation set of a body plan.
    static let getAnimationSetType body =
        match body with
        | Biped _ -> BipedAnimations
        | Quadruped _ -> QuadrupedAnimations

    /// A puppet's animations and the layers laid over them all, from its animation set and then its own animations, a later
    /// statement taking the place of an earlier one.
    static let getAnimations name (data : PuppetData) (animationSets : Map<AnimationSetType, AnimationStatement list>) =
        let animationSetType = getAnimationSetType data.Body
        let statements =
            match Map.tryFind animationSetType animationSets with
            | Some statements -> statements @ data.Animations
            | None -> failwith ("No animation file for " + scstring animationSetType + ", which " + name + "'s puppet needs, in " + Assets.Gameplay.PuppetDirPath + ".")
        statements |>
        List.fold (fun (animations, always) statement ->
            match statement with
            | Animation (animationType, segments) -> (Map.add animationType segments animations, always)
            | Always layers -> (animations, layers))
            (Map.empty, [])

    /// A bone's twin on the other side of the body, or itself if it's on the body's middle.
    static let mirrorBone bone =
        match bone with
        | ArmLeft -> ArmRight | ArmRight -> ArmLeft
        | HandLeft -> HandRight | HandRight -> HandLeft
        | LegLeft -> LegRight | LegRight -> LegLeft
        | Wing (LeftSide, segment) -> Wing (RightSide, segment) | Wing (RightSide, segment) -> Wing (LeftSide, segment)
        | SpiderLeg (LeftSide, pair, segment) -> SpiderLeg (RightSide, pair, segment) | SpiderLeg (RightSide, pair, segment) -> SpiderLeg (LeftSide, pair, segment)
        | Pelvis | Torso | Head | Tail _ | Abdomen | Spine _ | Jaw -> bone

    /// A part's twin, mirrored across the body's middle - on the twin bone, across the middle, turned the other way
    /// about the vertical and to the other side.
    static let mirrorPart (part : PuppetPartData) =
        let turn = part.Turn |> List.map (fun step -> match step with Twist a -> Twist -a | Spread a -> Spread -a | Lean _ | Swing _ -> step)
        { part with Bone = mirrorBone part.Bone; At = v3 -part.At.X part.At.Y part.At.Z; Turn = turn }

    /// A biped's skeleton - its bones, parents first, each with its parent and pivot.
    static let getBipedSkeleton (plan : BipedPlan) =
        [|(Pelvis, None, v3 0.0f plan.Legs 0.0f)
          (Torso, Some Pelvis, v3 0.0f 0.04f 0.0f)
          (Head, Some Torso, v3 0.0f plan.Torso 0.0f)
          (ArmLeft, Some Torso, v3 plan.Shoulders (plan.Torso - 0.06f) 0.0f)
          (ArmRight, Some Torso, v3 -plan.Shoulders (plan.Torso - 0.06f) 0.0f)
          (HandLeft, Some ArmLeft, v3 0.0f -plan.Arms 0.0f)
          (HandRight, Some ArmRight, v3 0.0f -plan.Arms 0.0f)
          (LegLeft, Some Pelvis, v3 plan.Hips 0.0f 0.0f)
          (LegRight, Some Pelvis, v3 -plan.Hips 0.0f 0.0f)|]

    /// A quadruped's skeleton - its hips, the root of it all, at the back of its body, its torso reaching forward from
    /// them along +z to its shoulders, its head up and ahead on its neck, its front legs (the arm bones) hanging from its
    /// shoulders and its hind legs from its hips, front and hind the same length so all four feet stand on the ground,
    /// and its tail, if it has one, trailing from its hips.
    static let getQuadrupedSkeleton (plan : QuadrupedPlan) =
        Array.append
            [|(Pelvis, None, v3 0.0f plan.Legs (-plan.Body * 0.5f))
              (Torso, Some Pelvis, v3Zero)
              (Head, Some Torso, v3 0.0f plan.HeadRise plan.HeadForward)
              (ArmLeft, Some Torso, v3 plan.Shoulders 0.0f plan.Body)
              (ArmRight, Some Torso, v3 -plan.Shoulders 0.0f plan.Body)
              (HandLeft, Some ArmLeft, v3 0.0f -plan.Legs 0.0f)
              (HandRight, Some ArmRight, v3 0.0f -plan.Legs 0.0f)
              (LegLeft, Some Pelvis, v3 plan.Hips 0.0f 0.0f)
              (LegRight, Some Pelvis, v3 -plan.Hips 0.0f 0.0f)|]
            (match plan.Tail with
             | Some tail -> Array.init tail.Segments (fun segment -> (Tail segment, Some (if segment = 0 then Pelvis else Tail (dec segment)), if segment = 0 then tail.Root else tail.Step))
             | None -> [||])

    /// A limb of a biped, mirrored to the other side.
    static let makeLimb bone shape at size (color, surface) =
        { Bone = bone; Shape = shape; At = at; Size = size; Turn = []; Color = color; Surface = surface; Emission = 0.0f; Pulse = None; Mirror = true }

    /// A biped's standard limbs - arms, hands, legs and feet - as its clothes color them.
    static let getBipedLimbs (plan : BipedPlan) (clothes : ClothesData) =
        [makeLimb ArmLeft PartBox (v3 0.0f (-plan.Arms * 0.5f) 0.0f) (v3 plan.Limbs plan.Arms plan.Limbs) clothes.Arms
         makeLimb HandLeft PartBall v3Zero (v3Dup (plan.Limbs * 1.3f)) clothes.Hands
         makeLimb LegLeft PartBox (v3 0.0f (-plan.Legs * 0.5f) 0.0f) (v3 (plan.Limbs * 1.1f) plan.Legs (plan.Limbs * 1.1f)) clothes.Legs
         makeLimb LegLeft PartBox (v3 0.0f (-plan.Legs + 0.035f) 0.05f) (v3 (plan.Limbs * 1.15f) 0.07f (plan.Limbs * 2.0f)) clothes.Feet]

    /// A color of a puppet's palette, by name.
    static let getPaletteColor puppetName (data : PuppetData) name =
        match Map.tryFind name data.Palette with
        | Some color -> color
        | None -> failwith ("No color named '" + name + "' in the palette of " + puppetName + "'s puppet.")

    /// How high a bone's pivot sits above its parent's.
    static let getPivotY (puppet : Puppet) bone =
        match puppet.BoneIndices.TryGetValue bone with (true, index) -> puppet.Pivots[index].Y | (false, _) -> 0.0f

    /// Turn a bone in rotations being made for a pose, if the puppet has it. Only for rotations not yet in a pose, since
    /// a pose's never change once it's posed.
    static let setBoneRotation (puppet : Puppet) (rotations : Quaternion array) bone rotation =
        match puppet.BoneIndices.TryGetValue bone with
        | (true, index) -> rotations[index] <- rotation
        | (false, _) -> ()

    (* Poses *)

    /// One of a puppet's key poses, its root turned from the puppet's own space to face forward in the world.
    static let getPose (puppet : Puppet) (forward : Vector3) poseType =
        match puppet.Poses.TryGetValue poseType with
        | (true, pose) ->
            if pose.RootOffset = v3Zero then pose
            else
                let local = pose.RootOffset
                { pose with RootOffset = forward * local.Z + v3Up * local.Y + Vector3.Cross (v3Up, forward) * local.X }
        | (false, _) -> failwith ("No pose " + scstring poseType + " in the pose set of a " + (if puppet.Quadruped then "quadruped" else "biped") + ".")

    /// A swaying value at a cycle's phase, in radians.
    static let getSwayed (sway : Sway) (phase : single) =
        let at = phase + Math.DegreesToRadiansF sway.Offset
        let wave =
            match sway.Wave with
            | Sine -> sin at
            | Lift -> max 0.0f (sin at)
            | Rebound -> abs (sin at)
        sway.Base + sway.Amplitude * wave

    /// The rotation of a cycle's steps for a bone at a phase in radians, mirrored if wanted.
    static let getStepsRotation mirrored (phase : single) (steps : (Axis * Sway) list) =
        let mutable rotation = quatIdentity
        for (axis, sway) in steps do
            let angle = Math.DegreesToRadiansF (getSwayed sway phase)
            rotation <-
                rotation *
                match axis with
                | Twisting -> twist (if mirrored then -angle else angle)
                | Leaning -> lean angle
                | Swinging -> swing angle
                | Spreading -> spread (if mirrored then -angle else angle)
        rotation

    /// Turn the bones one of a puppet's cycles turns at a phase in radians, in rotations being made for a pose, giving its
    /// bob - mirrored across the body's middle if wanted, its bones' steps on the twin bones, turned the other way about
    /// the vertical and to the other side. Its chains turn each of their bones the puppet has, and its bones' own turns
    /// then take the place of theirs.
    static let turnByCycle (puppet : Puppet) cycleType mirrored (phase : single) (rotations : Quaternion array) =
        match puppet.Cycles.TryGetValue cycleType with
        | (true, cycle) ->
            for KeyValue (chain, steps) in cycle.Chains do
                match puppet.Chains.TryGetValue chain with
                | (true, bones) ->
                    let rotation = getStepsRotation mirrored phase steps
                    for bone in bones do setBoneRotation puppet rotations bone rotation
                | (false, _) -> ()
            for KeyValue (bone, steps) in cycle.Turns do
                setBoneRotation puppet rotations (if mirrored then mirrorBone bone else bone) (getStepsRotation mirrored phase steps)
            match cycle.Bob with Some bob -> getSwayed bob phase | None -> 0.0f
        | (false, _) -> failwith ("No cycle " + scstring cycleType + " in the pose set of a " + (if puppet.Quadruped then "quadruped" else "biped") + ".")

    /// One of a puppet's cycles at a phase in radians, as a pose - mirrored across the body's middle if wanted - bones it
    /// doesn't turn taking the rest turns.
    static let getCycle (puppet : Puppet) cycleType mirrored (phase : single) =
        let rotations = Array.copy puppet.Rest
        let bob = turnByCycle puppet cycleType mirrored phase rotations
        { emptyPose with RootOffset = v3 0.0f bob 0.0f; Rotations = rotations }

    /// Blend between key poses, slerping every bone - either pose itself at either end.
    static let blend (a : Pose) (b : Pose) (t : single) =
        let t = max 0.0f (min 1.0f t)
        if t = 0.0f then a
        elif t = 1.0f then b
        else
            let rotations = Array.zeroCreate a.Rotations.Length
            for i in 0 .. dec rotations.Length do
                rotations[i] <- Quaternion.Slerp (a.Rotations[i], b.Rotations[i], t)
            { RootOffset = Vector3.Lerp (a.RootOffset, b.RootOffset, t)
              Fall = lerp a.Fall b.Fall t
              Roll = lerp a.Roll b.Roll t
              Spin = lerp a.Spin b.Spin t
              Rise = lerp a.Rise b.Rise t
              Rotations = rotations }

    (* Animations *)

    /// How an animation plays, by its arguments.
    static let getPlaying idleTime (forward : Vector3) enemy animation =
        let playing =
            { Forward = forward; Enemy = enemy; Offset = v3Zero; Center = v3Zero; Height = 0.0f
              AimPitch = 0.0f; Mirrored = false; IdleTime = idleTime }
        match animation with
        | AimAnimation pitch | ShootAnimation pitch | ReadyAnimation pitch -> { playing with AimPitch = pitch }
        | IdleAnimation | RelaxAnimation | WalkAnimation | WalkBackAnimation | SwingAnimation | HurtAnimation | WoundAnimation
        | GrabAnimation | CrouchAnimation | TackleAnimation | SprawlAnimation -> playing

    /// One of a puppet's animations.
    static let getAnimation (puppet : Puppet) animationType =
        match puppet.Animations.TryGetValue animationType with
        | (true, animation) -> animation
        | (false, _) -> failwith ("No " + scstring animationType + " animation in the animation set of a " + (if puppet.Quadruped then "quadruped" else "biped") + ".")

    /// Where along an ease a progress through it is.
    static let getEased ease (t : single) =
        let t = max 0.0f (min 1.0f t)
        match ease with
        | Ease.Smooth -> Math.Smoothstep t
        | Ease.Linear -> t
        | Ease.In -> t * t
        | Ease.Out -> 1.0f - (1.0f - t) * (1.0f - t)
        | Ease.InOut -> if t < 0.5f then 2.0f * t * t else 1.0f - 2.0f * (1.0f - t) * (1.0f - t)
        | Ease.Snap -> t * t * (2.0f - t)
        | Ease.Pulse -> sin (t * MathF.PI)
        | Ease.Late -> let s = Math.Smoothstep t in s * s

    /// A direction in a puppet's own space (+z its front, +x its left), turned to face forward in the world.
    static let toWorld (forward : Vector3) (local : Vector3) =
        forward * local.Z + v3Up * local.Y + Vector3.Cross (v3Up, forward) * local.X

    /// Each segment's start and duration, in seconds.
    static let getSpans (segments : Segment array) =
        segments |>
        Array.mapFold (fun start segment -> ((start, segment.Over), start + segment.Over)) 0.0f |>
        fst

    /// The segment playing at a local time - the last holding once all have played.
    static let getPlayingIndex (localTime : single) (spans : (single * single) array) =
        let mutable index = 0
        while index < dec spans.Length && localTime >= fst spans[index] + snd spans[index] do
            index <- inc index
        index

    /// How far through a span a local time is, held at either end.
    static let getProgress (localTime : single) (start, duration) =
        if duration > 0.0f then max 0.0f (min 1.0f ((localTime - start) / duration)) else 1.0f

    /// How high a hop lifts the root part way through it, higher the further it carries.
    static let getHop (hop : HopData) (distance : single) (t : single) =
        let height = if distance > 0.01f then hop.Hop + hop.HopPerMeter * distance else 0.0f
        v3Up * (4.0f * height * t * (1.0f - t))

    /// Where a move carries the root from where it started, part way through its segment.
    static let getMoved (seconds : single) (playing : AnimationPlayback) move (start : Vector3) (t : single) =
        match move with
        | Travel hop -> start + (playing.Offset - start) * Math.Smoothstep t + getHop hop (playing.Offset - start).Magnitude t
        | Return hop -> start * (1.0f - Math.Smoothstep t) + getHop hop start.Magnitude t
        | Rise rise ->
            let height = playing.Offset.Y + rise.Up + rise.OfHeight * playing.Height
            v3 start.X (start.Y + (height - start.Y) * getEased rise.Ease t) start.Z
        | Circle circle ->
            let p = Math.Smoothstep t
            let around = Vector3.Transform (start - playing.Center, Quaternion.CreateFromAxisAngle (v3Up, MathF.Tau * p))
            playing.Center + around + v3Up * (abs (sin (p * MathF.PI * circle.Bobs)) * circle.Bob)
        | Sink rate -> if playing.Enemy then start - v3Up * (rate * seconds) else start

    /// The root's offset from its moves at a local time, each move starting where the last left it.
    static let getMoveRoot (localTime : single) playing (segments : Segment array) (spans : (single * single) array) index =
        let mutable root = v3Zero
        for i in 0 .. index do
            let (start, duration) = spans[i]
            let (t, seconds) = if i < index then (1.0f, duration) else (getProgress localTime spans[i], localTime - start)
            match segments[i].Move with
            | Some move -> root <- getMoved seconds playing move root t
            | None -> ()
        root

    /// An angle of the whole body at a local time, in radians, each segment's starting where the last left it.
    static let getAngle (localTime : single) (getAngleOpt : Segment -> AngleData option) (segments : Segment array) (spans : (single * single) array) index =
        let mutable angle = 0.0f
        for i in 0 .. index do
            match getAngleOpt segments[i] with
            | Some data ->
                let t = if i < index then 1.0f else getProgress localTime spans[i]
                let start = match data.From with Some from -> Math.DegreesToRadiansF from | None -> angle
                angle <- start + (Math.DegreesToRadiansF data.To - start) * getEased data.Ease t
            | None -> ()
        angle

    /// The phase in radians a layer's clock reads at a rate - by seconds, by its segment's progress, or by idle time.
    static let getClockPhase (seconds : single) (playing : AnimationPlayback) (progress : single) clock rate =
        match clock with
        | Clock.Seconds -> seconds * rate * MathF.Tau
        | Clock.Progress -> progress * rate * MathF.Tau
        | Clock.Idle -> playing.IdleTime * rate * MathF.Tau

    /// Lay a layer over a pose.
    static let layer (seconds : single) (puppet : Puppet) (playing : AnimationPlayback) (pose : Pose) (kind : LayerKind) (progress : single) (span : single) =
        match kind with
        | Waver waver ->
            let ramp = if waver.RampIn > 0.0f then min 1.0f (seconds / waver.RampIn) else 1.0f
            let fade = if waver.Fade then max 0.0f (1.0f - seconds / span) else 1.0f
            let sway = { waver.Sway with Amplitude = waver.Sway.Amplitude * ramp * fade }
            let value = getSwayed sway (getClockPhase seconds playing progress waver.Clock waver.Rate)
            match waver.On with
            | OnRise -> { pose with Rise = pose.Rise + value }
            | OnRoot direction -> { pose with RootOffset = pose.RootOffset + toWorld playing.Forward direction * value }
            | OnBone (bone, axis) ->
                let angle = Math.DegreesToRadiansF value
                let turn = match axis with Twisting -> twist angle | Leaning -> lean angle | Swinging -> swing angle | Spreading -> spread angle
                match puppet.BoneIndices.TryGetValue bone with
                | (true, index) ->
                    let rotations = Array.copy pose.Rotations
                    rotations[index] <- rotations[index] * turn
                    { pose with Rotations = rotations }
                | (false, _) -> pose
        | Overlay overlay ->
            let rotations = Array.copy pose.Rotations
            turnByCycle puppet overlay.Cycle false (getClockPhase seconds playing progress overlay.Clock overlay.Rate) rotations |> ignore
            { pose with Rotations = rotations }

    /// The phase in radians a pace reads at a local time - by seconds, by steps, or by its segment's progress.
    static let getPacePhase (localTime : single) (progress : single) pace =
        match pace with
        | PerSecond rate -> localTime * rate * MathF.Tau
        | PerStep rate -> localTime * rate * Constants.Puppet.StepRate * MathF.Tau
        | PerSegment rate -> progress * rate * MathF.Tau

    /// A target's pose at a local time, part way through its segment.
    static let rec getTarget (localTime : single) (puppet : Puppet) (playing : AnimationPlayback) (progress : single) target =
        match target with
        | Idle -> play localTime puppet { playing with Mirrored = false } AnimationType.Idle
        | Still poseType -> getPose puppet playing.Forward poseType
        | Cycled (cycleType, pace) -> getCycle puppet cycleType false (getPacePhase localTime progress pace)
        | Sided (cycleType, pace) -> getCycle puppet cycleType playing.Mirrored (getPacePhase localTime progress pace)
        | Between (from, toward, sway, pace) -> blend (getPose puppet playing.Forward from) (getPose puppet playing.Forward toward) (getSwayed sway (getPacePhase localTime progress pace))
        | Mixed (from, toward, ease) -> blend (getTarget localTime puppet playing progress from) (getTarget localTime puppet playing progress toward) (getEased ease progress)
        | Choose (condition, chosen, otherwise) ->
            let weight =
                match condition with
                | AimingUp -> max 0.0f (min 1.0f playing.AimPitch)
                | AimingDown -> max 0.0f (min 1.0f -playing.AimPitch)
            if weight <= 0.0f then getTarget localTime puppet playing progress otherwise
            elif weight >= 1.0f then getTarget localTime puppet playing progress chosen
            else blend (getTarget localTime puppet playing progress otherwise) (getTarget localTime puppet playing progress chosen) weight

    /// Play one of a puppet's animations at a local time (in seconds) - the segment playing blending from its start toward
    /// its target, the body carried along by the moves so far, turned by its angles, its layers laid over it.
    and play (localTime : single) (puppet : Puppet) (playing : AnimationPlayback) animationType =
        let animation = getAnimation puppet animationType
        let segments = animation.Segments
        let spans = animation.Spans
        let index = getPlayingIndex localTime spans
        let segment = segments[index]
        let (start, _) = spans[index]
        let t = getProgress localTime spans[index]
        let reached =
            match segment.Reach with
            | Some reach when reach > 0.0f -> max 0.0f (min 1.0f ((localTime - start) / reach))
            | Some _ | None -> t
        let weight = getEased segment.Ease reached * Option.defaultValue 1.0f segment.Partway
        let toward = getTarget localTime puppet playing t segment.To
        let from =
            match segment.From with
            | Some from -> getTarget localTime puppet playing t from
            | None -> if index > 0 then getTarget localTime puppet playing t segments[dec index].To else toward
        let pose = blend from toward weight
        let pose =
            { pose with
                RootOffset = pose.RootOffset + getMoveRoot localTime playing segments spans index
                Fall = pose.Fall + getAngle localTime (fun segment -> segment.Fall) segments spans index
                Roll = pose.Roll + getAngle localTime (fun segment -> segment.Roll) segments spans index
                Spin = pose.Spin + getAngle localTime (fun segment -> segment.Spin) segments spans index }
        let mutable pose = pose
        for i in 0 .. dec segments.Length do
            let (start, duration) = spans[i]
            for layerData in segments[i].Layers do
                let span =
                    match layerData.Over with
                    | Some over -> over
                    | None -> if layerData.Onward || i = dec segments.Length then Single.PositiveInfinity else duration
                if localTime >= start && localTime < start + span then
                    let progress = getEased segments[i].Ease (getProgress localTime spans[i])
                    pose <- layer (localTime - start) puppet playing pose layerData.Lay progress span
        pose

    /// The world matrix of each of a posed puppet's bones, standing at a position and facing a yaw - the body lowered as its
    /// legs spread or stride, so its feet stay on the ground.
    static let computeBoneMatrices (puppet : Puppet) (pose : Pose) (position : Vector3) (yaw : single) =
        // legs spread or striding reach less far down, so lower the body to keep the feet on the ground
        let legLength = puppet.Pivots[0].Y
        let mutable reached = Single.MinValue
        for foot in puppet.Feet do
            reached <- max reached (legLength * -(Vector3.Transform (Vector3.Transform (-v3Up, pose.Rotations[foot]), pose.Rotations[0])).Y)
        let drop = max 0.0f (legLength - reached)
        let rootOffset = pose.RootOffset - v3 0.0f (drop * puppet.Scale) 0.0f
        let middle = Puppet.getHeight puppet / puppet.Scale * 0.5f // roll about the body's middle, so one rolled over stands on one's head
        let rootMatrix =
            Matrix4x4.CreateTranslation (v3 0.0f -middle 0.0f) *
            Matrix4x4.CreateRotationZ pose.Roll *
            Matrix4x4.CreateTranslation (v3 0.0f middle 0.0f) *
            Matrix4x4.CreateScale puppet.Scale *
            Matrix4x4.CreateRotationX pose.Fall *
            Matrix4x4.CreateRotationY (yaw + pose.Spin) *
            Matrix4x4.CreateTranslation (position + rootOffset)
        let boneMatrices = Array.zeroCreate puppet.Bones.Length
        for index in 0 .. dec boneMatrices.Length do
            let pivot = puppet.Pivots[index]
            let pivot = if puppet.Bones[index] = Torso then pivot + v3 0.0f pose.Rise 0.0f else pivot
            let parentIndex = puppet.Parents[index]
            let parent = if parentIndex >= 0 then boneMatrices[parentIndex] else rootMatrix
            boneMatrices[index] <- Matrix4x4.CreateFromQuaternion pose.Rotations[index] * Matrix4x4.CreateTranslation pivot * parent
        boneMatrices

    (* Rigs *)

    /// Make a puppet from its puppet file, named for its character, and the pose and animation sets of every body plan.
    static member make name (data : PuppetData) poseSets animationSets (surfaces : Map<SurfaceType, SurfaceData>) =
        let (skeleton, limbs, quadruped) =
            match data.Body with
            | Biped plan -> (getBipedSkeleton plan, (match plan.Clothes with Some clothes -> getBipedLimbs plan clothes | None -> []), false)
            | Quadruped plan -> (getQuadrupedSkeleton plan, [], true)
        let tail = skeleton |> Array.choose (fun (bone, _, _) -> match bone with Tail _ -> Some bone | Pelvis | Torso | Head | ArmLeft | ArmRight | HandLeft | HandRight | LegLeft | LegRight | Wing _ | Abdomen | SpiderLeg _ | Spine _ | Jaw -> None)
        let chains = Map.ofList [if tail.Length > 0 then (TailChain, tail)]
        let bones = Array.map (fun (bone, _, _) -> bone) skeleton
        let boneIndices = bones |> Array.mapi (fun index bone -> (bone, index)) |> Map.ofArray
        let (rest, poses, cycles) = getPoses name data bones chains poseSets
        let (animations, always) = getAnimations name data animationSets
        let parts =
            [|for part in data.Parts @ limbs do
                part
                if part.Mirror then mirrorPart part|] |>
            Array.map (fun part ->
                let tilt = getTurn part.Turn
                let color = getPaletteColor name data part.Color
                { Bone = part.Bone; Shape = part.Shape; Offset = part.At; Size = part.Size; Tilt = tilt
                  Color = color; Surface = part.Surface; Emission = part.Emission; Pulse = part.Pulse
                  BoneIndex = boneIndices[part.Bone]
                  Local = Matrix4x4.CreateScale part.Size * Matrix4x4.CreateFromQuaternion tilt * Matrix4x4.CreateTranslation part.At
                  Properties = SurfaceData.makeProperties surfaces[part.Surface] (color.WithA 1.0f) part.Emission })
        { Name = name
          Scale = data.Scale
          Bones = bones
          BoneIndices = boneIndices
          Parents = skeleton |> Array.map (fun (_, parentOpt, _) -> match parentOpt with Some parent -> boneIndices[parent] | None -> -1)
          Pivots = skeleton |> Array.map (fun (_, _, pivot) -> pivot)
          Chains = chains
          Feet = [|boneIndices[LegLeft]; boneIndices[LegRight]|]
          Quadruped = quadruped
          Parts = parts
          Rest = rest
          Poses = poses
          Cycles = cycles
          Animations = animations |> Map.map (fun _ segments -> let segments = Array.ofList segments in { Segments = segments; Spans = getSpans segments })
          Always = always }

    /// The standing height of a puppet, from its feet to the top of its head.
    static member getHeight (puppet : Puppet) =
        (getPivotY puppet Pelvis + getPivotY puppet Torso + getPivotY puppet Head + 0.36f) * puppet.Scale

    (* Animations *)

    /// The type of an animation, its arguments aside.
    static member getAnimationType animation =
        match animation with
        | IdleAnimation -> AnimationType.Idle
        | RelaxAnimation -> AnimationType.Relax
        | WalkAnimation -> AnimationType.Walk
        | WalkBackAnimation -> AnimationType.WalkBack
        | AimAnimation _ -> AnimationType.Aim
        | ShootAnimation _ -> AnimationType.Shoot
        | ReadyAnimation _ -> AnimationType.Ready
        | SwingAnimation -> AnimationType.Swing
        | HurtAnimation -> AnimationType.Hurt
        | WoundAnimation -> AnimationType.Wound
        | GrabAnimation -> AnimationType.Grab
        | CrouchAnimation -> AnimationType.Crouch
        | TackleAnimation -> AnimationType.Tackle
        | SprawlAnimation -> AnimationType.Sprawl

    /// When a mark of one of a puppet's animations comes, in seconds from its start.
    static member getMarkTime (puppet : Puppet) animationType (mark : Mark) =
        let animation = getAnimation puppet animationType
        let found =
            Array.zip animation.Segments animation.Spans |>
            Array.tryPick (fun (segment, (start, duration)) ->
                segment.Marks |> List.tryPick (fun (mark', share) -> if mark' = mark then Some (start + share * duration) else None))
        match found with
        | Some time -> time
        | None -> failwith ("No " + scstring mark + " mark in the " + scstring animationType + " animation of " + puppet.Name + ".")

    /// Compute a puppet's pose for an animation at a local time (in seconds), its animation set's layers laid over it. The
    /// idle time drives breathing and the like.
    static member poseOf (localTime : single) (idleTime : single) (puppet : Puppet) (forward : Vector3) (enemy : bool) (animation : CharacterAnimation) =
        let playing = getPlaying idleTime forward enemy animation
        let posed = play localTime puppet playing (Puppet.getAnimationType animation)
        let posed = List.fold (fun posed (layerData : LayerData) -> layer localTime puppet playing posed layerData.Lay 0.0f Single.PositiveInfinity) posed puppet.Always
        posed

    /// The world transforms of a posed puppet's bones, by where each is among its bones - a part following its bone's,
    /// and a gun held in a hand the hand's.
    static member getBoneMatrices (puppet : Puppet) (pose : Pose) (position : Vector3) (yaw : single) =
        computeBoneMatrices puppet pose position yaw

    /// Place the parts of the given puppet in the world at a time in seconds, its glows pulsing by it.
    static member place (time : single) (puppet : Puppet) (pose : Pose) (position : Vector3) (yaw : single) =
        let boneMatrices = computeBoneMatrices puppet pose position yaw
        puppet.Parts |>
        Array.map (fun part ->
            let matrix = part.Local * boneMatrices[part.BoneIndex]
            let mutable scale = v3One
            let mutable rotation = quatIdentity
            let mutable translation = v3Zero
            Matrix4x4.Decompose (matrix, &scale, &rotation, &translation) |> ignore
            let emission =
                match part.Pulse with
                | Some pulse -> part.Emission * Pulse.getScale time 0.0f pulse
                | None -> part.Emission
            { PartPosition = translation
              PartRotation = rotation
              PartScale = scale
              PartShape = part.Shape
              PartColor = part.Color.WithA 1.0f
              PartSurface = part.Surface
              PartShadowed = true
              PartGlow = emission
              PartPowered = false
              PartUnreflected = false })

    /// A character's bounds, as its entity's size and offset - a naive box more than big enough to hold its puppet
    /// however it's posed, facing any way: four times its height across and twice its height up, from half its height
    /// below its feet.
    static member getBounds (puppet : Puppet) =
        let height = Puppet.getHeight puppet
        (v3 (height * 4.0f) (height * 2.0f) (height * 4.0f), v3 0.0f 0.25f 0.0f)