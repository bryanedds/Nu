// Zombie Game.
// Copyright (C) Bryan Edds.

namespace ZombieGame
open System
open System.Collections.Generic
open System.Numerics
open Prime
open Nu

/// How a puppet poses - who it is, its animation and how far into it, its idle time, whether it's an enemy, whether its
/// head's been shot off, and what it holds.
[<AutoOpen>]
module PuppetDispatcherExtensions =
    type Entity with
        member this.GetCharacterType world : CharacterType = this.Get (nameof this.CharacterType) world
        member this.SetCharacterType (value : CharacterType) world = this.Set (nameof this.CharacterType) value world
        member this.CharacterType = lens (nameof this.CharacterType) this this.GetCharacterType this.SetCharacterType
        member this.GetPuppetAnimation world : CharacterAnimation = this.Get (nameof this.CharacterAnimation) world
        member this.SetPuppetAnimation (value : CharacterAnimation) world = this.Set (nameof this.CharacterAnimation) value world
        member this.CharacterAnimation = lens (nameof this.CharacterAnimation) this this.GetPuppetAnimation this.SetPuppetAnimation
        member this.GetPuppetTime world : single = this.Get (nameof this.PuppetTime) world
        member this.SetPuppetTime (value : single) world = this.Set (nameof this.PuppetTime) value world
        member this.PuppetTime = lens (nameof this.PuppetTime) this this.GetPuppetTime this.SetPuppetTime
        member this.GetIdleTime world : single = this.Get (nameof this.IdleTime) world
        member this.SetIdleTime (value : single) world = this.Set (nameof this.IdleTime) value world
        member this.IdleTime = lens (nameof this.IdleTime) this this.GetIdleTime this.SetIdleTime
        member this.GetEnemy world : bool = this.Get (nameof this.Enemy) world
        member this.SetEnemy (value : bool) world = this.Set (nameof this.Enemy) value world
        member this.Enemy = lens (nameof this.Enemy) this this.GetEnemy this.SetEnemy
        member this.GetHeadless world : bool = this.Get (nameof this.Headless) world
        member this.SetHeadless (value : bool) world = this.Set (nameof this.Headless) value world
        member this.Headless = lens (nameof this.Headless) this this.GetHeadless this.SetHeadless
        member this.GetHeldOpt world : ItemType option = this.Get (nameof this.HeldOpt) world
        member this.SetHeldOpt (value : ItemType option) world = this.Set (nameof this.HeldOpt) value world
        member this.HeldOpt = lens (nameof this.HeldOpt) this this.GetHeldOpt this.SetHeldOpt
        member this.GetYaw world : single = this.Get (nameof this.Yaw) world
        member this.SetYaw (value : single) world = this.Set (nameof this.Yaw) value world
        member this.Yaw = lens (nameof this.Yaw) this this.GetYaw this.SetYaw


/// A puppet as posed - what it was posed from, its parts and whatever it holds made ready to draw, where its right hand
/// is, and where the middle of its head is, if it has one.
type PuppetPosing =
    { PosingCharacterType : CharacterType
      PosingAnimation : CharacterAnimation
      PosingTime : single
      PosingIdleTime : single
      PosingEnemy : bool
      PosingHeadless : bool
      PosingHeldOpt : ItemType option
      PosingPosition : Vector3
      PosingYaw : single
      PosingDrawings : Drawing array
      PosingHand : Matrix4x4
      PosingHeadCenterOpt : Vector3 option }

/// A character's body - its puppet posed for an animation at a time, and whatever it holds in hand drawn there. A
/// player or creature declares one as its child each frame, telling it how to pose. It's posed once for however many
/// render passes draw it, and posed afresh only when how it's told to pose changes.
type PuppetDispatcher () =
    inherit Entity3dDispatcher (false, false, false)

    /// Each puppet's posing, as of the last time it was drawn or asked where its hand is.
    static let posings = Dictionary<Entity, PuppetPosing> HashIdentity.Structural

    /// Each weapon's drawings in the frame of the hand holding it, made ready the first time it's held.
    static let heldDrawings = Dictionary<ItemType, Drawing array> HashIdentity.Structural

    /// Make a box or other shape in the hand's frame ready to draw - a gun or knife in hand, its fingers pointing along -y.
    static let prepareHeld surface shape (at : Vector3) (size : Vector3) color =
        Draw.prepareMatrix surface shape (Matrix4x4.CreateScale size * Matrix4x4.CreateTranslation at) color 0.0f true

    /// Make a weapon ready to draw in the frame of the hand holding it.
    static let prepareWeapon weapon =
        let gunmetal = Color (0.12f, 0.12f, 0.13f, 1.0f)
        let wood = Color (0.36f, 0.22f, 0.12f, 1.0f)
        match weapon with
        | Handgun ->
            [|prepareHeld Steel PartBox (v3 0.0f -0.11f 0.045f) (v3 0.032f 0.19f 0.045f) gunmetal
              prepareHeld Rubber PartBox (v3 0.0f -0.035f -0.01f) (v3 0.03f 0.05f 0.1f) (Color (0.08f, 0.07f, 0.06f, 1.0f))|]
        | Shotgun ->
            [|prepareHeld Steel PartCylinder (v3 0.0f -0.36f 0.05f) (v3 0.035f 0.62f 0.035f) gunmetal
              prepareHeld Wood PartBox (v3 0.0f -0.24f 0.015f) (v3 0.05f 0.22f 0.045f) wood
              prepareHeld Wood PartBox (v3 0.0f 0.13f 0.0f) (v3 0.05f 0.3f 0.075f) wood|]
        | Magnum ->
            [|prepareHeld Steel PartBox (v3 0.0f -0.16f 0.05f) (v3 0.036f 0.29f 0.045f) (Color (0.55f, 0.56f, 0.58f, 1.0f))
              prepareHeld Steel PartCylinder (v3 0.0f -0.05f 0.05f) (v3 0.06f 0.06f 0.06f) (Color (0.45f, 0.46f, 0.48f, 1.0f)) // the cylinder
              prepareHeld Wood PartBox (v3 0.0f -0.035f -0.01f) (v3 0.034f 0.055f 0.1f) (Color (0.3f, 0.18f, 0.1f, 1.0f))|]
        | GrenadeLauncher ->
            [|prepareHeld Steel PartCylinder (v3 0.0f -0.22f 0.06f) (v3 0.07f 0.38f 0.07f) (Color (0.25f, 0.29f, 0.18f, 1.0f)) // the fat short barrel
              prepareHeld Wood PartBox (v3 0.0f -0.11f 0.02f) (v3 0.05f 0.12f 0.05f) wood // its forend
              prepareHeld Wood PartBox (v3 0.0f 0.12f 0.0f) (v3 0.05f 0.3f 0.08f) wood // its stock
              prepareHeld Steel PartBox (v3 0.0f -0.3f 0.11f) (v3 0.02f 0.05f 0.03f) gunmetal|] // its leaf sight
        | Knife ->
            [|prepareHeld Chrome PartBox (v3 0.0f -0.12f 0.0f) (v3 0.006f 0.14f 0.025f) (Color (0.8f, 0.82f, 0.85f, 1.0f))
              prepareHeld Plastic PartBox (v3 0.0f -0.02f 0.0f) (v3 0.025f 0.07f 0.03f) (Color (0.15f, 0.25f, 0.5f, 1.0f))|]
        | HandgunBullets | ShotgunShells | MagnumRounds | Grenades | Antiseptic | Coagulant | DoubleAntiseptic | TraumaMix | FirstAidSpray | InkRibbon | PharmacyKey | MorgueKey | Fuse | AmbulanceKeys | StudentId | MasterKey | VanKeys | BoltCutters | Keycard | ElevatorKey -> [||]

    /// A weapon's drawings in the frame of the hand holding it.
    static let getHeldDrawings weapon =
        match heldDrawings.TryGetValue weapon with
        | (true, drawings) -> drawings
        | (false, _) ->
            let drawings = prepareWeapon weapon
            heldDrawings[weapon] <- drawings
            drawings

    /// Pose a puppet as it's told - its parts and whatever it holds made ready to draw, where its right hand is, and
    /// where the middle of its head is. A headless puppet leaves out its head's parts and shows the bloody stump of its
    /// neck.
    static let pose time idleTime characterType animation enemy headless heldOpt position yaw =
        let puppet = Data.Puppets[characterType]
        let pose = Puppet.poseOf time idleTime puppet (Math.YawForward yaw) enemy animation
        let boneMatrices = Puppet.getBoneMatrices puppet pose position yaw
        let hand = boneMatrices[puppet.BoneIndices[HandRight]]
        let held = match heldOpt with Some item -> getHeldDrawings item | None -> [||]
        let drawings = ResizeArray (puppet.Parts.Length + held.Length + 1)
        let mutable headCenter = v3Zero
        let mutable headParts = 0
        for part in puppet.Parts do
            let size = part.Size * puppet.Scale
            let matrix = part.Local * boneMatrices[part.BoneIndex]
            if part.Bone = Head then
                headCenter <- headCenter + matrix.Translation
                headParts <- inc headParts
            if size.X > 0.0005f && size.Y > 0.0005f && size.Z > 0.0005f && not (headless && part.Bone = Head) then
                drawings.Add (Draw.prepareProperties part.Shape matrix (PuppetPart.getProperties idleTime part) true)
        match Map.tryFind Head puppet.BoneIndices with
        | Some headIndex when headless ->
            let stump = Matrix4x4.CreateScale Constants.Gameplay.NeckStumpSize * Matrix4x4.CreateTranslation (v3Up * Constants.Gameplay.NeckStumpSize.Y * 0.5f) // the bone's frame's scaled with the puppet already
            drawings.Add (Draw.prepareMatrix Blood PartCylinder (stump * boneMatrices[headIndex]) Constants.Gameplay.NeckStumpColor 0.0f true)
        | Some _ | None -> ()
        for drawing in held do
            drawings.Add { drawing with DrawingMatrix = drawing.DrawingMatrix * hand }
        { PosingCharacterType = characterType
          PosingAnimation = animation
          PosingTime = time
          PosingIdleTime = idleTime
          PosingEnemy = enemy
          PosingHeadless = headless
          PosingHeldOpt = heldOpt
          PosingPosition = position
          PosingYaw = yaw
          PosingDrawings = drawings.ToArray ()
          PosingHand = hand
          PosingHeadCenterOpt = if headParts > 0 then Some (headCenter / single headParts) else None }

    /// A puppet's posing as it's told to pose now - the last one, if it's still told to pose so, or else a fresh one.
    static member getPosing (entity : Entity) world =
        let characterType = entity.GetCharacterType world
        let animation = entity.GetPuppetAnimation world
        let time = entity.GetPuppetTime world
        let idleTime = entity.GetIdleTime world
        let enemy = entity.GetEnemy world
        let headless = entity.GetHeadless world
        let heldOpt = entity.GetHeldOpt world
        let position = entity.GetPosition world
        let yaw = entity.GetYaw world
        match posings.TryGetValue entity with
        | (true, posing) when
            posing.PosingTime = time && posing.PosingIdleTime = idleTime && posing.PosingPosition = position && posing.PosingYaw = yaw &&
            posing.PosingEnemy = enemy && posing.PosingHeadless = headless && posing.PosingCharacterType = characterType &&
            posing.PosingAnimation = animation && posing.PosingHeldOpt = heldOpt ->
            posing
        | (_, _) ->
            let posing = pose time idleTime characterType animation enemy headless heldOpt position yaw
            posings[entity] <- posing
            posing

    static member Properties =
        [define Entity.CharacterType Player
         define Entity.CharacterAnimation RelaxAnimation
         define Entity.PuppetTime 0.0f
         define Entity.IdleTime 0.0f
         define Entity.Enemy false
         define Entity.Headless false
         define Entity.HeldOpt None
         define Entity.Yaw 0.0f
         define Entity.Size (fst (Puppet.getBounds Data.Puppets[Player]))
         define Entity.Offset (snd (Puppet.getBounds Data.Puppets[Player]))]

    override this.Unregister (entity, _) =
        posings.Remove entity |> ignore

    override this.Render (renderPass, entity, world) =
        if Draw.passes renderPass true world then
            for drawing in (PuppetDispatcher.getPosing entity world).PosingDrawings do
                Draw.drawing renderPass drawing world

/// Where a puppet's right hand and the muzzle of the gun in it are, and where its head is, posed as it's told.
[<AutoOpen>]
module PuppetDispatcherHandExtensions =
    type Entity with

        /// Where the middle of a puppet's head is, posed as it's told, if it has a head - where a shot to it bursts it.
        member this.GetHeadCenterOpt world =
            (PuppetDispatcher.getPosing this world).PosingHeadCenterOpt

        /// Where a puppet's right hand is, posed as it's told - so a shot springs from the gun in it.
        member this.GetHandTransform world =
            (PuppetDispatcher.getPosing this world).PosingHand

        /// Where the muzzle of the gun in a puppet's hand is, posed as it's told, if it holds a gun - just past the end
        /// of its barrel as the puppet draws it, along -y in the hand's frame.
        member this.GetMuzzleOpt world =
            let reachOpt =
                match this.GetHeldOpt world with
                | Some Handgun -> Some -0.24f
                | Some Shotgun -> Some -0.7f
                | Some Magnum -> Some -0.33f
                | Some GrenadeLauncher -> Some -0.43f
                | Some (Knife | HandgunBullets | ShotgunShells | MagnumRounds | Grenades | Antiseptic | Coagulant | DoubleAntiseptic | TraumaMix | FirstAidSpray | InkRibbon | PharmacyKey | MorgueKey | Fuse | AmbulanceKeys | StudentId | MasterKey | VanKeys | BoltCutters | Keycard | ElevatorKey) | None -> None
            reachOpt |> Option.map (fun reach -> Vector3.Transform (v3 0.0f reach 0.05f, this.GetHandTransform world))
