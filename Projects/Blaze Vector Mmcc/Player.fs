namespace BlazeVector
open System
open System.Numerics
open Prime
open Nu
open BlazeVector

type [<SymbolicExpansion>] Player =
    { Alive : bool
      LastTimeGrounded : int64
      LastTimeJump : int64 }

    static member initial =
        { Alive = true
          LastTimeGrounded = Int64.MinValue
          LastTimeJump = Int64.MinValue }

type PlayerMessage =
    | UpdateMessage
    | TryJumpByMouse
    | TryJumpByKeyboard of KeyboardKeyData
    interface Message

type PlayerCommand =
    | UpdateCommand
    | Shoot
    | Jump
    | Die
    interface Command
    
[<AutoOpen>]
module PlayerExtensions =
    type Entity with
        member this.GetPlayer world : Player = this.GetModelGeneric<Player> world
        member this.SetPlayer player world = this.SetModelGeneric<Player> player world
        member this.Player = this.ModelGeneric<Player> ()

type PlayerDispatcher () =
    inherit Entity2dDispatcher<Player, PlayerMessage, PlayerCommand> (true, false, false, Player.initial)

    static member Facets =
        [typeof<RigidBodyFacet>
         typeof<AnimatedSpriteFacet>]

    override this.Definitions (_, _) =
        [Entity.Size == v3 24.0f 48.0f 0.0f
         Entity.Presence == Omnipresent
         Entity.BodyType == Dynamic
         Entity.BodyShape == CapsuleShape { Height = 0.5f; Radius = 0.25f; TransformOpt = None; PropertiesOpt = None }
         Entity.Friction == 0.0f
         Entity.LinearDamping == 3.0f
         Entity.AngularFactor == v3Zero
         Entity.GravityOverride == Some v3Zero
         Entity.CelCount == 16
         Entity.CelRun == 4
         Entity.CelSize == v2 48.0f 96.0f
         Entity.AnimationDelay == UpdateTime 3L
         Entity.AnimationSheet == Assets.Gameplay.PlayerImage
         Entity.UpdateEvent => UpdateMessage
         Entity.UpdateEvent => UpdateCommand
         Game.MouseLeftDownEvent => TryJumpByMouse
         Game.KeyboardKeyDownEvent =|> fun evt -> TryJumpByKeyboard evt.Data]

    override this.Message (player, message, entity, world) =

        match message with
        | UpdateMessage ->
            let player =
                if World.getBodyGrounded (entity.GetBodyId world) world
                then { player with LastTimeGrounded = world.UpdateTime }
                else player
            let (player, dying) =
                if player.Alive && (entity.GetPosition world).Y <= -320.0f
                then ({ player with Alive = false }, true)
                else (player, false)
            if dying then withSignal Die player
            elif player.Alive && world.UpdateTime % 5L = 0L then withSignal Shoot player
            else just player

        | TryJumpByMouse ->
            let time = world.UpdateTime
            if  time >= player.LastTimeJump + 12L &&
                time <= player.LastTimeGrounded + 10L then
                let player = { player with LastTimeJump = time }
                withSignal Jump player
            else just player

        | TryJumpByKeyboard keyboardKeyData ->
            match (keyboardKeyData.KeyboardKey, keyboardKeyData.Repeated) with
            | (KeyboardKey.Space, false) ->
                let time = world.UpdateTime
                if  time >= player.LastTimeJump + 12L &&
                    time <= player.LastTimeGrounded + 10L then
                    let player = { player with LastTimeJump = time }
                    withSignal Jump player
                else just player
            | _ -> just player

    override this.Command (player, command, entity, world) =

        match command with
        | UpdateCommand ->
            if player.Alive then
                let bodyId = entity.GetBodyId world
                let groundTangentOpt = World.getBodyToGroundContactTangentOpt bodyId world
                let force =
                    match groundTangentOpt with
                    | Some groundTangent ->
                        let downForce = if groundTangent.Y > 0.0f then Constants.Gameplay.PlayerClimbForce else 0.0f
                        Vector3.Multiply (groundTangent, v3 Constants.Gameplay.PlayerWalkForce downForce 0.0f)
                    | None -> v3 Constants.Gameplay.PlayerWalkForce Constants.Gameplay.PlayerFallForce 0.0f
                World.applyBodyForce force None bodyId world

        | Jump ->
            World.applyBodyLinearImpulse (v3 0.0f Constants.Gameplay.PlayerJumpForce 0.0f) None (entity.GetBodyId world) world
            World.playSound Constants.Audio.SoundVolumeDefault Assets.Gameplay.JumpSound world

        | Shoot ->
            let bullet = World.createEntity<BulletDispatcher> None NoOverlay None entity.Group world // OPTIMIZATION: NoOverlay to avoid reflection.
            bullet.SetPosition (entity.GetPosition world + v3 24.0f 1.0f 0.0f) world
            bullet.SetElevation (entity.GetElevation world) world
            World.applyBodyLinearImpulse (v3 Constants.Gameplay.BulletForce 0.0f 0.0f) None (bullet.GetBodyId world) world
            World.playSound Constants.Audio.SoundVolumeDefault Assets.Gameplay.ShotSound world

        | Die ->
            World.publish entity entity.DeathEvent entity world
            World.playSound Constants.Audio.SoundVolumeDefault Assets.Gameplay.DeathSound world