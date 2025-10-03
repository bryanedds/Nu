namespace TerraFirma
open System
open System.Numerics
open Prime
open Nu
open TerraFirma

// this represents the state of gameplay simulation.
type GameplayState =
    | Playing
    | Quit

// this extends the Screen API to expose the Gameplay model as well as the Quit event.
[<AutoOpen>]
module GameplayExtensions =
    type Screen with
        member this.GetGameplayState world : GameplayState = this.Get (nameof Screen.GameplayState) world
        member this.SetGameplayState (value : GameplayState) world = this.Set (nameof Screen.GameplayState) value world
        member this.GameplayState = lens (nameof Screen.GameplayState) this this.GetGameplayState this.SetGameplayState
        member this.GetScore world : int = this.Get (nameof Screen.Score) world
        member this.SetScore (value : int) world = this.Set (nameof Screen.Score) value world
        member this.Score = lens (nameof Screen.Score) this this.GetScore this.SetScore

// this is the dispatcher that defines the behavior of the screen where gameplay takes place.
type GameplayDispatcher () =
    inherit ScreenDispatcherImSim ()

    // here we define default property values
    static member Properties =
        [define Screen.GameplayState Quit
         define Screen.Score 0]

    // here we define the behavior of our gameplay
    override this.Process (selectionResults, screen, world) =

        // only process when selected
        if screen.GetSelected world then

            // process screen selection
            let selecting = FQueue.contains Select selectionResults
            if selecting then
                Simulants.Gameplay.SetGameplayState Playing world
                Simulants.Gameplay.SetScore 0 world

            // begin scene declaration, processing nav sync at end of frame since optimized representations like
            // frozen entities won't have their nav info registered until then
            World.beginGroupFromFile Simulants.GameplayScene.Name "Assets/Gameplay/Scene.nugroup" [] world
            if selecting then World.defer (World.synchronizeNav3d false (Some "Assets/Gameplay/Scene.nav") screen) screen world

            // declare player
            World.doEntity<PlayerDispatcher> Simulants.GameplayPlayer.Name
                [Entity.Position |= v3 0.0f 1.65f 0.0f
                 Entity.Elevation .= 1.0f] world

            // collect characters for processing
            let characters = World.getEntitiesAs<CharacterDispatcher> Simulants.GameplayScene world

            // process character attacks
            for character in characters do
                for attacked in World.doSubscription "Attack" character.AttackEvent world do
                    if attacked.GetExists world then
                        attacked.HitPoints.Map dec world
                        if attacked.GetHitPoints world > 0 then
                            if not (attacked.GetActionState world).IsInjuryState then
                                attacked.SetActionState (InjuryState { InjuryTime = world.UpdateTime }) world
                                attacked.SetLinearVelocity (v3Up * attacked.GetLinearVelocity world) world
                                World.playSound Constants.Audio.SoundVolumeDefault Assets.Gameplay.InjureSound world
                        else
                            if not (attacked.GetActionState world).IsWoundState then
                                attacked.SetActionState (WoundState { WoundTime = world.UpdateTime }) world
                                attacked.SetLinearVelocity (v3Up * attacked.GetLinearVelocity world) world
                                World.playSound Constants.Audio.SoundVolumeDefault Assets.Gameplay.InjureSound world

            // process character deaths
            for character in characters do
                for dead in World.doSubscription "Death" character.DeathEvent world do
                    if dead.GetExists world then
                        match dead.GetCharacterType world with
                        | Enemy ->
                            World.destroyEntity dead world
                            screen.Score.Map ((+) 100) world
                        | Player ->
                            screen.SetGameplayState Quit world

            // update sun to shine over player as snapped to shadow map's texel grid in shadow space. This is similar
            // in concept to - https://learn.microsoft.com/en-us/windows/win32/dxtecharts/common-techniques-to-improve-shadow-depth-maps?redirectedfrom=MSDN#moving-the-light-in-texel-sized-increments
            let sun = Simulants.GameplaySun
            let shadowOrigin = sun.GetPosition world
            let shadowRotation = sun.GetRotation world
            let shadowForward = shadowRotation.Down
            let shadowUp = shadowForward.OrthonormalUp
            let shadowView = Matrix4x4.CreateLookAt (shadowOrigin, shadowOrigin + shadowForward, shadowUp)
            let shadowWidth = max (sun.GetLightCutoff world * 2.0f) (Constants.Render.NearPlaneDistanceInterior * 2.0f)
            let shadowTexelSize = shadowWidth / single world.GeometryViewport.ShadowTextureResolution.X // assuming square shadow texture, of course
            let position = Simulants.GameplayPlayer.GetPositionInterpolated world
            let positionShadow = position.Transform shadowView
            let positionSnapped =
                v3
                    (floor (positionShadow.X / shadowTexelSize) * shadowTexelSize)
                    (floor (positionShadow.Y / shadowTexelSize) * shadowTexelSize)
                    (floor (positionShadow.Z / shadowTexelSize) * shadowTexelSize)
            let position = positionSnapped.Transform shadowView.Inverted
            sun.SetPositionLocal position world

            // update eye to look at player while game is advancing
            if world.Advancing then
                let position = Simulants.GameplayPlayer.GetPositionInterpolated world
                let rotation = Simulants.GameplayPlayer.GetRotationInterpolated world * Quaternion.CreateFromAxisAngle (v3Right, -0.1f)
                World.setEye3dCenter (position + v3Up * 1.75f - rotation.Forward * 3.0f) world
                World.setEye3dRotation rotation world

            // declare score text
            let scoreText = "Score: " + string (screen.GetScore world)
            World.doText "Score" [Entity.Position .= v3 260.0f 155.0f 0.0f; Entity.Elevation .= 10.0f; Entity.Text @= scoreText] world

            // declare pause button
            if World.doButton "Pause" [Entity.Position .= v3 232.0f -104.0f 0.0f; Entity.Elevation .= 10.0f; Entity.Text .= "Pause"] world then
                World.setAdvancing (not world.Advancing) world

            // declare quit button
            if World.doButton "Quit" [Entity.Position .= v3 232.0f -144.0f 0.0f; Entity.Elevation .= 10.0f; Entity.Text .= "Quit"] world then
                screen.SetGameplayState Quit world

            // end scene declaration
            World.endGroup world