// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu
open System
open System.Collections.Generic
open System.Numerics
open BulletSharp
open Prime
open Nu

/// Tracks Bullet physics bodies by their BodyIds.
type internal BulletBodyDictionary = OrderedDictionary<BodyId, Vector3 option * RigidBody>

/// Tracks Bullet physics ghosts by their BodyIds.
type internal BulletGhostDictionary = OrderedDictionary<BodyId, GhostObject>

/// Tracks Bullet physics collision objects by their BodyIds.
type internal BulletObjectDictionary = OrderedDictionary<BodyId, CollisionObject>

/// Tracks Bullet physics constraints by their BodyIds.
type internal BulletConstraintDictionary = OrderedDictionary<JointId, TypedConstraint>

/// The BulletPhysics 3d implementation of PhysicsEngine.
/// TODO: only record the collisions for bodies that have event subscriptions associated with them.
type [<ReferenceEquality>] BulletPhysicsEngine =
    private
        { PhysicsContext : DynamicsWorld
          Constraints : BulletConstraintDictionary
          Bodies : BulletBodyDictionary
          Ghosts : BulletGhostDictionary
          Objects : BulletObjectDictionary
          Collisions : SDictionary<BodyId * BodyId, Vector3>
          CollisionConfiguration : CollisionConfiguration
          PhysicsDispatcher : Dispatcher
          BroadPhaseInterface : BroadphaseInterface
          ConstraintSolver : ConstraintSolver
          StaticModels : Dictionary<StaticModel AssetTag, OpenGL.PhysicallyBased.PhysicallyBasedStaticModel>
          PhysicsMessages : PhysicsMessage UList
          IntegrationMessages : IntegrationMessage List }

    static member private handleCollision physicsEngine (bodyId : BodyId) (bodyId2 : BodyId) normal =
        let bodyCollisionMessage =
            { BodyShapeSource = { BodyId = bodyId; ShapeIndex = 0 }
              BodyShapeSource2 = { BodyId = bodyId2; ShapeIndex = 0 }
              Normal = normal }
        let integrationMessage = BodyCollisionMessage bodyCollisionMessage
        physicsEngine.IntegrationMessages.Add integrationMessage
    
    static member private handleSeparation physicsEngine (bodyId : BodyId) (bodyId2 : BodyId) =
        let bodySeparationMessage =
            { BodyShapeSource = { BodyId = bodyId; ShapeIndex = 0 }
              BodyShapeSource2 = { BodyId = bodyId2; ShapeIndex = 0 }}
        let integrationMessage = BodySeparationMessage bodySeparationMessage
        physicsEngine.IntegrationMessages.Add integrationMessage

    static member private configureBodyShapeProperties (_ : BodyProperties) (_ : BodyShapeProperties option) (shape : ConvexInternalShape) =
        shape.Margin <- Constants.Physics.CollisionMargin3d

    static member private configureCollisionObjectProperties (bodyProperties : BodyProperties) (object : CollisionObject) =
        match bodyProperties.Enabled with
        | true -> object.ActivationState <- ActivationState.ActiveTag
        | false -> object.ActivationState <- ActivationState.DisableSimulation
        object.Friction <- bodyProperties.Friction
        object.Restitution <- bodyProperties.Restitution
        match bodyProperties.CollisionDetection with
        | Discontinuous ->
            object.CcdMotionThreshold <- 0.0f
            object.CcdSweptSphereRadius <- 0.0f
        | Continuous (motionThreshold, sweptSphereRadius) ->
            object.CcdMotionThreshold <- motionThreshold
            object.CcdSweptSphereRadius <- sweptSphereRadius
        match bodyProperties.BodyType with
        | Static ->
            object.CollisionFlags <- object.CollisionFlags ||| CollisionFlags.StaticObject
            object.CollisionFlags <- object.CollisionFlags &&& ~~~CollisionFlags.KinematicObject
        | Dynamic ->
            object.CollisionFlags <- object.CollisionFlags &&& ~~~CollisionFlags.StaticObject
            object.CollisionFlags <- object.CollisionFlags &&& ~~~CollisionFlags.KinematicObject
        | Kinematic ->
            object.CollisionFlags <- object.CollisionFlags ||| CollisionFlags.KinematicObject
            object.CollisionFlags <- object.CollisionFlags &&& ~~~CollisionFlags.StaticObject
        //object.IsBullet <- bodyProperties.Bullet // TODO: see if we can find a Bullet equivalent of this to Aether.

    static member private configureBodyProperties (bodyProperties : BodyProperties) (body : RigidBody) gravity =
        BulletPhysicsEngine.configureCollisionObjectProperties bodyProperties body
        body.WorldTransform <- Matrix4x4.CreateFromTrs (bodyProperties.Center, bodyProperties.Rotation, v3One)
        if bodyProperties.SleepingAllowed // TODO: see if we can find a more reliable way to disable sleeping.
        then body.SetSleepingThresholds (Constants.Physics.SleepingThresholdLinear, Constants.Physics.SleepingThresholdAngular)
        else body.SetSleepingThresholds (0.0f, 0.0f)
        body.LinearVelocity <- bodyProperties.LinearVelocity
        body.LinearFactor <- if bodyProperties.BodyType = Static then v3Zero else v3One
        body.AngularVelocity <- bodyProperties.AngularVelocity
        body.AngularFactor <- if bodyProperties.BodyType = Static then v3Zero else bodyProperties.AngularFactor
        body.SetDamping (bodyProperties.LinearDamping, bodyProperties.AngularDamping)
        body.Gravity <- match bodyProperties.GravityOverride with Some gravityOverride -> gravityOverride | None -> gravity

    static member private attachBodyBox bodySource (bodyProperties : BodyProperties) (bodyBox : BodyBox) (compoundShape : CompoundShape) centerMassInertias =
        let box = new BoxShape (bodyBox.Size * 0.5f)
        BulletPhysicsEngine.configureBodyShapeProperties bodyProperties bodyBox.PropertiesOpt box
        box.UserObject <-
            { BodyId = { BodySource = bodySource; BodyIndex = bodyProperties.BodyIndex }
              ShapeIndex = match bodyBox.PropertiesOpt with Some p -> p.ShapeIndex | None -> 0 }
        let center =
            match bodyBox.TransformOpt with
            | Some transform -> transform.Translation
            | None -> v3Zero
        let mass =
            match bodyProperties.Substance with
            | Density density ->
                let volume = bodyBox.Size.X * bodyBox.Size.Y * bodyBox.Size.Z
                volume * density
            | Mass mass -> mass
        let inertia = box.CalculateLocalInertia mass
        compoundShape.AddChildShape (Option.defaultValue m4Identity bodyBox.TransformOpt, box)
        (center, mass, inertia) :: centerMassInertias

    static member private attachBodySphere bodySource (bodyProperties : BodyProperties) (bodySphere : BodySphere) (compoundShape : CompoundShape) centerMassInertias =
        let sphere = new SphereShape (bodySphere.Radius)
        BulletPhysicsEngine.configureBodyShapeProperties bodyProperties bodySphere.PropertiesOpt sphere
        sphere.UserObject <-
            { BodyId = { BodySource = bodySource; BodyIndex = bodyProperties.BodyIndex }
              ShapeIndex = match bodySphere.PropertiesOpt with Some p -> p.ShapeIndex | None -> 0 }
        let center =
            match bodySphere.TransformOpt with
            | Some transform -> transform.Translation
            | None -> v3Zero
        let mass =
            match bodyProperties.Substance with
            | Density density ->
                let volume = 4.0f / 3.0f * MathF.PI * pown bodySphere.Radius 3
                volume * density
            | Mass mass -> mass
        let inertia = sphere.CalculateLocalInertia mass
        compoundShape.AddChildShape (Option.defaultValue m4Identity bodySphere.TransformOpt, sphere)
        (center, mass, inertia) :: centerMassInertias

    static member private attachBodyCapsule bodySource (bodyProperties : BodyProperties) (bodyCapsule : BodyCapsule) (compoundShape : CompoundShape) centerMassInertias =
        let capsule = new CapsuleShape (bodyCapsule.Radius, bodyCapsule.Height)
        BulletPhysicsEngine.configureBodyShapeProperties bodyProperties bodyCapsule.PropertiesOpt capsule
        capsule.UserObject <-
            { BodyId = { BodySource = bodySource; BodyIndex = bodyProperties.BodyIndex }
              ShapeIndex = match bodyCapsule.PropertiesOpt with Some p -> p.ShapeIndex | None -> 0 }
        let center =
            match bodyCapsule.TransformOpt with
            | Some transform -> transform.Translation
            | None -> v3Zero
        let mass =
            match bodyProperties.Substance with
            | Density density ->
                let volume = MathF.PI * pown bodyCapsule.Radius 2 * (4.0f / 3.0f * bodyCapsule.Radius * bodyCapsule.Height)
                volume * density
            | Mass mass -> mass
        let inertia = capsule.CalculateLocalInertia mass
        compoundShape.AddChildShape (Option.defaultValue m4Identity bodyCapsule.TransformOpt, capsule)
        (center, mass, inertia) :: centerMassInertias

    static member private attachBodyBoxRounded bodySource (bodyProperties : BodyProperties) (bodyBoxRounded : BodyBoxRounded) (compoundShape : CompoundShape) centerMassInertias =
        Log.debugOnce "Rounded box not yet implemented via BulletPhysicsEngine; creating a normal box instead."
        let bodyBox = { Size = bodyBoxRounded.Size; TransformOpt = bodyBoxRounded.TransformOpt; PropertiesOpt = bodyBoxRounded.PropertiesOpt }
        BulletPhysicsEngine.attachBodyBox bodySource bodyProperties bodyBox compoundShape centerMassInertias

    static member private attachBodyConvexHull bodySource (bodyProperties : BodyProperties) (bodyConvexHull : BodyConvexHull) (compoundShape : CompoundShape) centerMassInertias =
        let hull = new ConvexHullShape (bodyConvexHull.Vertices)
        BulletPhysicsEngine.configureBodyShapeProperties bodyProperties bodyConvexHull.PropertiesOpt hull
        hull.OptimizeConvexHull () // TODO: instead of always optimizing hull, instead consider caching hull.UnscaledPoints after optimizing the first time and reusing that.
        hull.UserObject <-
            { BodyId = { BodySource = bodySource; BodyIndex = bodyProperties.BodyIndex }
              ShapeIndex = match bodyConvexHull.PropertiesOpt with Some p -> p.ShapeIndex | None -> 0 }
        // NOTE: we approximate volume with the volume of a bounding box.
        // TODO: use a more accurate volume calculation.
        let mutable min = v3Zero
        let mutable max = v3Zero
        hull.GetAabb (m4Identity, &min, &max)
        let center =
            let centerUntransformed = (min + max) * 0.5f
            match bodyConvexHull.TransformOpt with
            | Some transform -> Vector3.Transform (centerUntransformed, transform)
            | None -> centerUntransformed
        let box = box3 min max
        let mass =
            match bodyProperties.Substance with
            | Density density ->
                let volume = box.Width * box.Height * box.Depth
                volume * density
            | Mass mass -> mass
        let inertia = hull.CalculateLocalInertia mass
        compoundShape.AddChildShape (Option.defaultValue m4Identity bodyConvexHull.TransformOpt, hull)
        (center, mass, inertia) :: centerMassInertias

    // TODO: add some error logging.
    static member private attachBodyStaticModel bodySource (bodyProperties : BodyProperties) (bodyStaticModel : BodyStaticModel) (compoundShape : CompoundShape) centerMassInertias physicsEngine =
        match physicsEngine.StaticModels.TryGetValue bodyStaticModel.StaticModel with
        | (true, staticModel) ->
            Seq.fold (fun centerMassInertias i ->
                let surface = staticModel.Surfaces.[i]
                let transform =
                    match bodyStaticModel.TransformOpt with
                    | Some transform -> transform * surface.SurfaceMatrix
                    | None -> surface.SurfaceMatrix
                let bodyStaticModelSurface = { SurfaceIndex = i; StaticModel = bodyStaticModel.StaticModel; TransformOpt = Some transform; PropertiesOpt = bodyStaticModel.PropertiesOpt }
                BulletPhysicsEngine.attachBodyStaticModelSurface bodySource bodyProperties bodyStaticModelSurface compoundShape centerMassInertias physicsEngine)
                centerMassInertias
                [0 .. dec staticModel.Surfaces.Length]
        | (false, _) -> centerMassInertias

    // TODO: add some error logging.
    static member private attachBodyStaticModelSurface bodySource (bodyProperties : BodyProperties) (bodyStaticModelSurface : BodyStaticModelSurface) (compoundShape : CompoundShape) centerMassInertias physicsEngine =
        match physicsEngine.StaticModels.TryGetValue bodyStaticModelSurface.StaticModel with
        | (true, staticModel) ->
            if  bodyStaticModelSurface.SurfaceIndex > -1 &&
                bodyStaticModelSurface.SurfaceIndex < staticModel.Surfaces.Length then
                let geometry = staticModel.Surfaces.[bodyStaticModelSurface.SurfaceIndex].PhysicallyBasedGeometry
                let bodyConvexHull = { Vertices = geometry.Vertices; TransformOpt = bodyStaticModelSurface.TransformOpt; PropertiesOpt = bodyStaticModelSurface.PropertiesOpt }
                BulletPhysicsEngine.attachBodyConvexHull bodySource bodyProperties bodyConvexHull compoundShape centerMassInertias
            else centerMassInertias
        | (false, _) -> centerMassInertias

    static member private attachBodyShapes bodySource bodyProperties bodyShapes compoundShape centerMassInertias physicsEngine =
        List.fold (fun centerMassInertias bodyShape ->
            let centerMassInertias' = BulletPhysicsEngine.attachBodyShape bodySource bodyProperties bodyShape compoundShape centerMassInertias physicsEngine
            centerMassInertias' @ centerMassInertias)
            centerMassInertias
            bodyShapes

    static member private attachBodyShape bodySource bodyProperties bodyShape compoundShape centerMassInertias physicsEngine =
        match bodyShape with
        | BodyEmpty -> centerMassInertias
        | BodyBox bodyBox -> BulletPhysicsEngine.attachBodyBox bodySource bodyProperties bodyBox compoundShape centerMassInertias
        | BodySphere bodySphere -> BulletPhysicsEngine.attachBodySphere bodySource bodyProperties bodySphere compoundShape centerMassInertias
        | BodyCapsule bodyCapsule -> BulletPhysicsEngine.attachBodyCapsule bodySource bodyProperties bodyCapsule compoundShape centerMassInertias
        | BodyBoxRounded bodyBoxRounded -> BulletPhysicsEngine.attachBodyBoxRounded bodySource bodyProperties bodyBoxRounded compoundShape centerMassInertias
        | BodyConvexHull bodyConvexHull -> BulletPhysicsEngine.attachBodyConvexHull bodySource bodyProperties bodyConvexHull compoundShape centerMassInertias
        | BodyStaticModel bodyStaticModel -> BulletPhysicsEngine.attachBodyStaticModel bodySource bodyProperties bodyStaticModel compoundShape centerMassInertias physicsEngine
        | BodyStaticModelSurface bodyStaticModelSurface -> BulletPhysicsEngine.attachBodyStaticModelSurface bodySource bodyProperties bodyStaticModelSurface compoundShape centerMassInertias physicsEngine
        | BodyShapes bodyShapes -> BulletPhysicsEngine.attachBodyShapes bodySource bodyProperties bodyShapes compoundShape centerMassInertias physicsEngine

    static member private createBody3 attachBodyShape (bodyId : BodyId) (bodyProperties : BodyProperties) physicsEngine =
        let (shape, centerMassInertias) =
            let compoundShape = new CompoundShape ()
            let centerMassInertias = attachBodyShape bodyProperties compoundShape []
            (compoundShape, centerMassInertias)
        let userIndex = if bodyId.BodyIndex = Constants.Physics.InternalIndex then -1 else 1
        if not bodyProperties.Sensor then
            let (_, mass, inertia) =
                // TODO: make this more accurate by making each c weighted proportionately to its respective m.
                List.reduce (fun (c, m, i) (c', m', i') -> (c + c', m + m', i + i')) centerMassInertias
            let constructionInfo = new RigidBodyConstructionInfo (mass, new DefaultMotionState (), shape, inertia)
            let body = new RigidBody (constructionInfo)
            body.WorldTransform <- Matrix4x4.CreateFromTrs (bodyProperties.Center, bodyProperties.Rotation, v3One)
            body.UserObject <- bodyId
            body.UserIndex <- userIndex
            BulletPhysicsEngine.configureBodyProperties bodyProperties body physicsEngine.PhysicsContext.Gravity
            physicsEngine.PhysicsContext.AddRigidBody (body, bodyProperties.CollisionCategories, bodyProperties.CollisionMask)
            if physicsEngine.Bodies.TryAdd (bodyId, (bodyProperties.GravityOverride, body))
            then physicsEngine.Objects.Add (bodyId, body)
            else Log.debug ("Could not add body for '" + scstring bodyId + "'.")
        else
            let ghost = new GhostObject ()
            ghost.CollisionShape <- shape
            ghost.CollisionFlags <- ghost.CollisionFlags &&& ~~~CollisionFlags.NoContactResponse
            ghost.UserObject <- bodyId
            ghost.UserIndex <- userIndex
            BulletPhysicsEngine.configureCollisionObjectProperties bodyProperties ghost
            physicsEngine.PhysicsContext.AddCollisionObject (ghost, bodyProperties.CollisionCategories, bodyProperties.CollisionMask)
            if physicsEngine.Ghosts.TryAdd (bodyId, ghost)
            then physicsEngine.Objects.Add (bodyId, ghost)
            else Log.debug ("Could not add body for '" + scstring bodyId + "'.")

    static member private createBody4 bodyShape (bodyId : BodyId) bodyProperties physicsEngine =
        BulletPhysicsEngine.createBody3 (fun ps cs cmas ->
            BulletPhysicsEngine.attachBodyShape bodyId.BodySource ps bodyShape cs cmas physicsEngine)
            bodyId bodyProperties physicsEngine

    static member private createBody (createBodyMessage : CreateBodyMessage) physicsEngine =
        let bodyId = createBodyMessage.BodyId
        let bodyProperties = createBodyMessage.BodyProperties
        BulletPhysicsEngine.createBody4 bodyProperties.BodyShape bodyId bodyProperties physicsEngine

    static member private createBodies (createBodiesMessage : CreateBodiesMessage) physicsEngine =
        List.iter
            (fun (bodyProperties : BodyProperties) ->
                let createBodyMessage =
                    { BodyId = { BodySource = createBodiesMessage.BodySource; BodyIndex = bodyProperties.BodyIndex }
                      BodyProperties = bodyProperties }
                BulletPhysicsEngine.createBody createBodyMessage physicsEngine)
            createBodiesMessage.BodiesProperties

    static member private destroyBody (destroyBodyMessage : DestroyBodyMessage) physicsEngine =
        let bodyId = destroyBodyMessage.BodyId
        match physicsEngine.Objects.TryGetValue bodyId with
        | (true, object) ->
            match object with
            | :? RigidBody as body ->
                physicsEngine.Objects.Remove bodyId |> ignore
                physicsEngine.Bodies.Remove bodyId |> ignore
                physicsEngine.PhysicsContext.RemoveRigidBody body
            | :? GhostObject as ghost ->
                physicsEngine.Objects.Remove bodyId |> ignore
                physicsEngine.Ghosts.Remove bodyId |> ignore
                physicsEngine.PhysicsContext.RemoveCollisionObject ghost
            | _ -> ()
        | (false, _) -> ()

    static member private destroyBodies (destroyBodiesMessage : DestroyBodiesMessage) physicsEngine =
        List.iter (fun bodyId ->
            BulletPhysicsEngine.destroyBody { BodyId = bodyId } physicsEngine)
            destroyBodiesMessage.BodyIds

    static member private createJoint (createJointMessage : CreateJointMessage) physicsEngine =
        let jointProperties = createJointMessage.JointProperties
        let jointId = { JointSource = createJointMessage.JointSource; JointIndex = jointProperties.JointIndex }
        match jointProperties.JointDevice with
        | JointEmpty -> ()
        | JointAngle jointAngle ->
            match (physicsEngine.Bodies.TryGetValue jointAngle.TargetId, physicsEngine.Bodies.TryGetValue jointAngle.TargetId2) with
            | ((true, (_, body)), (true, (_, body2))) ->
                let hinge = new HingeConstraint (body, body2, jointAngle.Anchor, jointAngle.Anchor2, jointAngle.Axis, jointAngle.Axis2)
                hinge.SetLimit (jointAngle.AngleMin, jointAngle.AngleMax, jointAngle.Softness, jointAngle.BiasFactor, jointAngle.RelaxationFactor)
                hinge.BreakingImpulseThreshold <- jointAngle.BreakImpulseThreshold
                physicsEngine.PhysicsContext.AddConstraint (hinge, false)
                if physicsEngine.Constraints.TryAdd (jointId, hinge)
                then () // nothing to do
                else Log.debug ("Could not add joint via '" + scstring createJointMessage + "'.")
            | (_, _) -> Log.debug "Could not create a joint for one or more non-existent bodies."
        | _ -> failwithnie ()

    static member private createJoints (createJointsMessage : CreateJointsMessage) physicsEngine =
        List.iter
            (fun (jointProperties : JointProperties) ->
                let createJointMessage =
                    { JointSource = createJointsMessage.JointsSource
                      JointProperties = jointProperties }
                BulletPhysicsEngine.createJoint createJointMessage physicsEngine)
            createJointsMessage.JointsProperties

    static member private destroyJoint (destroyJointMessage : DestroyJointMessage) physicsEngine =
        match physicsEngine.Constraints.TryGetValue destroyJointMessage.JointId with
        | (true, contrain) ->
            physicsEngine.Constraints.Remove destroyJointMessage.JointId |> ignore
            physicsEngine.PhysicsContext.RemoveConstraint contrain
        | (false, _) -> ()

    static member private destroyJoints (destroyJointsMessage : DestroyJointsMessage) physicsEngine =
        List.iter (fun jointId ->
            BulletPhysicsEngine.destroyJoint { JointId = jointId } physicsEngine)
            destroyJointsMessage.JointIds

    static member private setBodyEnabled (setBodyEnabledMessage : SetBodyEnabledMessage) physicsEngine =
        match physicsEngine.Objects.TryGetValue setBodyEnabledMessage.BodyId with
        | (true, object) ->
            object.ActivationState <-
                if setBodyEnabledMessage.Enabled
                then ActivationState.ActiveTag
                else ActivationState.DisableSimulation
        | (false, _) -> ()

    static member private setBodyCenter (setBodyCenterMessage : SetBodyCenterMessage) physicsEngine =
        match physicsEngine.Objects.TryGetValue setBodyCenterMessage.BodyId with
        | (true, object) ->
            let mutable transform = object.WorldTransform
            transform.Translation <- setBodyCenterMessage.Center
            object.WorldTransform <- transform
        | (false, _) -> ()

    static member private setBodyRotation (setBodyRotationMessage : SetBodyRotationMessage) physicsEngine =
        match physicsEngine.Objects.TryGetValue setBodyRotationMessage.BodyId with
        | (true, object) -> object.WorldTransform <- object.WorldTransform.SetRotation setBodyRotationMessage.Rotation
        | (false, _) -> ()

    static member private setBodyAngularVelocity (setBodyAngularVelocityMessage : SetBodyAngularVelocityMessage) physicsEngine =
        match physicsEngine.Objects.TryGetValue setBodyAngularVelocityMessage.BodyId with
        | (true, (:? RigidBody as body)) -> body.AngularVelocity <- setBodyAngularVelocityMessage.AngularVelocity
        | (_, _) -> ()

    static member private applyBodyAngularImpulse (applyBodyAngularImpulseMessage : ApplyBodyAngularImpulseMessage) physicsEngine =
        match physicsEngine.Objects.TryGetValue applyBodyAngularImpulseMessage.BodyId with
        | (true, (:? RigidBody as body)) -> body.ApplyTorqueImpulse (applyBodyAngularImpulseMessage.AngularImpulse)
        | (_, _) -> ()

    static member private setBodyLinearVelocity (setBodyLinearVelocityMessage : SetBodyLinearVelocityMessage) physicsEngine =
        match physicsEngine.Objects.TryGetValue setBodyLinearVelocityMessage.BodyId with
        | (true, (:? RigidBody as body)) -> body.LinearVelocity <- setBodyLinearVelocityMessage.LinearVelocity
        | (_, _) -> ()

    static member private applyBodyLinearImpulse (applyBodyLinearImpulseMessage : ApplyBodyLinearImpulseMessage) physicsEngine =
        match physicsEngine.Objects.TryGetValue applyBodyLinearImpulseMessage.BodyId with
        | (true, (:? RigidBody as body)) -> body.ApplyImpulse (applyBodyLinearImpulseMessage.LinearImpulse, applyBodyLinearImpulseMessage.Offset)
        | (_, _) -> ()

    static member private applyBodyForce (applyBodyForceMessage : ApplyBodyForceMessage) physicsEngine =
        match physicsEngine.Objects.TryGetValue applyBodyForceMessage.BodyId with
        | (true, (:? RigidBody as body)) -> body.ApplyForce (applyBodyForceMessage.Force, applyBodyForceMessage.Offset)
        | (_, _) -> ()

    static member private applyBodyTorque (applyBodyTorqueMessage : ApplyBodyTorqueMessage) physicsEngine =
        match physicsEngine.Objects.TryGetValue applyBodyTorqueMessage.BodyId with
        | (true, (:? RigidBody as body)) -> body.ApplyTorque applyBodyTorqueMessage.Torque
        | (_, _) -> ()

    static member private setBodyObservable (setBodyObservableMessage : SetBodyObservableMessage) physicsEngine =
        match physicsEngine.Bodies.TryGetValue setBodyObservableMessage.BodyId with
        | (true, (_, body)) -> body.UserIndex <- if setBodyObservableMessage.Observable then 1 else -1
        | (false, _) ->
            match physicsEngine.Ghosts.TryGetValue setBodyObservableMessage.BodyId with
            | (true, ghost) -> ghost.UserIndex <- if setBodyObservableMessage.Observable then 1 else -1
            | (false, _) -> ()

    static member private handlePhysicsMessage physicsEngine physicsMessage =
        match physicsMessage with
        | CreateBodyMessage createBodyMessage -> BulletPhysicsEngine.createBody createBodyMessage physicsEngine
        | CreateBodiesMessage createBodiesMessage -> BulletPhysicsEngine.createBodies createBodiesMessage physicsEngine
        | DestroyBodyMessage destroyBodyMessage -> BulletPhysicsEngine.destroyBody destroyBodyMessage physicsEngine
        | DestroyBodiesMessage destroyBodiesMessage -> BulletPhysicsEngine.destroyBodies destroyBodiesMessage physicsEngine
        | CreateJointMessage createJointMessage -> BulletPhysicsEngine.createJoint createJointMessage physicsEngine
        | CreateJointsMessage createJointsMessage -> BulletPhysicsEngine.createJoints createJointsMessage physicsEngine
        | DestroyJointMessage destroyJointMessage -> BulletPhysicsEngine.destroyJoint destroyJointMessage physicsEngine
        | DestroyJointsMessage destroyJointsMessage -> BulletPhysicsEngine.destroyJoints destroyJointsMessage physicsEngine
        | SetBodyEnabledMessage setBodyEnabledMessage -> BulletPhysicsEngine.setBodyEnabled setBodyEnabledMessage physicsEngine
        | SetBodyCenterMessage setBodyCenterMessage -> BulletPhysicsEngine.setBodyCenter setBodyCenterMessage physicsEngine
        | SetBodyRotationMessage setBodyRotationMessage -> BulletPhysicsEngine.setBodyRotation setBodyRotationMessage physicsEngine
        | SetBodyAngularVelocityMessage setBodyAngularVelocityMessage -> BulletPhysicsEngine.setBodyAngularVelocity setBodyAngularVelocityMessage physicsEngine
        | ApplyBodyAngularImpulseMessage applyBodyAngularImpulseMessage -> BulletPhysicsEngine.applyBodyAngularImpulse applyBodyAngularImpulseMessage physicsEngine
        | SetBodyLinearVelocityMessage setBodyLinearVelocityMessage -> BulletPhysicsEngine.setBodyLinearVelocity setBodyLinearVelocityMessage physicsEngine
        | ApplyBodyLinearImpulseMessage applyBodyLinearImpulseMessage -> BulletPhysicsEngine.applyBodyLinearImpulse applyBodyLinearImpulseMessage physicsEngine
        | ApplyBodyForceMessage applyBodyForceMessage -> BulletPhysicsEngine.applyBodyForce applyBodyForceMessage physicsEngine
        | ApplyBodyTorqueMessage applyBodyTorqueMessage -> BulletPhysicsEngine.applyBodyTorque applyBodyTorqueMessage physicsEngine
        | SetBodyObservableMessage setBodyObservableMessage -> BulletPhysicsEngine.setBodyObservable setBodyObservableMessage physicsEngine
        | SetGravityMessage gravity ->
            physicsEngine.PhysicsContext.Gravity <- gravity
            for (gravityOverride, body) in physicsEngine.Bodies.Values do
                match gravityOverride with
                | Some gravity -> body.Gravity <- gravity
                | None -> body.Gravity <- gravity
        | ClearPhysicsMessageInternal ->
            for constrain in physicsEngine.Constraints.Values do physicsEngine.PhysicsContext.RemoveConstraint constrain
            physicsEngine.Objects.Clear ()
            physicsEngine.Constraints.Clear ()
            for ghost in physicsEngine.Ghosts.Values do physicsEngine.PhysicsContext.RemoveCollisionObject ghost
            physicsEngine.Ghosts.Clear ()
            for (_, body) in physicsEngine.Bodies.Values do physicsEngine.PhysicsContext.RemoveRigidBody body
            physicsEngine.Bodies.Clear ()
            physicsEngine.IntegrationMessages.Clear ()

    static member private integrate stepTime physicsEngine =
        let physicsStepAmount =
            match (Constants.GameTime.DesiredFrameRate, stepTime) with
            | (StaticFrameRate frameRate, UpdateTime frames) -> 1.0f / single frameRate * single frames
            | (DynamicFrameRate _, ClockTime secs) -> secs
            | (_, _) -> failwithumf ()
        if physicsStepAmount > 0.0f then
            let result = physicsEngine.PhysicsContext.StepSimulation physicsStepAmount
            ignore result

    static member private createIntegrationMessages physicsEngine =

        // create collision messages
        let collisionsOld = physicsEngine.Collisions
        physicsEngine.Collisions.Clear ()
        let numManifolds = physicsEngine.PhysicsContext.Dispatcher.NumManifolds
        for i in 0 .. dec numManifolds do
            let manifold = physicsEngine.PhysicsContext.Dispatcher.GetManifoldByIndexInternal i
            let body0 = manifold.Body0
            let body1 = manifold.Body1
            if  body0.UserIndex = 1 ||
                body1.UserIndex = 1 then
                let bodySource0 = body0.UserObject :?> BodyId
                let bodySource1 = body1.UserObject :?> BodyId
                let collisionKey = (bodySource0, bodySource1)
                let mutable normal = v3Zero
                let numContacts = manifold.NumContacts
                for j in 0 .. dec numContacts do
                    let contact = manifold.GetContactPoint j
                    normal <- normal - contact.NormalWorldOnB
                normal <- normal / single numContacts
                physicsEngine.Collisions.Add (collisionKey, normal)

        // create collision messages
        for entry in physicsEngine.Collisions do
            let (bodySourceA, bodySourceB) = entry.Key
            if not (collisionsOld.ContainsKey entry.Key) then
                BulletPhysicsEngine.handleCollision physicsEngine bodySourceA bodySourceB entry.Value
                BulletPhysicsEngine.handleCollision physicsEngine bodySourceB bodySourceA -entry.Value

        // create separation messages
        for entry in collisionsOld do
            let (bodySourceA, bodySourceB) = entry.Key
            if not (physicsEngine.Collisions.ContainsKey entry.Key) then
                BulletPhysicsEngine.handleSeparation physicsEngine bodySourceA bodySourceB
                BulletPhysicsEngine.handleSeparation physicsEngine bodySourceB bodySourceA

        // create transform messages
        for (_, body) in physicsEngine.Bodies.Values do
            if body.IsActive then
                let bodyTransformMessage =
                    BodyTransformMessage
                        { BodyId = body.UserObject :?> BodyId
                          Center = body.WorldTransform.Translation
                          Rotation = body.WorldTransform.Rotation
                          LinearVelocity = body.LinearVelocity
                          AngularVelocity = body.AngularVelocity }
                physicsEngine.IntegrationMessages.Add bodyTransformMessage

    static member private handlePhysicsMessages physicsMessages physicsEngine =
        for physicsMessage in physicsMessages do
            BulletPhysicsEngine.handlePhysicsMessage physicsEngine physicsMessage

    static member make imperative gravity staticModels =
        let config = if imperative then Imperative else Functional
        let physicsMessages = UList.makeEmpty config
        let collisionConfiguration = new DefaultCollisionConfiguration ()
        let physicsDispatcher = new CollisionDispatcher (collisionConfiguration)
        let broadPhaseInterface = new DbvtBroadphase ()
        let constraintSolver = new SequentialImpulseConstraintSolver ()
        let world = new DiscreteDynamicsWorld (physicsDispatcher, broadPhaseInterface, constraintSolver, collisionConfiguration)
        world.Gravity <- gravity
        { PhysicsContext = world
          Constraints = OrderedDictionary HashIdentity.Structural
          Bodies = OrderedDictionary HashIdentity.Structural
          Ghosts = OrderedDictionary HashIdentity.Structural
          Objects = OrderedDictionary HashIdentity.Structural
          Collisions = SDictionary.make HashIdentity.Structural
          CollisionConfiguration = collisionConfiguration
          PhysicsDispatcher = physicsDispatcher
          BroadPhaseInterface = broadPhaseInterface
          ConstraintSolver = constraintSolver
          StaticModels = staticModels
          PhysicsMessages = physicsMessages
          IntegrationMessages = List () }

    static member cleanUp physicsEngine =
        physicsEngine.PhysicsContext.Dispose ()
        physicsEngine.ConstraintSolver.Dispose ()
        physicsEngine.BroadPhaseInterface.Dispose ()
        physicsEngine.PhysicsDispatcher.Dispose ()
        physicsEngine.CollisionConfiguration.Dispose ()

    interface PhysicsEngine with

        member physicsEngine.GetBodyExists bodyId =
            physicsEngine.Objects.ContainsKey bodyId

        member physicsEngine.GetBodyContactNormals bodyId =
            // TODO: see if this can be optimized from a linear-time search to constant-time look-up.
            match physicsEngine.Objects.TryGetValue bodyId with
            | (true, object) ->
                let dispatcher = physicsEngine.PhysicsContext.Dispatcher
                let manifoldCount = dispatcher.NumManifolds
                [for i in 0 .. dec manifoldCount do
                    let manifold = dispatcher.GetManifoldByIndexInternal i
                    if manifold.Body0 = object then
                        let contactCount = manifold.NumContacts
                        for j in 0 .. dec contactCount do
                            let contact = manifold.GetContactPoint j
                            yield -contact.NormalWorldOnB]
            | (false, _) -> []

        member physicsEngine.GetBodyLinearVelocity bodyId =
            match physicsEngine.Bodies.TryGetValue bodyId with
            | (true, (_, body)) -> body.LinearVelocity
            | (false, _) ->
                if physicsEngine.Ghosts.ContainsKey bodyId then v3Zero
                else failwith ("No body with BodyId = " + scstring bodyId + ".")

        member physicsEngine.GetBodyToGroundContactNormals bodyId =
            List.filter
                (fun normal ->
                    let theta = Vector3.Dot (normal, Vector3.UnitY) |> double |> Math.Acos |> Math.Abs
                    theta < Math.PI * 0.25)
                ((physicsEngine :> PhysicsEngine).GetBodyContactNormals bodyId)

        member physicsEngine.GetBodyToGroundContactNormalOpt bodyId =
            let groundNormals = (physicsEngine :> PhysicsEngine).GetBodyToGroundContactNormals bodyId
            match groundNormals with
            | [] -> None
            | _ ->
                let averageNormal = List.reduce (fun normal normal2 -> (normal + normal2) * 0.5f) groundNormals
                Some averageNormal

        member physicsEngine.GetBodyToGroundContactTangentOpt bodyId =
            match (physicsEngine :> PhysicsEngine).GetBodyToGroundContactNormalOpt bodyId with
            | Some normal -> Some (Vector3.Cross (v3Forward, normal))
            | None -> None

        member physicsEngine.IsBodyOnGround bodyId =
            let groundNormals = (physicsEngine :> PhysicsEngine).GetBodyToGroundContactNormals bodyId
            List.notEmpty groundNormals

        member physicsEngine.PopMessages () =
            let messages = physicsEngine.PhysicsMessages
            let physicsEngine = { physicsEngine with PhysicsMessages = UList.makeEmpty (UList.getConfig physicsEngine.PhysicsMessages) }
            (messages, physicsEngine :> PhysicsEngine)

        member physicsEngine.ClearMessages () =
            let physicsEngine = { physicsEngine with PhysicsMessages = UList.makeEmpty (UList.getConfig physicsEngine.PhysicsMessages) }
            physicsEngine :> PhysicsEngine

        member physicsEngine.EnqueueMessage physicsMessage =
#if HANDLE_PHYSICS_MESSAGES_IMMEDIATE
            BulletPhysicsEngine.handlePhysicsMessage physicsEngine physicsMessage
            physicsEngine
#else
            let physicsMessages = UList.add physicsMessage physicsEngine.PhysicsMessages
            let physicsEngine = { physicsEngine with PhysicsMessages = physicsMessages }
            physicsEngine :> PhysicsEngine
#endif

        member physicsEngine.Integrate stepTime physicsMessages =
            BulletPhysicsEngine.handlePhysicsMessages physicsMessages physicsEngine
            BulletPhysicsEngine.integrate stepTime physicsEngine
            BulletPhysicsEngine.createIntegrationMessages physicsEngine
            let integrationMessages = SArray.ofSeq physicsEngine.IntegrationMessages
            physicsEngine.IntegrationMessages.Clear ()
            integrationMessages

        member physicsEngine.CleanUp () =
            BulletPhysicsEngine.cleanUp physicsEngine