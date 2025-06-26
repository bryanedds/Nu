// Nu Game Engine.
// Copyright (C) Bryan Edds.

namespace Nu
open System
open System.Numerics
open Prime

[<AutoOpen>]
module WorldPhysics =

    type World with

        static member internal getPhysicsEngine2d (world : World) =
            world.Subsystems.PhysicsEngine2d

        static member internal getPhysicsEngine3d (world : World) =
            world.Subsystems.PhysicsEngine3d

        static member internal getRendererPhysics3d (world : World) =
            world.Subsystems.RendererPhysics3d

        /// Localize a primitive body shape to a specific size; non-primitive body shapes are unaffected.
        static member localizePrimitiveBodyShape (size : Vector3) (bodyShape : BodyShape) =
            Physics.localizePrimitiveBodyShape size bodyShape

        /// Handle a 2d physics message in the world.
        static member handlePhysicsMessage2d (message : PhysicsMessage) world =
            match message with
            | CreateBodyMessage message ->
                let eventTrace = EventTrace.debug "World" "handlePhysicsMessage2d" "CreateBodyMessage" EventTrace.empty
                World.publishPlus message.BodyId Game.Handle.BodyAddingEvent eventTrace Game.Handle false false world
            | CreateBodiesMessage message ->
                let eventTrace = EventTrace.debug "World" "handlePhysicsMessage2d" "CreateBodiesMessage" EventTrace.empty
                for bodyProperties in message.BodiesProperties do
                    let bodyId = { BodySource = message.BodySource; BodyIndex = bodyProperties.BodyIndex }
                    World.publishPlus bodyId Game.Handle.BodyAddingEvent eventTrace Game.Handle false false world
            | DestroyBodyMessage message ->
                let eventTrace = EventTrace.debug "World" "handlePhysicsMessage2d" "DestroyBodyMessage" EventTrace.empty
                World.publishPlus { BodyId = message.BodyId } Game.Handle.BodySeparationImplicitEvent eventTrace Game.Handle false false world
                World.publishPlus message.BodyId Game.Handle.BodyRemovingEvent eventTrace Game.Handle false false world
            | DestroyBodiesMessage message ->
                let eventTrace = EventTrace.debug "World" "handlePhysicsMessage2d" "DestroyBodiesMessage" EventTrace.empty
                for bodyId in message.BodyIds do
                    World.publishPlus { BodyId = bodyId } Game.Handle.BodySeparationImplicitEvent eventTrace Game.Handle false false world
                    World.publishPlus bodyId Game.Handle.BodyRemovingEvent eventTrace Game.Handle false false world
            | SetBodyEnabledMessage message ->
                if not message.Enabled then
                    let eventTrace = EventTrace.debug "World" "handlePhysicsMessage2d" "SetBodyEnabledMessage" EventTrace.empty
                    World.publishPlus { BodyId = message.BodyId } Game.Handle.BodySeparationImplicitEvent eventTrace Game.Handle false false world
            | _ -> ()
            (World.getPhysicsEngine2d world).HandleMessage message

        /// Send multiple 2d physics messages to the world.
        static member handlePhysicsMessages2d (messages : PhysicsMessage seq) world =
            for message in messages do
                World.handlePhysicsMessage2d message world

        /// Send a 3d physics message in the world.
        static member handlePhysicsMessage3d (message : PhysicsMessage) world =
            match message with
            | CreateBodyMessage message ->
                let eventTrace = EventTrace.debug "World" "handlePhysicsMessage3d" "CreateBodyMessage" EventTrace.empty
                World.publishPlus message.BodyId Game.Handle.BodyAddingEvent eventTrace Game.Handle false false world
            | CreateBodiesMessage message ->
                let eventTrace = EventTrace.debug "World" "handlePhysicsMessage3d" "CreateBodiesMessage" EventTrace.empty
                for bodyProperties in message.BodiesProperties do
                    let bodyId = { BodySource = message.BodySource; BodyIndex = bodyProperties.BodyIndex }
                    World.publishPlus bodyId Game.Handle.BodyAddingEvent eventTrace Game.Handle false false world
            | DestroyBodyMessage message ->
                let eventTrace = EventTrace.debug "World" "handlePhysicsMessage3d" "DestroyBodyMessage" EventTrace.empty
                World.publishPlus { BodyId = message.BodyId } Game.Handle.BodySeparationImplicitEvent eventTrace Game.Handle false false world
                World.publishPlus message.BodyId Game.Handle.BodyRemovingEvent eventTrace Game.Handle false false world
            | DestroyBodiesMessage message ->
                let eventTrace = EventTrace.debug "World" "handlePhysicsMessage3d" "DestroyBodiesMessage" EventTrace.empty
                for bodyId in message.BodyIds do
                    World.publishPlus { BodyId = bodyId } Game.Handle.BodySeparationImplicitEvent eventTrace Game.Handle false false world
                    World.publishPlus bodyId Game.Handle.BodyRemovingEvent eventTrace Game.Handle false false world
            | SetBodyEnabledMessage message ->
                if not message.Enabled then
                    match message.BodyId.BodySource with
                    | :? Entity ->
                        let eventTrace = EventTrace.debug "World" "handlePhysicsMessage3d" "SetBodyEnabledMessage" EventTrace.empty
                        World.publishPlus { BodyId = message.BodyId } Game.Handle.BodySeparationImplicitEvent eventTrace Game.Handle false false world
                    | _ -> ()
            | _ -> ()
            (World.getPhysicsEngine3d world).HandleMessage message

        /// Send multiple 3d physics messages to the world.
        static member handlePhysicsMessages3d (messages : PhysicsMessage seq) world =
            for message in messages do
                World.handlePhysicsMessage3d message world

        /// Check that the world contains a body with the given body id.
        static member getBodyExists bodyId (world : World) =
            world.Subsystems.PhysicsEngine3d.GetBodyExists bodyId ||
            world.Subsystems.PhysicsEngine2d.GetBodyExists bodyId

        /// Get the contact normals of the body with the given body id.
        static member getBodyContactNormals bodyId (world : World) =
            if world.Subsystems.PhysicsEngine3d.GetBodyExists bodyId then
                world.Subsystems.PhysicsEngine3d.GetBodyContactNormals bodyId
            elif world.Subsystems.PhysicsEngine2d.GetBodyExists bodyId then
                world.Subsystems.PhysicsEngine2d.GetBodyContactNormals bodyId
            else
                Log.info ("Body for '" + scstring bodyId + "' not found.")
                [||]

        /// Get the linear velocity of the body with the given body id.
        static member getBodyLinearVelocity bodyId (world : World) =
            if world.Subsystems.PhysicsEngine3d.GetBodyExists bodyId then
                world.Subsystems.PhysicsEngine3d.GetBodyLinearVelocity bodyId
            elif world.Subsystems.PhysicsEngine2d.GetBodyExists bodyId then
                world.Subsystems.PhysicsEngine2d.GetBodyLinearVelocity bodyId
            else
                Log.info ("Body for '" + scstring bodyId + "' not found.")
                v3Zero

        /// Get the angular velocity of the body with the given body id.
        static member getBodyAngularVelocity bodyId (world : World) =
            if world.Subsystems.PhysicsEngine3d.GetBodyExists bodyId then
                world.Subsystems.PhysicsEngine3d.GetBodyAngularVelocity bodyId
            elif world.Subsystems.PhysicsEngine2d.GetBodyExists bodyId then
                world.Subsystems.PhysicsEngine2d.GetBodyAngularVelocity bodyId
            else
                Log.info ("Body for '" + scstring bodyId + "' not found.")
                v3Zero

        /// Get the contact normals where the body with the given body id is touching the ground.
        static member getBodyToGroundContactNormals bodyId (world : World) =
            if world.Subsystems.PhysicsEngine3d.GetBodyExists bodyId then
                world.Subsystems.PhysicsEngine3d.GetBodyToGroundContactNormals bodyId
            elif world.Subsystems.PhysicsEngine2d.GetBodyExists bodyId then
                world.Subsystems.PhysicsEngine2d.GetBodyToGroundContactNormals bodyId
            else
                Log.info ("Body for '" + scstring bodyId + "' not found.")
                [||]

        /// Get a contact normal where the body with the given body id is touching the ground (if one exists).
        static member getBodyToGroundContactNormalOpt bodyId (world : World) =
            if world.Subsystems.PhysicsEngine3d.GetBodyExists bodyId then
                world.Subsystems.PhysicsEngine3d.GetBodyToGroundContactNormalOpt bodyId
            elif world.Subsystems.PhysicsEngine2d.GetBodyExists bodyId then
                world.Subsystems.PhysicsEngine2d.GetBodyToGroundContactNormalOpt bodyId
            else
                Log.info ("Body for '" + scstring bodyId + "' not found.")
                None

        /// Get a contact tangent where the body with the given body id is touching the ground (if one exists).
        static member getBodyToGroundContactTangentOpt bodyId (world : World) =
            if world.Subsystems.PhysicsEngine3d.GetBodyExists bodyId then
                world.Subsystems.PhysicsEngine3d.GetBodyToGroundContactTangentOpt bodyId
            elif world.Subsystems.PhysicsEngine2d.GetBodyExists bodyId then
                world.Subsystems.PhysicsEngine2d.GetBodyToGroundContactTangentOpt bodyId
            else
                Log.info ("Body for '" + scstring bodyId + "' not found.")
                None

        /// Check that the body with the given body id is on the ground.
        static member getBodyGrounded bodyId (world : World) =
            if world.Subsystems.PhysicsEngine3d.GetBodyExists bodyId then
                world.Subsystems.PhysicsEngine3d.GetBodyGrounded bodyId
            elif world.Subsystems.PhysicsEngine2d.GetBodyExists bodyId then
                world.Subsystems.PhysicsEngine2d.GetBodyGrounded bodyId
            else
                Log.info ("Body for '" + scstring bodyId + "' not found.")
                false

        /// Check that the body with the given body id is a sensor.
        static member getBodySensor bodyId (world : World) =
            if world.Subsystems.PhysicsEngine3d.GetBodyExists bodyId then
                world.Subsystems.PhysicsEngine3d.GetBodySensor bodyId
            elif world.Subsystems.PhysicsEngine2d.GetBodyExists bodyId then
                world.Subsystems.PhysicsEngine2d.GetBodySensor bodyId
            else
                Log.info ("Body for '" + scstring bodyId + "' not found.")
                false

        /// Ray cast against 3d physics bodies.
        static member rayCast3dBodies ray collisionMask closestOnly (world : World) =
            world.Subsystems.PhysicsEngine3d.RayCast (ray, collisionMask, closestOnly)

        /// Ray cast against 2d physics bodies.
        static member rayCast2dBodies ray collisionMask closestOnly (world : World) =
            world.Subsystems.PhysicsEngine2d.RayCast (ray, collisionMask, closestOnly)

        /// Send a physics message to create a physics body.
        static member createBody is2d bodyId (bodyProperties : BodyProperties) world =
            let createBodyMessage = CreateBodyMessage { BodyId = bodyId; BodyProperties = bodyProperties }
            if not is2d
            then World.handlePhysicsMessage3d createBodyMessage world
            else World.handlePhysicsMessage2d createBodyMessage world

        /// Send a physics message to create several physics bodies.
        static member createBodies is2d bodySource bodiesProperties world =
            let createBodiesMessage = CreateBodiesMessage { BodySource = bodySource; BodiesProperties = bodiesProperties }
            if not is2d
            then World.handlePhysicsMessage3d createBodiesMessage world
            else World.handlePhysicsMessage2d createBodiesMessage world

        /// Send a physics message to destroy a physics body.
        static member destroyBody is2d bodyId world =
            let destroyBodyMessage = DestroyBodyMessage { BodyId = bodyId }
            if not is2d
            then World.handlePhysicsMessage3d destroyBodyMessage world
            else World.handlePhysicsMessage2d destroyBodyMessage world

        /// Send a physics message to destroy several physics bodies.
        static member destroyBodies is2d bodyIds world =
            let destroyBodiesMessage = DestroyBodiesMessage { BodyIds = bodyIds }
            if not is2d
            then World.handlePhysicsMessage3d destroyBodiesMessage world
            else World.handlePhysicsMessage2d destroyBodiesMessage world

        /// Send a physics message to create a physics joint.
        static member createBodyJoint is2d bodyJointSource bodyJointProperties world =
            let createBodyJointMessage = CreateBodyJointMessage { BodyJointSource = bodyJointSource; BodyJointProperties = bodyJointProperties }
            if not is2d
            then World.handlePhysicsMessage3d createBodyJointMessage world
            else World.handlePhysicsMessage2d createBodyJointMessage world

        /// Send a physics message to destroy a physics joint.
        static member destroyBodyJoint is2d bodyJointTarget bodyJointTarget2Opt bodyJointId world =
            let destroyBodyJointMessage = DestroyBodyJointMessage { BodyJointId = bodyJointId; BodyJointTarget = bodyJointTarget; BodyJointTarget2Opt = bodyJointTarget2Opt }
            if not is2d
            then World.handlePhysicsMessage3d destroyBodyJointMessage world
            else World.handlePhysicsMessage2d destroyBodyJointMessage world

        /// Send a physics message to set the enabled-ness of a body with the given body id.
        static member setBodyEnabled enabled bodyId world =
            let setBodyEnabledMessage = SetBodyEnabledMessage { BodyId = bodyId; Enabled = enabled }
            World.handlePhysicsMessage3d setBodyEnabledMessage world
            World.handlePhysicsMessage2d setBodyEnabledMessage world

        /// Send a physics message to set the position of a body with the given body id.
        static member setBodyCenter center bodyId world =
            let setBodyCenterMessage = SetBodyCenterMessage { BodyId = bodyId; Center = center }
            World.handlePhysicsMessage3d setBodyCenterMessage world
            World.handlePhysicsMessage2d setBodyCenterMessage world

        /// Send a physics message to set the rotation of a body with the given body id.
        static member setBodyRotation rotation bodyId world =
            let setBodyRotationMessage = SetBodyRotationMessage { BodyId = bodyId; Rotation = rotation }
            World.handlePhysicsMessage3d setBodyRotationMessage world
            World.handlePhysicsMessage2d setBodyRotationMessage world

        /// Send a physics message to set the linear velocity of a body with the given body id.
        static member setBodyLinearVelocity linearVelocity bodyId world =
            let setBodyLinearVelocityMessage = SetBodyLinearVelocityMessage { BodyId = bodyId; LinearVelocity = linearVelocity }
            World.handlePhysicsMessage3d setBodyLinearVelocityMessage world
            World.handlePhysicsMessage2d setBodyLinearVelocityMessage world

        /// Send a physics message to set the angular velocity of a body with the given body id.
        static member setBodyAngularVelocity angularVelocity bodyId world =
            let setBodyAngularVelocityMessage = SetBodyAngularVelocityMessage { BodyId = bodyId; AngularVelocity = angularVelocity }
            World.handlePhysicsMessage3d setBodyAngularVelocityMessage world
            World.handlePhysicsMessage2d setBodyAngularVelocityMessage world

        /// Send a physics message to set the forward input of a vehicle body with the given body id.
        static member setBodyVehicleForwardInput forwardInput bodyId world =
            let setBodyVehicleForwardInputMessage = SetBodyVehicleForwardInputMessage { BodyId = bodyId; ForwardInput = forwardInput }
            World.handlePhysicsMessage3d setBodyVehicleForwardInputMessage world
            World.handlePhysicsMessage2d setBodyVehicleForwardInputMessage world

        /// Send a physics message to set the right input of a vehicle body with the given body id.
        static member setBodyVehicleRightInput rightInput bodyId world =
            let setBodyVehicleRightInputMessage = SetBodyVehicleRightInputMessage { BodyId = bodyId; RightInput = rightInput }
            World.handlePhysicsMessage3d setBodyVehicleRightInputMessage world
            World.handlePhysicsMessage2d setBodyVehicleRightInputMessage world

        /// Send a physics message to set the brake input of a vehicle body with the given body id.
        static member setBodyVehicleBrakeInput brakeInput bodyId world =
            let setBodyVehicleBrakeInputMessage = SetBodyVehicleBrakeInputMessage { BodyId = bodyId; BrakeInput = brakeInput }
            World.handlePhysicsMessage3d setBodyVehicleBrakeInputMessage world
            World.handlePhysicsMessage2d setBodyVehicleBrakeInputMessage world

        /// Send a physics message to set the hand brake input of a vehicle body with the given body id.
        static member setBodyVehicleHandBrakeInput handBrakeInput bodyId world =
            let setBodyVehicleHandBrakeInputMessage = SetBodyVehicleHandBrakeInputMessage { BodyId = bodyId; HandBrakeInput = handBrakeInput }
            World.handlePhysicsMessage3d setBodyVehicleHandBrakeInputMessage world
            World.handlePhysicsMessage2d setBodyVehicleHandBrakeInputMessage world

        /// Send a physics message to apply linear impulse to a body with the given body id.
        static member applyBodyLinearImpulse linearImpulse originWorldOpt bodyId world =
            let applyBodyLinearImpulseMessage = ApplyBodyLinearImpulseMessage { BodyId = bodyId; LinearImpulse = linearImpulse; OriginWorldOpt = originWorldOpt }
            World.handlePhysicsMessage3d applyBodyLinearImpulseMessage world
            World.handlePhysicsMessage2d applyBodyLinearImpulseMessage world

        /// Send a physics message to apply angular impulse to a body with the given body id.
        static member applyBodyAngularImpulse angularImpulse bodyId world =
            let applyBodyAngularImpulseMessage = ApplyBodyAngularImpulseMessage { BodyId = bodyId; AngularImpulse = angularImpulse }
            World.handlePhysicsMessage3d applyBodyAngularImpulseMessage world
            World.handlePhysicsMessage2d applyBodyAngularImpulseMessage world

        /// Send a physics message to apply force to a body with the given body id.
        static member applyBodyForce force originWorldOpt bodyId world =
            let applyBodyForceMessage = ApplyBodyForceMessage { BodyId = bodyId; Force = force; OriginWorldOpt = originWorldOpt }
            World.handlePhysicsMessage3d applyBodyForceMessage world
            World.handlePhysicsMessage2d applyBodyForceMessage world

        /// Send a physics message to apply torque to a body with the given body id.
        static member applyBodyTorque torque bodyId world =
            let applyBodyTorqueMessage = ApplyBodyTorqueMessage { BodyId = bodyId; Torque = torque }
            World.handlePhysicsMessage3d applyBodyTorqueMessage world
            World.handlePhysicsMessage2d applyBodyTorqueMessage world

        /// Send a physics message to jump to a body with the given body id (KinematicCharacter only).
        static member jumpBody canJumpInAir jumpSpeed bodyId world =
            let jumpBodyMessage = JumpBodyMessage { BodyId = bodyId; CanJumpInAir = canJumpInAir; JumpSpeed = jumpSpeed }
            World.handlePhysicsMessage3d jumpBodyMessage world
            World.handlePhysicsMessage2d jumpBodyMessage world

        /// Reregister all currently selected 3d physics.
        static member reregisterPhysics world =
            match World.getSelectedScreenOpt world with
            | Some selectedScreen ->
                WorldModule.unregisterScreenPhysics selectedScreen world
                WorldModule.registerScreenPhysics selectedScreen world
            | None -> ()

        /// Reload all currently selected physics assets.
        static member reloadPhysicsAssets world =
            World.reregisterPhysics world