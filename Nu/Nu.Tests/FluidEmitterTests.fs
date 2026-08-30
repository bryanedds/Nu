// Nu Game Engine.
// Required Notice:
// Copyright (C) Bryan Edds.
// Nu Game Engine is licensed under the Nu Game Engine Noncommercial License.
// See https://github.com/bryanedds/Nu/blob/master/License.md.

namespace Nu.Tests
open System.Numerics
open NUnit.Framework
open Prime
open Nu

module FluidEmitterTests =

    let private particles =
        SArray.singleton
            { FluidParticlePosition = Vector3 (-50.0f, -50.0f, 0.0f)
              FluidParticleVelocity = Vector3.Zero
              FluidParticleConfig = "Water" }

    let private integratesFluidEmitterMessage messagesEnabled =
        Nu.init ()
        let physicsEngine = Box2dNetPhysicsEngine.make Vector3.Zero
        let fluidEmitterId = { FluidEmitterSource = Unchecked.defaultof<Simulant> }
        let descriptor =
            { Box2dNetFluidEmitterDescriptor.defaultDescriptor with
                MessagesEnabled = messagesEnabled }
        try
            physicsEngine.HandleMessage
                (CreateFluidEmitterMessage
                    { FluidEmitterId = fluidEmitterId
                      FluidParticles = particles
                      FluidEmitterDescriptor = Box2dNetFluidEmitterDescriptor descriptor })
            let result = physicsEngine.TryIntegrate (GameTime.ofUpdates 1L)
            match result with
            | Some messages ->
                messages
                |> Seq.exists (function
                    | FluidEmitterMessage message -> message.FluidParticles.Length > 0
                    | _ -> false)
            | None -> false
        finally
            physicsEngine.CleanUp ()

    let private integratesFluidEmitterMessagesAfterReenable () =
        Nu.init ()
        let physicsEngine = Box2dNetPhysicsEngine.make Vector3.Zero
        let fluidEmitterId = { FluidEmitterSource = Unchecked.defaultof<Simulant> }
        let descriptor = Box2dNetFluidEmitterDescriptor.defaultDescriptor
        let sendDescriptor descriptor =
            physicsEngine.HandleMessage
                (UpdateFluidEmitterMessage
                    { FluidEmitterId = fluidEmitterId
                      FluidEmitterDescriptor = Box2dNetFluidEmitterDescriptor descriptor })
        try
            physicsEngine.HandleMessage
                (CreateFluidEmitterMessage
                    { FluidEmitterId = fluidEmitterId
                      FluidParticles = particles
                      FluidEmitterDescriptor = Box2dNetFluidEmitterDescriptor descriptor })
            let integrate () =
                let result = physicsEngine.TryIntegrate (GameTime.ofUpdates 1L)
                match result with
                | Some messages ->
                    messages
                    |> Seq.exists (function
                        | FluidEmitterMessage message -> message.FluidParticles.Length > 0
                        | _ -> false)
                | None -> false
            let initiallyEnabled = integrate ()
            sendDescriptor { descriptor with MessagesEnabled = false }
            let disabled = integrate ()
            sendDescriptor { descriptor with MessagesEnabled = true }
            let reenabled = integrate ()
            (initiallyEnabled, disabled, reenabled)
        finally
            physicsEngine.CleanUp ()

    let private removesOutOfBoundsParticleWhenMessagesDisabled () =
        Nu.init ()
        let physicsEngine = Box2dNetPhysicsEngine.make Vector3.Zero
        let fluidEmitterId = { FluidEmitterSource = Unchecked.defaultof<Simulant> }
        let descriptor =
            { Box2dNetFluidEmitterDescriptor.defaultDescriptor with
                MessagesEnabled = false }
        let outOfBoundsParticles =
            SArray.singleton
                { FluidParticlePosition = Vector3 (100.0f, 100.0f, 0.0f)
                  FluidParticleVelocity = Vector3.Zero
                  FluidParticleConfig = "Water" }
        let enabledDescriptor = { descriptor with MessagesEnabled = true }
        try
            physicsEngine.HandleMessage
                (CreateFluidEmitterMessage
                    { FluidEmitterId = fluidEmitterId
                      FluidParticles = outOfBoundsParticles
                      FluidEmitterDescriptor = Box2dNetFluidEmitterDescriptor descriptor })
            let disabledResult = physicsEngine.TryIntegrate (GameTime.ofUpdates 1L)
            physicsEngine.HandleMessage
                (UpdateFluidEmitterMessage
                    { FluidEmitterId = fluidEmitterId
                      FluidEmitterDescriptor = Box2dNetFluidEmitterDescriptor enabledDescriptor })
            let enabledResult = physicsEngine.TryIntegrate (GameTime.ofUpdates 1L)
            let disabledHasNoFluidMessage =
                match disabledResult with
                | Some messages ->
                    messages
                    |> Seq.forall (function
                        | FluidEmitterMessage _ -> false
                        | _ -> true)
                | None -> true
            let enabledShowsParticleWasRemoved =
                match enabledResult with
                | Some messages ->
                    messages
                    |> Seq.exists (function
                        | FluidEmitterMessage message ->
                            message.FluidParticles.Length = 0 && message.OutOfBoundsParticles.Length = 0
                        | _ -> false)
                | None -> false
            disabledHasNoFluidMessage && enabledShowsParticleWasRemoved
        finally
            physicsEngine.CleanUp ()

    let [<Test>] ``Fluid emitter messages are enabled by default.`` () =
        Assert.That (Box2dNetFluidEmitterDescriptor.defaultDescriptor.MessagesEnabled, Is.True)

    let [<Test>] ``Fluid emitter message setting can be disabled and re-enabled.`` () =
        let descriptor = Box2dNetFluidEmitterDescriptor.defaultDescriptor
        let disabledDescriptor = { descriptor with MessagesEnabled = false }
        let reenabledDescriptor = { disabledDescriptor with MessagesEnabled = true }
        Assert.That (disabledDescriptor.MessagesEnabled, Is.False)
        Assert.That (reenabledDescriptor.MessagesEnabled, Is.True)

    let [<Test>] ``Enabled fluid emitter produces integration messages.`` () =
        Assert.That (integratesFluidEmitterMessage true, Is.True)

    let [<Test>] ``Disabled fluid emitter suppresses integration messages.`` () =
        Assert.That (integratesFluidEmitterMessage false, Is.False)

    let [<Test>] ``Re-enabled fluid emitter resumes integration messages.`` () =
        let initiallyEnabled, disabled, reenabled = integratesFluidEmitterMessagesAfterReenable ()
        Assert.That (initiallyEnabled, Is.True)
        Assert.That (disabled, Is.False)
        Assert.That (reenabled, Is.True)

    let [<Test>] ``Disabled fluid emitter removes out-of-bounds particles without a message.`` () =
        Assert.That (removesOutOfBoundsParticleWhenMessagesDisabled (), Is.True)
