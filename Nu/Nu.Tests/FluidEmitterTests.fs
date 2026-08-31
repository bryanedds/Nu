// Nu Game Engine.
// Required Notice:
// Copyright (C) Bryan Edds.
// Nu Game Engine is licensed under the Nu Game Engine Noncommercial License.
// See https://github.com/bryanedds/Nu/blob/master/License.md.

namespace Nu.Tests
open System
open System.Numerics
open NUnit.Framework
open Prime
open Nu
module FluidEmitterTests =

    // NOTE: we test Box2dNetPhysicsEngine directly instead of via the World API because only Box2DNet is going to
    // support 2D particles physics for the foreseeable future.

    let private particles =
        SArray.singleton
            { FluidParticlePosition = Vector3 (-50.0f, -50.0f, 0.0f)
              FluidParticleVelocity = Vector3.Zero
              FluidParticleConfig = "Water" }

    let private makeParticles () =
        SArray.ofList
            [{ FluidParticlePosition = v3Zero
               FluidParticleVelocity = v3Zero
               FluidParticleConfig = "Water" }
             { FluidParticlePosition = v3 10.0f 0.0f 0.0f
               FluidParticleVelocity = v3Zero
               FluidParticleConfig = "Water" }]

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

    let [<Test>] ``Destroying a fluid emitter destroys its particle bodies.`` () =

        let physicsEngine = Box2dNetPhysicsEngine.make Vector3.Zero
        try
            let emitterSource = { GsgAddress = atoa (stoa "FluidEmitterTests/Emitter") } :> Simulant
            let emitterId = { FluidEmitterSource = emitterSource }
            let particles = makeParticles ()
            let descriptor = Box2dNetFluidEmitterDescriptor Box2dNetFluidEmitterDescriptor.defaultDescriptor
            let createMessage =
                { FluidEmitterId = emitterId
                  FluidParticles = particles
                  FluidEmitterDescriptor = descriptor }
            let particleBody bodyIndex = { BodySource = emitterSource; BodyIndex = bodyIndex }
            let particleRay = Ray3 (Vector3 (-20.0f, 0.0f, 0.0f), Vector3 (40.0f, 0.0f, 0.0f))
            let rayCast () = physicsEngine.RayCast (particleRay, UInt64.MaxValue, UInt64.MaxValue, false)

            physicsEngine.HandleMessage (CreateFluidEmitterMessage createMessage)
            Assert.That (physicsEngine.GetFluidEmitterExists emitterId, Is.True)
            let particleIntersections = rayCast ()
            let actualParticleBodies = particleIntersections |> Array.map _.BodyShapeIntersected.BodyId |> Set.ofArray
            let expectedParticleBodies = Set.ofList [particleBody 0; particleBody 1]
            Assert.That (particleIntersections.Length, Is.EqualTo 2)
            Assert.That (actualParticleBodies, Is.EqualTo expectedParticleBodies)

            physicsEngine.HandleMessage (DestroyFluidEmitterMessage { FluidEmitterId = emitterId })
            physicsEngine.HandleMessage (DestroyFluidEmitterMessage { FluidEmitterId = emitterId })

            Assert.That (physicsEngine.GetFluidEmitterExists emitterId, Is.False)
            Assert.That (rayCast (), Is.Empty)

        finally
            physicsEngine.CleanUp ()

    let [<Test>] ``Resizing a fluid emitter destroys replaced particle bodies.`` () =

        let physicsEngine = Box2dNetPhysicsEngine.make Vector3.Zero
        try
            let emitterSource = { GsgAddress = atoa (stoa "FluidEmitterTests/ResizedEmitter") } :> Simulant
            let emitterId = { FluidEmitterSource = emitterSource }
            let particles = makeParticles ()
            let boxDescriptor = Box2dNetFluidEmitterDescriptor.defaultDescriptor
            let descriptor = Box2dNetFluidEmitterDescriptor boxDescriptor
            let createMessage =
                { FluidEmitterId = emitterId
                  FluidParticles = particles
                  FluidEmitterDescriptor = descriptor }
            let particleRay = Ray3 (Vector3 (-20.0f, 0.0f, 0.0f), Vector3 (40.0f, 0.0f, 0.0f))
            let rayCast () = physicsEngine.RayCast (particleRay, UInt64.MaxValue, UInt64.MaxValue, false)
            let resizedDescriptor =
                Box2dNetFluidEmitterDescriptor
                    { boxDescriptor with ParticlesMax = boxDescriptor.ParticlesMax + 1 }

            physicsEngine.HandleMessage (CreateFluidEmitterMessage createMessage)
            Assert.That ((rayCast ()).Length, Is.EqualTo 2)
            physicsEngine.HandleMessage
                (UpdateFluidEmitterMessage
                    { FluidEmitterId = emitterId
                      FluidEmitterDescriptor = resizedDescriptor })

            Assert.That ((rayCast ()).Length, Is.EqualTo 2)
            physicsEngine.HandleMessage (DestroyFluidEmitterMessage { FluidEmitterId = emitterId })
            Assert.That (rayCast (), Is.Empty)

        finally
            physicsEngine.CleanUp ()