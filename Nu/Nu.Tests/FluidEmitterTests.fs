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

    let private makeParticles () =
        SArray.ofList
            [ { FluidParticlePosition = Vector3 (0.0f, 0.0f, 0.0f)
                FluidParticleVelocity = Vector3.Zero
                FluidParticleConfig = "Water" }
              { FluidParticlePosition = Vector3 (10.0f, 0.0f, 0.0f)
                FluidParticleVelocity = Vector3.Zero
                FluidParticleConfig = "Water" } ]

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
