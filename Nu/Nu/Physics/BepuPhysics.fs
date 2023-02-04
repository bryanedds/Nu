// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu
open System
open System.Collections.Generic
open BepuPhysics
open BepuUtilities
open BepuUtilities.Memory
open Prime
open Nu

/// The BepuPhysics 3d implementation of PhysicsEngine.
type [<ReferenceEquality>] BepuPhysicsEngine =
    private
        { PhysicsContext : Simulation
          PhysicsMessages : PhysicsMessage UList
          IntegrationMessages : IntegrationMessage List
          ThreadDispatcher : ThreadDispatcher
          ContactEvents : ContactEvents
          NarrowPhaseCallbacks : NarrowPhaseCallbacks
          PoseIntegratorCallbacks : PoseIntegratorCallbacks
          EventHandler : EventHandler
          mutable RebuildingHack : bool }

    static member make () =
    
        //Generally, shoving as many threads as possible into the simulation won't produce the best results on systems with multiple logical cores per physical core.
        //Environment.ProcessorCount reports logical core count only, so we'll use a simple heuristic here- it'll leave one or two logical cores idle.
        //For the common Intel quad core with hyperthreading, this'll use six logical cores and leave two logical cores free to be used for other stuff.
        //This is by no means perfect. To maximize performance, you'll need to profile your simulation and target hardware.
        //Note that issues can be magnified on older operating systems like Windows 7 if all logical cores are given work.

        //Generally, the more memory bandwidth you have relative to CPU compute throughput, and the more collision detection heavy the simulation is relative to solving,
        //the more benefit you get out of SMT/hyperthreading. 
        //For example, if you're using the 64 core quad memory channel AMD 3990x on a scene composed of thousands of ragdolls, 
        //there won't be enough memory bandwidth to even feed half the physical cores. Using all 128 logical cores would just add overhead.

        //It may be worth using something like hwloc or CPUID to extract extra information to reason about.
        let targetThreadCount = max 1 (Environment.ProcessorCount - 2)
        let threadDispatcher = new ThreadDispatcher (targetThreadCount)
        let bufferPool = new BufferPool ()
        let contactEvents = ContactEvents (threadDispatcher, bufferPool)
        let narrowPhaseCallbacks = NarrowPhaseCallbacks ()
        let poseIntegratorCallbacks = PoseIntegratorCallbacks Constants.Engine.GravityDefault
        let solveDescription = new SolveDescription (8, 1)
        let simulation = Simulation.Create (bufferPool, narrowPhaseCallbacks, poseIntegratorCallbacks, solveDescription)
        let eventHandler = new EventHandler (simulation, bufferPool)

        //let box = new Box (1.0f, 2.0f, 3.0f)
        //let convexDyanmic = BodyDescription.CreateConvexDynamic(new Vector3(0.0f, 5.0f, 0.0f), 1.0f, Simulation.Shapes, &box)
        //let listenedBody1 = simulation.Bodies.Add &convexDyanmic
        //events.Register (simulation.Bodies[listenedBody1].CollidableReference, eventHandler)

        { PhysicsContext = simulation
          PhysicsMessages = UList.makeEmpty Imperative
          IntegrationMessages = List ()
          ThreadDispatcher = threadDispatcher
          ContactEvents = contactEvents
          NarrowPhaseCallbacks = narrowPhaseCallbacks
          PoseIntegratorCallbacks = poseIntegratorCallbacks
          EventHandler = eventHandler
          RebuildingHack = false }

    static member private integrate stepTime physicsEngine =

        let physicsStepAmount =
            match (Constants.Engine.DesiredFrameRate, stepTime) with
            | (StaticFrameRate frameRate, UpdateTime frames) -> 1.0f / single frameRate * single frames |> min 0.5f
            | (DynamicFrameRate _, ClockTime secs) -> secs |> min 0.5f
            | (_, _) -> failwithumf ()

        //Note that taking steps of variable length can reduce stability.
        if physicsStepAmount > 0.0f then
            physicsEngine.PhysicsContext.Timestep (physicsStepAmount, physicsEngine.ThreadDispatcher)

        // flush contact events
        physicsEngine.ContactEvents.Flush ()