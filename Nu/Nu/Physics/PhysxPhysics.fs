// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu
open Nu

// 3d physics with PhysX.NET.

namespace Nu.Analysis.Physx
open System
open System.Numerics

[<AutoOpen>]
module CoreModule =

    type Index = int
    type PxBounds3 = unit
    type Matrix3x3 = unit
    type Singleton = interface end
    type 'a HashSet = { __ :  unit }
    type PxTransform = { Quaternion : Quaternion; Position : Vector3 }
    type SpatialVector = { Linear : Vector3; Angular : Vector3 }
    type ForwardDeclaration = unit
    type Call =
        | Fn of string * Call list
        | Task of string
        | Handler of string
        | ForEachIn of string * Call
        | Do of Call list
    let Fn name calls = Fn (name, calls)
    let Task name = Task name
    let Handler name = Handler name
    let ForEachIn name call = ForEachIn (name, call)
    let Do calls = Do calls
    let ($) = (<|)

[<AutoOpen>]
module ShapeModule =

    type [<RequireQualifiedAccess>] PxShapeFlag =
        | eSIMULATION_SHAPE = 1
        | eSCENE_QUERY_SHAPE = 2
        | eTRIGGER_SHAPE = 4
        | eVISUALIZATION = 8

    type GeometryUnion =
        | PxBoxGeometry
        | PxSphereGeometry
        | PxCapsuleGeometry
        | PxPlaneGeometry
        | PxConvexMeshGeometryLL
        | PxTriangleMeshGeometryLL
        | PxHeightFieldGeometryLL

    type Shape =
        { Transform : PxTransform
          ContactOffset : single
          ShapeFlags : PxShapeFlag
          MaterialIndex : Index
          Geometry : GeometryUnion }

    type ShapeSim =
        { Shape : Shape
          ID : Index }

[<AutoOpen>]
module BodyModule =

    type Body =
        { Transform : PxTransform
          CcdAdvanceCoefficient : single
          LinearVelocity : Vector3
          MaxPenBias : Vector3
          AngularVelocity : Vector3
          AndMuchMore___ : unit }

    type RigidBody =
        { Body : Body }

    type BodySim =
        { RigidBody : RigidBody
          ID : Index}

[<AutoOpen>]
module ActorModule =

    type [<RequireQualifiedAccess>] PxActorType =
        | eRIGID_STATIC
        | eRIGID_DYNAMIC
        | eARTICULATION_LINK

    type [<RequireQualifiedAccess>] PxActorFlag =
        | eVISUALIZATION = 1
        | eDISABLE_GRAVITY = 2
        | eSEND_SLEEP_NOTIFIES = 4
        | eDISABLE_SIMULATION = 8

    type Actor =
        { ActorType : PxActorType
          ActorFlags : PxActorFlag }

    type ActorSim =
        { Actor : Actor
          OwnerClient : Index
          DominanceGroup : Index
          AggregateId : Index }

    /// A ElementSim is a part of a ActorSim. It contributes to the activation framework by adding its 
    /// interactions to the actor.
    type ElementSim =
        { Actor : ActorSim
          ElementID : Index }

[<AutoOpen>]
module InteractionModule =

    type ConstraintSim = ForwardDeclaration

    type [<RequireQualifiedAccess>] InteractionType =
        | eOVERLAP // corresponds to ShapeInteraction
        | eTRIGGER // corresponds to TriggerInteraction
        | eMARKER // corresponds to ElementInteractionMarker
        | eTRACKED_IN_SCENE_COUNT // not a real type, interactions above this limit are tracked in the scene
        | eCONSTRAINTSHADER // corresponds to ConstraintInteraction
        | eARTICULATION // corresponds to ArticulationJointSim

    type [<RequireQualifiedAccess>] InteractionFlag =
        | eRB_ELEMENT = 1 // Interactions between rigid body shapes
        | eCONSTRAINT = 2
        | eFILTERABLE = 4 // Interactions that go through the filter code
        | eIN_DIRTY_LIST = 8 // The interaction is in the dirty list
        | eIS_FILTER_PAIR = 16 // The interaction is tracked by the filter callback mechanism
        | eIS_ACTIVE = 32

    type [<RequireQualifiedAccess>] PxCombineMode =
        | eAVERAGE
        | eMIN
        | eMULTIPLY
        | eMAX

    type PxMaterial =
        { DynamicFriction : single
          StaticFriction : single
          Restitution : single
          FrictionCombineMode : PxCombineMode
          RestitutionCombineMode : PxCombineMode
          ConcreteTypeName : string }

    type PxsMaterialManager =
        { PxMaterial : PxMaterial array }

    type ElementInteractionMarker =
        | ElementInteractionMarker of ActorSim * ActorSim * ElementSim * ElementSim
    type ShapeInteraction =
        | ShapeInteraction of ActorSim * ActorSim * ElementSim * ElementSim
    type TriggerInteraction =
        | TriggerInteraction of ActorSim * ActorSim * ElementSim * ElementSim
    type ArticulationJointSim =
        | ArticularionJointSim of ActorSim * ActorSim * InteractionType * InteractionFlag
    type ConstraintInteraction =
        | ConstraintInteraction of ActorSim * ActorSim * ConstraintSim * Index
    type ElementSimInteraction =
        | ElementInteractionMarker of ElementInteractionMarker
        | ShapeInteraction of ShapeInteraction
        | TriggerInteraction of TriggerInteraction
    type Interaction =
        | ArticulationJointSim of ArticulationJointSim
        | ConstraintInteraction of ConstraintInteraction
        | ElementSimInteraction of ElementSimInteraction

[<AutoOpen>]
module ConstraintModule =

    type [<RequireQualifiedAccess>] ConstraintFlag =
        | ePENDING_GROUP_UPDATE = 1 // For constraint projection an island of the bodies connected by constraints is generated.
        | eBREAKABLE = 2 // The constraint can break
        | eCHECK_MAX_FORCE_EXCEEDED = 4 // This constraint will get tested for breakage at the end of the sim step
        | eBROKEN = 8

    type Constraint =
        { LinBreakForce : single
          AngBreakForce : single
          Index : Index //this is also a constraint write back index
          BodyCore1 : Body
          BodyCore2 : Body }

    type ArticulationLoopConstraint =
        { LinkIndex0 : Index
          LinkIndex1 : Index
          Constraint : Constraint }

    type ConstraintSim =
        { Bodies : BodySim * BodySim
          ConstraintInteraction : ConstraintInteraction
          ConstraintFlags : ConstraintFlag }

[<AutoOpen>]
module ArticulationModule =

    type ArticulationLink = unit

    type ArticulationV =
        { __ : unit }

    type Articulation =
        { InternalLoads : Matrix3x3 array
          ExternalLoads : Matrix3x3 array
          DeltaQuaternion : Quaternion array
          MotionVelocity : SpatialVector array
          Pose : PxTransform array }

    and ArticulationSim =
        { Articulation : Articulation
          ArticulationLinks : ArticulationLink array
          Bodies : BodySim array
          Joints : ArticulationJointSim array
          LoopConstraints : ArticulationLoopConstraint array
          LowLevelArticulation : ArticulationV
          IslandNodeIndex : Index }

[<AutoOpen>]
module ContactModule =

    type PxsContactManager =
        { RigidBody0 : RigidBody
          RigidBody1 : RigidBody
          ShapeInteraction : ShapeInteraction }

[<AutoOpen>]
module BroadPhaseModule =

    type [<RequireQualifiedAccess>] FilterGroup =
        | eSTATICS
        | eDYNAMICS_BASE
        | eAGGREGATE_BASE

    type BroadPhaseUpdateData =
        { ShapesCreated : Index array
          ShapesUpdated : Index array
          ShapesRemoved : Index array
          BoxBounds : PxBounds3 array
          BoxGroups : FilterGroup array
          BoxContactDistances : single array }

    type ABP = unit // 
    type MBP = unit // multi-box pruning
    type Sap = unit // sweep-and-prune
    type BroadPhase =
        | BroadPhaseABP of ABP
        | BroadPhaseMBP of MBP
        | BroadPhaseSap of Sap

    type Aggregate =
        { Bounds : PxBounds3
          AggregatedBounds : Index array
          BoundsIndex : Index }

    /// A structure responsible for:
    /// storing an aabb representation for each active shape in the related scene
    /// managing the creation/removal of aabb representations when their related shapes are created/removed
    /// updating all aabbs that require an update due to modification of shape geometry or transform
    /// updating the aabb of all aggregates from the union of the aabbs of all shapes that make up each aggregate
    /// computing and reporting the incremental changes to the set of overlapping aabb pairs
    type AABBManager =
        { Aggregates : Aggregate array }

[<AutoOpen>]
module IslandModule =

    type Island =
        { __ : unit }

    type IslandSim =
        { Islands : Island array }

    type SimpleIslandManager =
        { AccurateIslandManager : IslandSim
          SpeculativeIslandManager : IslandSim }

[<AutoOpen>]
module NarrowPhaseModule =

    type PxvSimStats =
        { __ : unit }

    type PxsContext =
        { SimStats : PxvSimStats }

    type PxsCCDContext =
        { __ : unit }

    type NPhaseCore =
        { __ : unit }

[<AutoOpen>]
module SceneModule =

    type [<RequireQualifiedAccess>] PxSceneFlag =
        | eENABLE_ACTIVE_ACTORS = 1
        | eENABLE_CCD = 2
        | eDISABLE_CCD_RESWEEP = 4
        | eADAPTIVE_FORCE = 8
        | eENABLE_PCM = 64
        | eDISABLE_CONTACT_REPORT_BUFFER_RESIZE = 128
        | eDISABLE_CONTACT_CACHE = 256
        | eREQUIRE_RW_LOCK = 512
        | eENABLE_STABILIZATION = 1024
        | eENABLE_AVERAGE_POINT = 2048
        | eEXCLUDE_KINEMATICS_FROM_ACTIVE_ACTORS = 4096
        | eENABLE_GPU_DYNAMICS = 8092
        | eENABLE_ENHANCED_DETERMINISM = 16184
        | eENABLE_FRICTION_EVERY_ITERATION = 32368
        | eMUTABLE_FLAGS = 4097 // PxSceneFlag.eENABLE_ACTIVE_ACTORS ||| eEXCLUDE_KINEMATICS_FROM_ACTIVE_ACTORS

    type NpScene =
        { Actors : ActorSim array
          Articulations : Articulation array
          SceneFlags : PxSceneFlag
          LowLevelContext : PxsContext }

    type Scene =

        { // unknown
          ActiveBodies : Body array
          ActiveCompoundBodies : Body array
          Interactions : Interaction array
          Articulations : ArticulationSim HashSet
          Constraints : ConstraintSim HashSet
          ActiveBreakableConstraints : ConstraintSim array
          BrokenConstraints : ConstraintSim HashSet
          PublicFlags : PxSceneFlag
          LowLevelContext : PxsContext

          // broad phase
          AABBManager : AABBManager
          BroadPhase : BroadPhase

          // narrow phase
          SimpleIslandManager : SimpleIslandManager
          FoundPatchManagers : PxsContactManager array
          LostPatchManagers : PxsContactManager array
          CCDContext : PxsCCDContext
          NPhaseCore : NPhaseCore }

        member this.Pipeline =

            [Fn "simulate"
                [Fn "prepareCollide"
                    [Fn "mBrokenConstraints.clear" []]
                 Fn "stepSetupCollide"
                    [Fn "mProjectionManager.processPendingUpdates" []
                     Fn "kinematicsSetup"
                        [ForEachIn "getActiveKinematicBodies" $ Task "SCKinematicUpdateTask"
                         Task "ScKinematicAddDynamicTask"]
                     Fn "mNPhaseCore.updateDirtyInteractions" []]]

             Fn "collideStep" []

             Fn "preRigidBodyNarrowPhase"
                [ForEachIn "mSpeculativeCCDRigidBodyBitMap" $ Task "SpeculativeCCDContactDistanceUpdateTask"
                 ForEachIn "mSpeculativeCCDArticulationBitMap" $ Task "SpeculativeCCDContactDistanceArticulationUpdateTask"
                 ForEachIn "mDirtyShapeSimMap" $ Task "SpeculativeCCDContactDistanceArticulationUpdateTask"]

             Fn "rigidBodyNarrowPhase"
                [Fn "mLLContext.resetThreadContexts" []
                 Fn "mLLContext.updateContactManager"
                    [Fn "mNpImplementationContext.updateContactManager"
                        [Fn "processContactManager"
                            [ForEachIn "mNarrowPhasePairs.mContactManagerMapping" $ Task "PxsCMDiscreteUpdateTask"]]]]

             Fn "broadPhase"
                [Fn "mAABBManager.updateAABBsAndBP"
                    [ForEachIn "mAddedHandleMap" $ Fn "mAddedHandles.pushBack" []
                     ForEachIn "mChangedHandleMap" $ Fn "mUpdatedHandles.pushBack" []
                     ForEachIn "mDirtyAggregates" $ Fn "mUpdatedHandles|?.pushBack" []
                     ForEachIn "mRemovedHandleMap" $ Fn "mRemovedHandles.pushBack" []
                     Fn "handleOriginShift" []]]

             Fn "postBroadPhase"
                [Fn "mAABBManager.postBroadPhase"
                    [Fn "processBPPairs"
                        [ForEachIn "mBroadPhase.mDeletedPairsArray" $ Handler "DeletedPairHandler"
                         ForEachIn "mDirtyAggregates" $ Task "SortAggregateBoundsParallel"
                         Fn "postBpStage2"
                            [ForEachIn "mDirtyAggregates" $ Task "ProcessSelfCollisionPairsParallel"
                             ForEachIn "mAggregateAggregatePairs" $ Task "ProcessAggPairsParallelTask"
                             ForEachIn "mActorAggregatePairs" $ Task "ProcessAggPairsParallelTask"]
                         Fn "postBpStage3"
                            [ForEachIn "mDirtyAggregates" $ Fn "item.resetDirtyState" []
                             ForEachIn "mAggPairTasks" $ Unchecked.defaultof<_>
                             ForEachIn "mBroadPhase.mCreatedPairsArray" $ Handler "CreatedPairHandler"
                             Fn "mCreatedPairs.clear" []
                             Unchecked.defaultof<_>
                             Fn "mAddedHandleMap.clear" []
                             Fn "mRemovedHandleMap.clear" []]]]]

             Fn "postBroadPhaseContinuation"
                [Fn "mAABBManager.mChangedHandleMap.clear" []
                 Fn "finishBroadPhase"
                    [ForEachIn "mAABBManager.mCreatedOverlaps" $ Fn "createRbElementInteraction" []
                     ForEachIn "mAABBManager.mCreatedOverlaps" $ Task "OverlapFilterTask"]]

             Fn "preallocateContactManagers"
                [ForEachIn "mOverlapFilterTaskHead" $ Unchecked.defaultof<_> []
                 ForEachIn "mOverlapFilterTaskHead" $ Task "OnOverlapCreatedTask"]

             Fn "postBroadPhaseStage2"
                [Fn "processLostTouchPairs"
                    [ForEachIn "mLostTouchPairs" $ Fn "internalWakeUp" []
                     Fn "mLostTouchPairs.clear" []
                     Fn "mLostTouchPairsDeletedBodyIDs.clear" []]
                 ForEachIn "mPreallocatedContactManagers" $ Fn "mLLContext.mContactManagerPool.put" []
                 ForEachIn "mPreallocatedShapeInteractions" $ Fn "mNPhaseCore.mShapeInteractionPool.deallocate" []
                 ForEachIn "mPreallocatedInteractionMarkers" $ Fn "mNPhaseCore.mShapeInteractionPool.deallocate" []]

             Fn "registerSceneInteractions"
                [ForEachIn "mPreallocatedShapeInteractions" $ Do
                    [Fn "registerInteraction" []
                     Fn "mNPhaseCore.registerInteraction" []]
                 ForEachIn "mPreallocatedInteractionMarkers" $ Do
                    [Fn "registerInteraction" []
                     Fn "mNPhaseCore.registerInteraction" []]]

             Fn "registerInteractions"
                [ForEachIn "mPreallocatedShapeInteractions" $ Do
                    [Fn "interaction.mActor0.registerInteractionInActor" []
                     Fn "interaction.mActor1.registerInteractionInActor" []
                     Fn "interaction.mActor0.registerCountedInteraction" []
                     Fn "interaction.mActor1.registerCountedInteraction" []]
                 ForEachIn "mPreallocatedInteractionMarkers" $ Fn "registerInActors(NULL)" []]

             Fn "registerContactManagers"
                [ForEachIn "mPreallocatedContactManagers" $ Fn "nphaseContext.registerContactManager" []
                 Fn "nphaseContext->unlock" []]

             Fn "islandInsertion"
                [ForEachIn "mPreallocatedShapeInteractions" $ Fn "mSimpleIslandManager->addContactManager" []
                 Fn "mSimpleIslandManager.firstPassIslandGen"
                    [Fn "mSpeculativeIslandManager.clearDeactivations" []
                     Fn "mSpeculativeIslandManager.wakeIslands" []
                     Fn "mSpeculativeIslandManager.processNewEdges" []
                     Fn "mSpeculativeIslandManager.removeDestroyedEdges" []
                     Fn "mSpeculativeIslandManager.processLostEdges" []]]

             Fn "postBroadPhaseStage3"
                [Fn "finishBroadPhaseStage2"
                    [ForEachIn "mAABBManager.mDestroyedOverlaps" $ Do
                        [Fn "mNPhaseCore.onOverlapRemovedStage1" []
                         Fn "mNPhaseCore.lostTouchReports" []
                         Fn "interaction.destroyManager" []
                         Fn "interaction.clearIslandGenData" []
                         Fn "unregisterInteraction" []
                         Fn "mNPhaseCore.unregisterInteraction" []
                         Fn "mNPhaseCore.onOverlapRemoved" []]
                     ForEachIn "mAABBManager.mDestroyedOverlaps" $ Fn "mNPhaseCore.onOverlapRemoved" []
                     Fn "processLostTouchPairs" []]]

             Fn "unblockNarrowPhase" []

             Fn "advanceStep" []
             Fn "secondPassNarrowPhase" []
             Fn "postNarrowPhase"
                [Fn "releaseConstraints" []]
             Fn "islandGen"
                [Fn "processNarrowPhaseTouchEvents" []
                 Fn "processNarrowPhaseTouchEventsStage2" []]
             Fn "setEdgesConnected"
                [Fn "wakeObjectsUp" []]
             Fn "postIslandGen" []
             Fn "solver"
                [Fn "beforeSolver" []]
             Fn "updateBodiesAndShapes" []
             Fn "updateDynamics" []
             Fn "updateSimulationController" []
             Fn "processLostContacts" []
             Fn "processNarrowPhaseLostTouchEvents" []
             Fn "processNarrowPhaseLostTouchEventsIslands" []
             Fn "processLostContacts2" []
             Fn "unregisterInteractions" []
             Fn "lostTouchReports" []
             Fn "destroyManagers" []
             Fn "processLostContacts3" []
             Fn "postThirdPassIslandGen"
                [Fn "putObjectsToSleep" []
                 Fn "putInteractionsToSleep" []]
             Fn "postSolver"
                [Fn "integrateKinematicPose" []]
             Fn "constraintProjection" []
             Fn "afterIntegration"
                [Fn "updateKinematicCached" []
                 Fn "checkForceThresholdContactEvents" []]
             Fn "finalizationPhase"
                [Fn "fireOnAdvanceCallback" []
                 Fn "checkConstraintBreakage" []]
             Fn "prepareOutOfBoundsCallbacks" []
             Fn "fireOutOfBoundsCallbacks" []
             Fn "fireTriggerCallbacks" []]

[<AutoOpen>]
module PhysicModule =

    type NpPhysics =
        { Scenes : NpScene array
          MaterialManager : PxsMaterialManager }
        interface Singleton