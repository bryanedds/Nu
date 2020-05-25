// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu
open System
open Prime
open Nu

/// Represents an untyped message to a subsystem.
type SubsystemMessage = obj

/// Represents an untyped collection of messages to a subsystem.
type SubsystemMessages = obj

/// Represents an untyped result of a subsystem.
type SubsystemResult = obj

/// Represents a subsystem for additional engine-level subsystems such as AI, optimized
/// particles, or efficient level-of-detailed subsystems like mega-swarms and Minecraft blocks,
/// a la - http://www.dataorienteddesign.com/dodmain/node6.html
type 'w Subsystem =
    interface
        /// Pop all the messages belonging to the subsystem.
        abstract PopMessages : unit -> SubsystemMessages * 'w Subsystem
        /// Clear the message belonging to the subsystem.
        abstract ClearMessages : unit -> 'w Subsystem
        /// Enqueue a message for the subsystem.
        abstract EnqueueMessage : SubsystemMessage -> 'w Subsystem
        /// Processed the queued messages with the subsystem.
        abstract ProcessMessages : SubsystemMessages -> 'w -> SubsystemResult
        /// Apply the result of the message processing to the world.
        abstract ApplyResult : SubsystemResult * 'w -> 'w
        /// Clean up any resources used by the subsystem.
        abstract CleanUp : 'w -> 'w Subsystem * 'w
        end

[<RequireQualifiedAccess>]
module Subsystem =

    /// Pop the messages queued by subsystem.
    let popMessages (subsystem : 'w Subsystem) =
        subsystem.PopMessages ()

    /// Clear the messages queued by subsystem.
    let clearMessages (subsystem : 'w Subsystem) =
        subsystem.ClearMessages ()

    /// Enqueue a message for the subsystem.
    let enqueueMessage message (subsystem : 'w Subsystem) =
        subsystem.EnqueueMessage message

    /// Processed the queued messages with the subsystem.
    let processMessages messages (subsystem : 'w Subsystem) world =
        subsystem.ProcessMessages messages world

    /// Apply the result of the message processing to the world.
    let applyResult subsystemResult (subsystem : 'w Subsystem) world =
        subsystem.ApplyResult (subsystemResult, world)

    /// Clean up any resources used by the subsystem.
    let cleanUp (subsystem : 'w Subsystem) world =
        subsystem.CleanUp world

/// The subsystems of a world.
type [<ReferenceEquality; NoComparison>] 'w Subsystems =
    { PhysicsEngine : 'w Subsystem
      Renderer : 'w Subsystem
      AudioPlayer : 'w Subsystem }

[<RequireQualifiedAccess>]
module Subsystems =

    let make physicsEngine renderer audioPlayer =
        { PhysicsEngine = physicsEngine
          Renderer = renderer 
          AudioPlayer = audioPlayer }

    let cleanUp subsystems world =
        let (physicsEngine, world) = Subsystem.cleanUp subsystems.PhysicsEngine world
        let (renderer, world) = Subsystem.cleanUp subsystems.Renderer world
        let (audioPlayer, world) = Subsystem.cleanUp subsystems.AudioPlayer world
        let subsystems = make physicsEngine renderer audioPlayer
        (subsystems, world)