// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2018.

namespace Nu
open System
open Prime
open Nu

/// Represents an untyped message to a subsystem.
type SubsystemMessage = obj

/// Represents an untyped result of a subsystem.
type SubsystemResult = obj

/// The type of subsystem. Dictates where subsystem's processing happens in the game loop.
type [<Struct>] SubsystemType =
    | UpdateType
    | RenderType
    | AudioType

/// Represents a subsystem for additional engine-level subsystems such as AI, optimized
/// particles, or efficient level-of-detailed subsystems like mega-swarms and Minecraft blocks,
/// a la - http://www.dataorienteddesign.com/dodmain/node6.html
type 'w ISubsystem =
    interface
        /// The type of subsystem. Dictates where its processing happens in the game loop.
        abstract SubsystemType : SubsystemType
        /// The ordering by which the subsystem will be processed relative to other subsystems of the same type.
        abstract SubsystemOrder : single
        /// Clear the messages queued by subsystem.
        abstract ClearMessages : unit -> 'w ISubsystem
        /// Enqueue a message for the subsystem.
        abstract EnqueueMessage : SubsystemMessage -> 'w ISubsystem
        /// Processed the queued messages with the subsystem.
        abstract ProcessMessages : 'w -> SubsystemResult * 'w ISubsystem * 'w
        /// Apply the result of the message processing to the world.
        abstract ApplyResult : SubsystemResult * 'w -> 'w
        /// Clean up any resources used by the subsystem.
        abstract CleanUp : 'w -> 'w ISubsystem * 'w
        end

[<RequireQualifiedAccess>]
module Subsystem =

    /// The type of subsystem. Dictates where its processing happens in the game loop.
    let subsystemType (subsystem : 'w ISubsystem) =
        subsystem.SubsystemType

    /// The ordering by which the subsystem will be processed relative to other subsystems of the same type.
    let subsystemOrder (subsystem : 'w ISubsystem) =
        subsystem.SubsystemOrder

    /// Clear the messages queued by subsystem.
    let clearMessages (subsystem : 'w ISubsystem) =
        subsystem.ClearMessages ()

    /// Enqueue a message for the subsystem.
    let enqueueMessage message (subsystem : 'w ISubsystem) =
        subsystem.EnqueueMessage message

    /// Processed the queued messages with the subsystem.
    let processMessages (subsystem : 'w ISubsystem) world =
        subsystem.ProcessMessages world

    /// Apply the result of the message processing to the world.
    let applyResult subsystemResult (subsystem : 'w ISubsystem) world =
        subsystem.ApplyResult (subsystemResult, world)

    /// Clean up any resources used by the subsystem.
    let cleanUp (subsystem : 'w ISubsystem) world =
        subsystem.CleanUp world

[<AutoOpen>]
module SubsystemsModule =

    /// The subsystems of a world.
    type [<ReferenceEquality>] 'w Subsystems =
        private
            { SubsystemMap : UMap<string, 'w ISubsystem> }

    [<RequireQualifiedAccess>]
    module Subsystems =
    
        let getSubsystemMap subsystems =
            subsystems.SubsystemMap

        let addSubsystem<'s, 'w when 's :> 'w ISubsystem> name (subsystem : 's) (subsystems : 'w Subsystems) =
            { SubsystemMap = UMap.add name (subsystem :> 'w ISubsystem) subsystems.SubsystemMap }
    
        let removeSubsystem<'s, 'w when 's :> 'w ISubsystem> name (subsystems : 'w Subsystems) =
            { SubsystemMap = UMap.remove name subsystems.SubsystemMap }

        let containsSubsystem<'s, 'w when 's :> 'w ISubsystem> name (subsystems : 'w Subsystems) =
            UMap.containsKey name subsystems.SubsystemMap

        let getSubsystem<'s, 'w when 's :> 'w ISubsystem> (name : string) (subsystems : 'w Subsystems) : 's =
            UMap.find name subsystems.SubsystemMap :?> 's
    
        let getSubsystemBy<'s, 't, 'w when 's :> 'w ISubsystem> by name (subsystems : 'w Subsystems) : 't =
            let subsystem = getSubsystem<'s, 'w> name subsystems
            by subsystem
    
        let updateSubsystem<'s, 'w when 's :> 'w ISubsystem> (updater : 's -> 'w -> 's) name subsystems world =
            let subsystem = getSubsystem<'s, 'w> name subsystems
            let subsystem = updater subsystem world
            addSubsystem name subsystem subsystems
    
        let updateSubsystems<'s, 'w when 's :> 'w ISubsystem> (updater : 'w ISubsystem -> 'w -> 'w ISubsystem) subsystems world =
            UMap.fold
                (fun subsystems name subsystem ->
                    let subsystem = updater subsystem world
                    addSubsystem name subsystem subsystems)
                subsystems
                subsystems.SubsystemMap
    
        let clearSubsystemsMessages<'s, 'w when 's :> 'w ISubsystem> subsystems (world : 'w) =
            updateSubsystems (fun subsystem _ -> subsystem.ClearMessages ()) subsystems world
    
        let make subsystems =
            { SubsystemMap = subsystems }

/// The subsystems of a world.
type 'w Subsystems = 'w SubsystemsModule.Subsystems