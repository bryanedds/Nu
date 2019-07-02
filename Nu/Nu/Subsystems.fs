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

/// Represents a subsystem for additional engine-level subsystems such as AI, optimized
/// particles, or efficient level-of-detailed subsystems like mega-swarms and Minecraft blocks,
/// a la - http://www.dataorienteddesign.com/dodmain/node6.html
type 'w Subsystem =
    interface
        abstract ClearMessages : unit -> 'w Subsystem
        /// Enqueue a message for the subsystem.
        abstract EnqueueMessage : SubsystemMessage -> 'w Subsystem
        /// Processed the queued messages with the subsystem.
        abstract ProcessMessages : 'w -> SubsystemResult * 'w Subsystem
        /// Apply the result of the message processing to the world.
        abstract ApplyResult : SubsystemResult * 'w -> 'w
        /// Clean up any resources used by the subsystem.
        abstract CleanUp : 'w -> 'w Subsystem * 'w
        end

[<RequireQualifiedAccess>]
module Subsystem =

    /// Clear the messages queued by subsystem.
    let clearMessages (subsystem : 'w Subsystem) =
        subsystem.ClearMessages ()

    /// Enqueue a message for the subsystem.
    let enqueueMessage message (subsystem : 'w Subsystem) =
        subsystem.EnqueueMessage message

    /// Processed the queued messages with the subsystem.
    let processMessages (subsystem : 'w Subsystem) world =
        subsystem.ProcessMessages world

    /// Apply the result of the message processing to the world.
    let applyResult subsystemResult (subsystem : 'w Subsystem) world =
        subsystem.ApplyResult (subsystemResult, world)

    /// Clean up any resources used by the subsystem.
    let cleanUp (subsystem : 'w Subsystem) world =
        subsystem.CleanUp world

[<AutoOpen>]
module SubsystemsModule =

    /// The subsystems of a world.
    type [<ReferenceEquality>] 'w Subsystems =
        private
            { SubsystemMap : UMap<string, 'w Subsystem> }

    [<RequireQualifiedAccess>]
    module Subsystems =
    
        let getSubsystemMap subsystems =
            subsystems.SubsystemMap

        let addSubsystem<'s, 'w when 's :> 'w Subsystem> name (subsystem : 's) (subsystems : 'w Subsystems) =
            { SubsystemMap = UMap.add name (subsystem :> 'w Subsystem) subsystems.SubsystemMap }
    
        let removeSubsystem<'s, 'w when 's :> 'w Subsystem> name (subsystems : 'w Subsystems) =
            { SubsystemMap = UMap.remove name subsystems.SubsystemMap }

        let containsSubsystem<'s, 'w when 's :> 'w Subsystem> name (subsystems : 'w Subsystems) =
            UMap.containsKey name subsystems.SubsystemMap

        let getSubsystem<'s, 'w when 's :> 'w Subsystem> (name : string) (subsystems : 'w Subsystems) : 's =
            UMap.find name subsystems.SubsystemMap :?> 's
    
        let getSubsystemBy<'s, 't, 'w when 's :> 'w Subsystem> by name (subsystems : 'w Subsystems) : 't =
            let subsystem = getSubsystem<'s, 'w> name subsystems
            by subsystem
    
        let updateSubsystem<'s, 'w when 's :> 'w Subsystem> (updater : 's -> 'w -> 's) name subsystems world =
            let subsystem = getSubsystem<'s, 'w> name subsystems
            let subsystem = updater subsystem world
            addSubsystem name subsystem subsystems
    
        let updateSubsystems<'s, 'w when 's :> 'w Subsystem> (updater : 'w Subsystem -> 'w -> 'w Subsystem) subsystems world =
            UMap.fold
                (fun subsystems name subsystem ->
                    let subsystem = updater subsystem world
                    addSubsystem name subsystem subsystems)
                subsystems
                subsystems.SubsystemMap
    
        let clearSubsystemsMessages<'s, 'w when 's :> 'w Subsystem> subsystems (world : 'w) =
            updateSubsystems (fun subsystem _ -> subsystem.ClearMessages ()) subsystems world
    
        let make subsystems =
            { SubsystemMap = subsystems }

/// The subsystems of a world.
type 'w Subsystems = 'w SubsystemsModule.Subsystems