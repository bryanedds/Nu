namespace Nu
open System
open Prime
open Nu

/// Represents an untyped message to a subsystem.
type SubsystemMessage = obj

/// Represents an untyped result of a subsystem.
type SubsystemResult = obj

/// The type of subsystem. Dictates where subsystem's processing happens in the game loop.
type SubsystemType =
    | UpdateType
    | RenderType
    | AudioType

/// Represents a subsystem by which additional engine-level subsystems such as AI, optimized
/// special FX, and the like can be added.
type 'w Subsystem =
    interface
        /// The type of subsystem. Dictates where its processing happens in the game loop.
        abstract SubsystemType : SubsystemType
        /// The ordering by which the subsystem will be processed relative to other subsystems of the same type.
        abstract SubsystemOrder : single
        /// Clear the messages queued by subsystem.
        abstract ClearMessages : unit -> 'w Subsystem
        /// Enqueue a message for the subsystem.
        abstract EnqueueMessage : SubsystemMessage -> 'w Subsystem
        /// Processed the queued messages with the subsystem.
        abstract ProcessMessages : 'w -> SubsystemResult * 'w Subsystem * 'w
        /// Apply the result of the message processing to the world.
        abstract ApplyResult : SubsystemResult * 'w -> 'w
        /// Clean up any resources used by the subsystem.
        abstract CleanUp : 'w -> 'w Subsystem * 'w
        end

[<AutoOpen>]
module SubsystemsModule =

    /// The subsystems of a world.
    type [<ReferenceEquality>] 'w Subsystems =
        private
            { SubsystemMap : Map<string, 'w Subsystem> }

    [<RequireQualifiedAccess>]
    module Subsystems =
    
        let getSubsystemMap subsystems =
            subsystems.SubsystemMap
    
        let getSubsystem<'s, 'w when 's :> 'w Subsystem> (name : string) (subsystems : 'w Subsystems) : 's =
            Map.find name subsystems.SubsystemMap :?> 's
    
        let getSubsystemBy<'s, 't, 'w when 's :> 'w Subsystem> by name (subsystems : 'w Subsystems) : 't =
            let subsystem = getSubsystem<'s, 'w> name subsystems
            by subsystem
    
        let setSubsystem<'s, 'w when 's :> 'w Subsystem> (subsystem : 's) name (subsystems : 'w Subsystems) =
#if DEBUG
            if not ^ Map.containsKey name subsystems.SubsystemMap then
                failwith ^ "Cannot set the state of a non-existent subsystem '" + name + "'"
#endif
            { SubsystemMap = Map.add name (subsystem :> 'w Subsystem) subsystems.SubsystemMap }
    
        let updateSubsystem<'s, 'w when 's :> 'w Subsystem> (updater : 's -> 'w -> 's) name subsystems world =
            let subsystem = getSubsystem<'s, 'w> name subsystems
            let subsystem = updater subsystem world
            setSubsystem subsystem name subsystems
    
        let updateSubsystems<'s, 'w when 's :> 'w Subsystem> (updater : 'w Subsystem -> 'w -> 'w Subsystem) subsystems world =
            Map.fold
                (fun subsystems name subsystem ->
                    let subsystem = updater subsystem world
                    setSubsystem subsystem name subsystems)
                subsystems
                subsystems.SubsystemMap
    
        let clearSubsystemsMessages<'s, 'w when 's :> 'w Subsystem> subsystems (world : 'w) =
            updateSubsystems (fun is _ -> is.ClearMessages ()) subsystems world
    
        let make subsystems =
            { SubsystemMap = subsystems }