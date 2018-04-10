// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2018.

namespace Nu
open System
open Prime
open Nu

[<AutoOpen; ModuleBinding>]
module WorldRenderModule =

    /// The subsystem for the world's renderer.
    type [<ReferenceEquality>] RendererSubsystem =
        private
            { SubsystemOrder : single
              Renderer : IRenderer }
    
        interface World Subsystem with
            member this.SubsystemType = RenderType
            member this.SubsystemOrder = this.SubsystemOrder
            member this.ClearMessages () = { this with Renderer = this.Renderer.ClearMessages () } :> World Subsystem
            member this.EnqueueMessage message = { this with Renderer = this.Renderer.EnqueueMessage (message :?> RenderMessage) } :> World Subsystem
            member this.ProcessMessages world =
                let this = { this with Renderer = this.Renderer.Render (World.getEyeCenter world) (World.getEyeSize world) }
                (() :> obj, this :> World Subsystem, world)
            member this.ApplyResult (_, world) = world
            member this.CleanUp world = (this :> World Subsystem, world)

        static member make subsystemOrder renderer =
            { SubsystemOrder = subsystemOrder
              Renderer = renderer }

    type World with

        /// Enqueue a rendering message to the world.
        static member enqueueRenderMessage (message : RenderMessage) world =
            World.updateSubsystem (fun rs _ -> rs.EnqueueMessage message) Constants.Engine.RendererSubsystemName world

        /// Hint that a rendering asset package with the given name should be loaded. Should be
        /// used to avoid loading assets at inconvenient times (such as in the middle of game play!)
        [<FunctionBinding>]
        static member hintRenderPackageUse packageName world =
            let hintRenderPackageUseMessage = HintRenderPackageUseMessage packageName
            World.enqueueRenderMessage hintRenderPackageUseMessage world
            
        /// Hint that a rendering package should be unloaded since its assets will not be used
        /// again (or until specified via World.hintRenderPackageUse).
        [<FunctionBinding>]
        static member hintRenderPackageDisuse packageName world =
            let hintRenderPackageDisuseMessage = HintRenderPackageDisuseMessage packageName
            World.enqueueRenderMessage hintRenderPackageDisuseMessage world
            
        /// Send a message to the renderer to reload its rendering assets.
        [<FunctionBinding>]
        static member reloadRenderAssets world =
            let reloadRenderAssetsMessage = ReloadRenderAssetsMessage
            World.enqueueRenderMessage reloadRenderAssetsMessage world

/// The subsystem for the world's renderer.
type RendererSubsystem = WorldRenderModule.RendererSubsystem