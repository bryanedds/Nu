// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu
open System
open System.Collections.Generic
open Prime
open Nu

[<AutoOpen; ModuleBinding>]
module WorldRenderModule =

    /// The subsystem for the world's renderer.
    type [<ReferenceEquality>] RendererSubsystem =
        private
            { Renderer : Renderer }
    
        interface World Subsystem with
            
            member this.PopMessages () =
                (this.Renderer.PopMessages () :> obj, this :> World Subsystem)
            
            member this.ClearMessages () =
                Renderer.clearMessages this.Renderer
                this :> World Subsystem
            
            member this.EnqueueMessage message =
                Renderer.enqueueMessage (message :?> RenderMessage) this.Renderer
                this :> World Subsystem
            
            member this.ProcessMessages messages world =
                let messages = messages :?> RenderMessage List
                Renderer.render (World.getEyeCenter world) (World.getEyeSize world) messages this.Renderer :> obj

            member this.ApplyResult (_, world) =
                world
            
            member this.CleanUp world =
                let this = { this with Renderer = Renderer.cleanUp this.Renderer }
                (this :> World Subsystem, world)

        static member make renderer =
            { Renderer = renderer }

    type World with

        static member internal getRenderer world =
            world.Subsystems.Renderer :?> RendererSubsystem

        static member internal setRenderer renderer world =
            World.updateSubsystems (fun subsystems -> { subsystems with Renderer = renderer }) world

        static member internal updateRenderer updater world =
            World.setRenderer (updater (World.getRenderer world :> World Subsystem)) world

        /// Enqueue a rendering message to the world.
        static member enqueueRenderMessage (message : RenderMessage) world =
            (World.getRenderer world).Renderer.EnqueueMessage message
            world

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