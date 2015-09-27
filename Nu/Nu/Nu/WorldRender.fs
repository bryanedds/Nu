// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2015.

namespace Nu
open System
open OpenTK
open Prime
open Nu

/// The subsystem for the world's renderer.
type [<ReferenceEquality>] RendererSubsystem =
    private
        { SubsystemOrder : single
          Renderer : IRenderer }

    interface Subsystem with
        member this.SubsystemType = RenderType
        member this.SubsystemOrder = this.SubsystemOrder
        member this.ClearMessages () = { this with Renderer = this.Renderer.ClearMessages () } :> Subsystem
        member this.EnqueueMessage message = { this with Renderer = this.Renderer.EnqueueMessage (message :?> RenderMessage) } :> Subsystem
        member this.ProcessMessages world = (() :> obj, { this with Renderer = this.Renderer.Render ^ World.getCamera world } :> Subsystem, world)
        member this.ApplyResult (_, world) = world
        member this.CleanUp world = (this :> Subsystem, world)

    static member make subsystemOrder renderer =
        { SubsystemOrder = subsystemOrder
          Renderer = renderer }

[<AutoOpen>]
module WorldRenderModule =

    type World with

        /// Add a rendering message to the world.
        static member addRenderMessage (message : RenderMessage) world =
            World.updateSubsystem (fun rs _ -> rs.EnqueueMessage message) Constants.Engine.RendererSubsystemName world

        /// Hint that a rendering asset package with the given name should be loaded. Should be
        /// used to avoid loading assets at inconvenient times (such as in the middle of game play!)
        static member hintRenderPackageUse packageName world =
            let hintRenderPackageUseMessage = HintRenderPackageUseMessage { PackageName = packageName }
            World.addRenderMessage hintRenderPackageUseMessage world
            
        /// Hint that a rendering package should be unloaded since its assets will not be used
        /// again (or until specified via World.hintRenderPackageUse).
        static member hintRenderPackageDisuse packageName world =
            let hintRenderPackageDisuseMessage = HintRenderPackageDisuseMessage { PackageName = packageName }
            World.addRenderMessage hintRenderPackageDisuseMessage world
            
        /// Send a message to the renderer to reload its rendering assets.
        static member reloadRenderAssets world =
            let reloadRenderAssetsMessage = ReloadRenderAssetsMessage
            World.addRenderMessage reloadRenderAssetsMessage world