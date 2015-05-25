// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2015.

namespace Nu
open System
open OpenTK
open Prime
open Nu
open Nu.Constants
open Nu.WorldConstants

/// The subsystem for the world's renderer.
type [<ReferenceEquality>] RendererSubsystem =
    private
        { SubsystemOrder : single
          Renderer : IRenderer }

    static member private getScreenTransitionRenderDescriptors camera (screen : Screen) transitionDescriptor world =
        match transitionDescriptor.OptDissolveImage with
        | Some dissolveImage ->
            let progress = single (screen.GetTransitionTicksNp world) / single transitionDescriptor.TransitionLifetime
            let alpha = match transitionDescriptor.TransitionType with Incoming -> 1.0f - progress | Outgoing -> progress
            let color = Vector4 (Vector3.One, alpha)
            [LayerableDescriptor
                { Depth = Single.MaxValue
                  LayeredDescriptor =
                    SpriteDescriptor
                        { Position = -camera.EyeSize * 0.5f // negation for right-handedness
                          Size = camera.EyeSize
                          Rotation = 0.0f
                          ViewType = Absolute
                          OptInset = None
                          Image = dissolveImage
                          Color = color }}]
        | None -> []

    static member private getRenderDescriptors world =
        match World.getOptSelectedScreen world with
        | Some selectedScreen ->
            if World.containsScreen selectedScreen world then
                let groups = World.proxyGroups selectedScreen world
                let descriptors =
                    Seq.map (fun group -> World.proxyEntities group world) groups |>
                    Seq.concat |>
                    Seq.filter (fun entity-> entity.GetVisible world) |>
                    Seq.map (fun entity -> World.getEntityRenderDescriptors entity world) |>
                    Seq.concat |>
                    List.ofSeq
                match selectedScreen.GetTransitionStateNp world with
                | IncomingState -> descriptors @ RendererSubsystem.getScreenTransitionRenderDescriptors (World.getCamera world) selectedScreen (selectedScreen.GetIncoming world) world
                | OutgoingState -> descriptors @ RendererSubsystem.getScreenTransitionRenderDescriptors (World.getCamera world) selectedScreen (selectedScreen.GetOutgoing world) world
                | IdlingState -> descriptors
            else []
        | None -> []

    interface Subsystem with
        member this.SubsystemType = RenderType
        member this.SubsystemOrder = this.SubsystemOrder
        member this.ClearMessages () = { this with Renderer = this.Renderer.ClearMessages () } :> Subsystem
        member this.EnqueueMessage message = { this with Renderer = this.Renderer.EnqueueMessage (message :?> RenderMessage) } :> Subsystem

        member this.ProcessMessages world =
            let camera = World.getCamera world
            let renderDescriptors = RendererSubsystem.getRenderDescriptors world
            (() :> obj, { this with Renderer = this.Renderer.Render camera renderDescriptors } :> Subsystem)

        member this.ApplyResult _ world = world
        member this.CleanUp world = (this :> Subsystem, world)

    static member make subsystemOrder renderer =
        { SubsystemOrder = subsystemOrder
          Renderer = renderer }

[<AutoOpen>]
module WorldRenderModule =

    type World with

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