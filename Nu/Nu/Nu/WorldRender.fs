// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2014.

namespace Nu
open System
open OpenTK
open Prime
open Nu
open Nu.Constants
open Nu.WorldConstants

[<AutoOpen>]
module WorldRenderModule =

    /// The subsystem for the world's renderer.
    type [<ReferenceEquality>] RendererSubsystem =
        { SubsystemType : SubsystemType
          SubsystemOrder : single
          Renderer : IRenderer }

        static member private getScreenTransitionRenderDescriptors camera screen transition =
            match transition.OptDissolveImage with
            | Some dissolveImage ->
                let progress = single screen.TransitionTicksNp / single transition.TransitionLifetime
                let alpha = match transition.TransitionType with Incoming -> 1.0f - progress | Outgoing -> progress
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
            match World.getOptSelectedScreenRep world with
            | Some selectedScreenRep ->
                let selectedScreenAddress = selectedScreenRep.ScreenAddress
                match Map.tryFind (Address.head selectedScreenAddress) (World.getScreenMap world) with
                | Some (_, groupMap) ->
                    let entityReps =
                        Map.fold
                            (fun entityReps groupName (_, entityMap) ->
                                let groupAddress = satoga selectedScreenAddress groupName
                                Map.fold
                                    (fun entityReps entityName _ -> { EntityAddress = gatoea groupAddress entityName } :: entityReps)
                                    entityReps
                                    entityMap)
                            []
                            groupMap
                    let descriptors = List.map (fun entityRep -> Entity.getRenderDescriptors entityRep world) entityReps
                    let descriptors = List.concat descriptors
                    let selectedScreen = World.getScreen selectedScreenAddress world
                    match selectedScreen.ScreenStateNp with
                    | IncomingState -> descriptors @ RendererSubsystem.getScreenTransitionRenderDescriptors world.State.Camera selectedScreen selectedScreen.Incoming
                    | OutgoingState -> descriptors @ RendererSubsystem.getScreenTransitionRenderDescriptors world.State.Camera selectedScreen selectedScreen.Outgoing
                    | IdlingState -> descriptors
                | None -> []
            | None -> []

        interface Subsystem with
            member this.SubsystemType = this.SubsystemType
            member this.SubsystemOrder = this.SubsystemOrder
            member this.ClearMessages () = { this with Renderer = this.Renderer.ClearMessages () } :> Subsystem
            member this.EnqueueMessage message = { this with Renderer = this.Renderer.EnqueueMessage (message :?> RenderMessage) } :> Subsystem

            member this.ProcessMessages world =
                let renderDescriptors = RendererSubsystem.getRenderDescriptors world
                (() :> obj, { this with Renderer = this.Renderer.Render world.State.Camera renderDescriptors } :> Subsystem)

            member this.ApplyResult (_, world) = world
            member this.CleanUp world = (this :> Subsystem, world)

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