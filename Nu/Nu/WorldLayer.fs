// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu
open System
open System.IO
open Prime
open Nu

[<AutoOpen; ModuleBinding>]
module WorldLayerModule =

    type Layer with
    
        member this.GetDispatcher world = World.getLayerDispatcher this world
        member this.Dispatcher = lensReadOnly Property? Dispatcher this.GetDispatcher this
        member this.GetModel<'a> world = World.getLayerModel<'a> this world
        member this.SetModel<'a> value world = World.setLayerModel<'a> value this world
        member this.Model<'a> () = lens Property? Model this.GetModel<'a> this.SetModel<'a> this
        member this.GetVisible world = World.getLayerVisible this world
        member this.SetVisible value world = World.setLayerVisible value this world
        member this.Visible = lens Property? Visible this.GetVisible this.SetVisible this
        member this.GetPersistent world = World.getLayerPersistent this world
        member this.SetPersistent value world = World.setLayerPersistent value this world
        member this.Persistent = lens Property? Persistent this.GetPersistent this.SetPersistent this
        member this.GetScriptFrame world = World.getLayerScriptFrame this world
        member this.ScriptFrame = lensReadOnly Property? Script this.GetScriptFrame this
        member this.GetCreationTimeStamp world = World.getLayerCreationTimeStamp this world
        member this.CreationTimeStamp = lensReadOnly Property? CreationTimeStamp this.GetCreationTimeStamp this
        member this.GetId world = World.getLayerId this world
        member this.Id = lensReadOnly Property? Id this.GetId this

        member this.ChangeEvent propertyName = Events.Change propertyName --> this
        member this.RegisterEvent = Events.Register --> this
        member this.UnregisteringEvent = Events.Unregistering --> this
        member this.UpdateEvent = Events.Update --> this
        member this.PostUpdateEvent = Events.PostUpdate --> this

        /// Try to get a property value and type.
        member this.TryGetProperty propertyName world = World.tryGetLayerProperty propertyName this world

        /// Get a property value and type.
        member this.GetProperty propertyName world = World.getLayerProperty propertyName this world

        /// Get a property value.
        member this.Get<'a> propertyName world : 'a = (World.getLayerProperty propertyName this world).PropertyValue :?> 'a

        /// Try to set a property value with explicit type.
        member this.TrySetProperty propertyName property world = World.trySetLayerProperty propertyName property this world

        /// Set a property value with explicit type.
        member this.SetProperty propertyName property world = World.setLayerProperty propertyName property this world

        /// Set a property value.
        member this.Set<'a> propertyName (value : 'a) world = World.setLayerProperty propertyName { PropertyType = typeof<'a>; PropertyValue = value } this world

        /// Check that a layer is selected.
        member this.GetSelected world =
            match (World.getGameState world).OmniScreenOpt with
            | Some omniScreen when Address.head this.LayerAddress = Address.head omniScreen.ScreenAddress -> true
            | _ ->
                match (World.getGameState world).SelectedScreenOpt with
                | Some screen when Address.head this.LayerAddress = Address.head screen.ScreenAddress -> true
                | _ -> false

        /// Check that a layer exists in the world.
        member this.Exists world = World.getLayerExists this world

        /// Check that a layer is selected.
        member this.Selected world = WorldModule.isSelected this world

        /// Check that a layer dispatches in the same manner as the dispatcher with the given type.
        member this.Is (dispatcherType, world) = Reflection.dispatchesAs dispatcherType (this.GetDispatcher world)

        /// Check that a layer dispatches in the same manner as the dispatcher with the given type.
        member this.Is<'a> world = this.Is (typeof<'a>, world)

        /// Resolve a relation in the context of a layer.
        member this.Resolve relation = resolve<Layer> this relation

        /// Relate a layer to a simulant.
        member this.Relate simulant = relate<Layer> this simulant

        /// Get a layer's change event address.
        member this.GetChangeEvent propertyName = Events.Change propertyName --> this.LayerAddress

        /// Try to signal a layer.
        member this.TrySignal signal world = (this.GetDispatcher world).TrySignal (signal, this, world)

    type World with

        static member internal updateLayer (layer : Layer) world =

            // update via dispatcher
            let dispatcher = layer.GetDispatcher world
            let world = dispatcher.Update (layer, world)

            // publish update event
            let eventTrace = EventTrace.record "World" "updateLayer" EventTrace.empty
            World.publishPlus () (Events.Update --> layer) eventTrace Simulants.Game false world

        static member internal postUpdateLayer (layer : Layer) world =

            // post-update via dispatcher
            let dispatcher = layer.GetDispatcher world
            let world = dispatcher.PostUpdate (layer, world)

            // publish post-update event
            let eventTrace = EventTrace.record "World" "postUpdateLayer" EventTrace.empty
            World.publishPlus () (Events.PostUpdate --> layer) eventTrace Simulants.Game false world

        static member internal actualizeLayer (layer : Layer) world =
            let dispatcher = layer.GetDispatcher world
            dispatcher.Actualize (layer, world)

        /// Get all the layers in a screen.
        [<FunctionBinding>]
        static member getLayers (screen : Screen) world =
            match Address.getNames screen.ScreenAddress with
            | [|screenName|] ->
                match UMap.tryFind screenName (World.getScreenDirectory world) with
                | Some layerDirectory ->
                    layerDirectory.Value |>
                    UMap.fold (fun state _ layerDirectory -> Layer layerDirectory.Key :: state) [] :>
                    _ seq
                | None -> failwith ("Invalid screen address '" + scstring screen.ScreenAddress + "'.")
            | _ -> failwith ("Invalid screen address '" + scstring screen.ScreenAddress + "'.")

        /// Create a layer and add it to the world.
        [<FunctionBinding "createLayer">]
        static member createLayer4 dispatcherName nameOpt (screen : Screen) world =
            let dispatchers = World.getLayerDispatchers world
            let dispatcher =
                match Map.tryFind dispatcherName dispatchers with
                | Some dispatcher -> dispatcher
                | None -> failwith ("Could not find a LayerDispatcher named '" + dispatcherName + "'. Did you forget to provide this dispatcher from your NuPlugin?")
            let layerState = LayerState.make nameOpt dispatcher
            let layerState = Reflection.attachProperties LayerState.copy layerState.Dispatcher layerState world
            let layer = Layer (screen.ScreenAddress <-- ntoa<Layer> layerState.Name)
            let world =
                if World.getLayerExists layer world then
                    Log.debug "Scheduling layer creation assuming existing layer at the same address is being destroyed."
                    World.schedule2 (World.addLayer false layerState layer) world
                else World.addLayer false layerState layer world
            (layer, world)

        /// Create a layer from a simulnat descriptor.
        static member createLayer3 descriptor screen world =
            let (layer, world) =
                World.createLayer4 descriptor.SimulantDispatcherName descriptor.SimulantNameOpt screen world
            let world =
                List.fold (fun world (propertyName, property) ->
                    World.setLayerProperty propertyName property layer world)
                    world descriptor.SimulantProperties
            let world =
                List.fold (fun world childDescriptor ->
                    World.createEntity4 DefaultOverlay childDescriptor layer world |> snd)
                    world descriptor.SimulantChildren
            (layer, world)

        /// Create a layer and add it to the world.
        static member createLayer<'d when 'd :> LayerDispatcher> nameOpt screen world =
            World.createLayer4 typeof<'d>.Name nameOpt screen world

        /// Destroy a layer in the world immediately. Can be dangerous if existing in-flight publishing depends on the
        /// layer's existence. Consider using World.destroyLayer instead.
        static member destroyLayerImmediate layer world =
            let destroyEntitiesImmediate layer world =
                let entities = World.getEntities layer world
                World.destroyEntitiesImmediate entities world
            World.removeLayer3 destroyEntitiesImmediate layer world

        /// Destroy a layer in the world at the end of the current update.
        [<FunctionBinding>]
        static member destroyLayer layer world =
            World.schedule2 (World.destroyLayerImmediate layer) world
            
        /// Destroy multiple layers in the world immediately. Can be dangerous if existing in-flight publishing depends
        /// on any of the layers' existences. Consider using World.destroyLayers instead.
        static member destroyLayersImmediate (layers : Layer seq) world =
            List.foldBack
                (fun layer world -> World.destroyLayerImmediate layer world)
                (List.ofSeq layers)
                world

        /// Destroy multiple layers from the world at the end of the current update.
        [<FunctionBinding>]
        static member destroyLayers layers world =
            World.schedule2 (World.destroyLayersImmediate layers) world

        /// Write a layer to a layer descriptor.
        static member writeLayer layer layerDescriptor world =
            let writeEntities layer layerDescriptor world =
                let entities = World.getEntities layer world
                World.writeEntities entities layerDescriptor world
            World.writeLayer4 writeEntities layer layerDescriptor world

        /// Write multiple layers to a screen descriptor.
        static member writeLayers layers screenDescriptor world =
            layers |>
            Seq.sortBy (fun (layer : Layer) -> layer.GetCreationTimeStamp world) |>
            Seq.filter (fun (layer : Layer) -> layer.GetPersistent world) |>
            Seq.fold (fun layerDescriptors layer -> World.writeLayer layer LayerDescriptor.empty world :: layerDescriptors) screenDescriptor.LayerDescriptors |>
            fun layerDescriptors -> { screenDescriptor with LayerDescriptors = layerDescriptors }

        /// Write a layer to a file.
        [<FunctionBinding>]
        static member writeLayerToFile (filePath : string) layer world =
            let filePathTmp = filePath + ".tmp"
            let prettyPrinter = (SyntaxAttribute.getOrDefault typeof<GameDescriptor>).PrettyPrinter
            let layerDescriptor = World.writeLayer layer LayerDescriptor.empty world
            let layerDescriptorStr = scstring layerDescriptor
            let layerDescriptorPretty = PrettyPrinter.prettyPrint layerDescriptorStr prettyPrinter
            File.WriteAllText (filePathTmp, layerDescriptorPretty)
            File.Delete filePath
            File.Move (filePathTmp, filePath)

        /// Read a layer from a layer descriptor.
        static member readLayer layerDescriptor nameOpt screen world =
            World.readLayer5 World.readEntities layerDescriptor nameOpt screen world

        /// Read multiple layers from a screen descriptor.
        static member readLayers screenDescriptor screen world =
            List.foldBack
                (fun layerDescriptor (layers, world) ->
                    let layerNameOpt = LayerDescriptor.getNameOpt layerDescriptor
                    let (layer, world) = World.readLayer layerDescriptor layerNameOpt screen world
                    (layer :: layers, world))
                screenDescriptor.LayerDescriptors
                ([], world)

        /// Read a layer from a file.
        [<FunctionBinding>]
        static member readLayerFromFile (filePath : string) nameOpt screen world =
            let layerDescriptorStr = File.ReadAllText filePath
            let layerDescriptor = scvalue<LayerDescriptor> layerDescriptorStr
            World.readLayer layerDescriptor nameOpt screen world

        /// Turn a layers lens into a series of live layers.
        static member expandLayers (lens : Lens<obj, World>) sieve spread indexOpt mapper origin screen world =
            let mapperGeneralized = fun i a w -> mapper i a w :> SimulantContent
            World.expandSimulants lens sieve spread indexOpt mapperGeneralized origin screen world

        /// Expand an existing layer.
        static member private expandLayerExisting origin handlers binds streams entityFilePaths entityContents (layer : Layer) world =
            let world =
                List.fold (fun world (_, entityName, filePath) ->
                    World.readEntityFromFile filePath (Some entityName) layer world |> snd)
                    world entityFilePaths
            let world =
                List.fold (fun world (simulant, left : World Lens, right) ->
                    WorldModule.bind5 simulant left right world)
                    world binds
            let world =
                List.fold (fun world (handler, address, simulant) ->
                    World.monitor (fun (evt : Event) world ->
                        let signal = handler evt
                        let owner = match origin with SimulantOrigin simulant -> simulant | FacetOrigin (simulant, _) -> simulant
                        let world = WorldModule.trySignal signal owner world
                        (Cascade, world))
                        address simulant world)
                    world handlers
            let world =
                List.fold (fun world (layer, lens, sieve, spread, indexOpt, mapper) ->
                    World.expandEntities lens sieve spread indexOpt mapper origin layer world)
                    world streams
            let world =
                List.fold (fun world (owner, entityContents) ->
                    List.fold (fun world entityContent ->
                        World.expandEntityContent entityContent (SimulantOrigin owner) layer world |> snd)
                        world entityContents)
                    world entityContents
            (Some layer, world)

        /// Turn layer content into a live layer.
        static member expandLayerContent content origin screen world =
            if World.getScreenExists screen world then
                match LayerContent.expand content screen world with
                | Left (lens, sieve, spread, indexOpt, mapper) ->
                    let world = World.expandLayers lens sieve spread indexOpt mapper origin screen world
                    (None, world)
                | Right (_, Left descriptor, handlers, binds, streams, entityFilePaths, entityContents) ->
                    let (layer, world) = World.createLayer3 descriptor screen world
                    let (layer, world) = World.expandLayerExisting origin handlers binds streams entityFilePaths entityContents layer world
                    (Some layer, world)
                | Right (layerName, Right filePath, handlers, binds, streams, entityFilePaths, entityContents) ->
                    let (layer, world) = World.readLayerFromFile filePath (Some layerName) screen world
                    let (layer, world) = World.expandLayerExisting origin handlers binds streams entityFilePaths entityContents layer world
                    (Some layer, world)
            else (None, world)

namespace Debug
open Nu
type Layer =

    /// Provides a full view of all the properties of a layer. Useful for debugging such
    /// as with the Watch feature in Visual Studio.
    static member view layer world = World.viewLayerProperties layer world