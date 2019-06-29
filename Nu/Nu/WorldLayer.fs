// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2018.

namespace Nu
open System
open System.IO
open System.ComponentModel
open System.Reflection
open System.Runtime.CompilerServices
open Prime
open Nu

[<AutoOpen; ModuleBinding>]
module WorldLayerModule =

    type Layer with
    
        member this.GetId world = World.getLayerId this world
        member this.Id = Lens.makeReadOnly Property? Id this.GetId this
        member this.GetName world = World.getLayerName this world
        member this.Name = Lens.makeReadOnly Property? Name this.GetName this
        member this.GetDispatcher world = World.getLayerDispatcher this world
        member this.Dispatcher = Lens.makeReadOnly Property? Dispatcher this.GetDispatcher this
        member this.GetPersistent world = World.getLayerPersistent this world
        member this.SetPersistent value world = World.setLayerPersistent value this world
        member this.Persistent = Lens.make Property? Persistent this.GetPersistent this.SetPersistent this
        member this.GetCreationTimeStamp world = World.getLayerCreationTimeStamp this world
        member this.CreationTimeStamp = Lens.makeReadOnly Property? CreationTimeStamp this.GetCreationTimeStamp this
        member this.GetImperative world = World.getLayerImperative this world
        member this.Imperative = Lens.makeReadOnly Property? Imperative this.GetImperative this
        member this.GetScriptOpt world = World.getLayerScriptOpt this world
        member this.SetScriptOpt value world = World.setLayerScriptOpt value this world
        member this.ScriptOpt = Lens.make Property? ScriptOpt this.GetScriptOpt this.SetScriptOpt this
        member this.GetScript world = World.getLayerScript this world
        member this.SetScript value world = World.setLayerScript value this world
        member this.Script = Lens.make Property? Script this.GetScript this.SetScript this
        member this.GetScriptFrame world = World.getLayerScriptFrame this world
        member this.ScriptFrame = Lens.makeReadOnly Property? Script this.GetScriptFrame this
        member internal this.GetScriptUnsubscriptions world = World.getLayerScriptUnsubscriptions this world
        member internal this.SetScriptUnsubscriptions value world = World.setLayerScriptUnsubscriptions value this world
        member internal this.ScriptUnsubscriptions = Lens.make Property? ScriptUnsubscriptions this.GetScriptUnsubscriptions this.SetScriptUnsubscriptions this
        member this.GetOnRegister world = World.getLayerOnRegister this world
        member this.SetOnRegister value world = World.setLayerOnRegister value this world
        member this.OnRegister = Lens.make Property? OnRegister this.GetOnRegister this.SetOnRegister this
        member this.GetOnUnregister world = World.getLayerOnUnregister this world
        member this.SetOnUnregister value world = World.setLayerOnUnregister value this world
        member this.OnUnregister = Lens.make Property? OnUnregister this.GetOnUnregister this.SetOnUnregister this
        member this.GetOnUpdate world = World.getLayerOnUpdate this world
        member this.SetOnUpdate value world = World.setLayerOnUpdate value this world
        member this.OnUpdate = Lens.make Property? OnUpdate this.GetOnUpdate this.SetOnUpdate this
        member this.GetOnPostUpdate world = World.getLayerOnPostUpdate this world
        member this.SetOnPostUpdate value world = World.setLayerOnPostUpdate value this world
        member this.OnPostUpdate = Lens.make Property? OnPostUpdate this.GetOnPostUpdate this.SetOnPostUpdate this
        member this.GetOnSignal world = World.getLayerOnSignal this world
        member this.SetOnSignal value world = World.setLayerOnSignal value this world
        member this.OnSignal = Lens.make Property? OnSignal this.GetOnSignal this.SetOnSignal this
        member this.GetDepth world = World.getLayerDepth this world
        member this.SetDepth value world = World.setLayerDepth value this world
        member this.Depth = Lens.make Property? Depth this.GetDepth this.SetDepth this
        member this.GetVisible world = World.getLayerVisible this world
        member this.SetVisible value world = World.setLayerVisible value this world
        member this.Visible = Lens.make Property? Visible this.GetVisible this.SetVisible this
        member this.ChangeEvent propertyName = Events.Change propertyName --> this
        member this.RegisterEvent = Events.Register --> this
        member this.UnregisteringEvent = Events.Unregistering --> this
        member this.UpdateEvent = Events.Update --> this
        member this.PostUpdateEvent = Events.PostUpdate --> this
        member this.SignalEvent = Events.Signal --> this

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
        member this.GetExists world = World.getLayerExists this world

        /// Check that a layer dispatches in the same manner as the dispatcher with the given type.
        member this.DispatchesAs (dispatcherType, world) = Reflection.dispatchesAs dispatcherType (this.GetDispatcher world)

        /// Check that a layer dispatches in the same manner as the dispatcher with the given type.
        member this.DispatchesAs<'a> world = this.DispatchesAs (typeof<'a>, world)

        /// Resolve a relation in the context of a layer.
        member this.Resolve relation = Layer (Relation.resolve this.LayerAddress relation)

        /// Get a layer's change event address.
        member this.GetChangeEvent propertyName = Events.Change propertyName --> this.LayerAddress

        /// Send a signal to the layer.
        member this.Signal signal world = World.signalLayer signal this world

    type World with

        static member internal updateLayer (layer : Layer) world =
            World.withEventContext (fun world ->
                
                // update via dispatcher
                let dispatcher = layer.GetDispatcher world
                let world = dispatcher.Update (layer, world)

                // run script update
                let world = World.evalWithLogging (layer.GetOnUpdate world) (layer.GetScriptFrame world) layer world |> snd'

                // publish update event
                let eventTrace = EventTrace.record "World" "updateLayer" EventTrace.empty
                World.publishPlus World.sortSubscriptionsByHierarchy () (Events.Update --> layer) eventTrace Default.Game true world)
                layer
                world

        static member internal postUpdateLayer (layer : Layer) world =
            World.withEventContext (fun world ->

                // post-update via dispatcher
                let dispatcher = layer.GetDispatcher world
                let world = dispatcher.PostUpdate (layer, world)

                // run script post-update
                let world = World.evalWithLogging (layer.GetOnPostUpdate world) (layer.GetScriptFrame world) layer world |> snd'

                // run script post-update
                let eventTrace = EventTrace.record "World" "postUpdateLayer" EventTrace.empty
                World.publishPlus World.sortSubscriptionsByHierarchy () (Events.PostUpdate --> layer) eventTrace Default.Game true world)
                layer
                world

        static member internal actualizeLayer (layer : Layer) world =
            World.withEventContext (fun world ->
                let dispatcher = layer.GetDispatcher world
                dispatcher.Actualize (layer, world))
                layer
                world

        /// Get all the layers in a screen.
        [<FunctionBinding>]
        static member getLayers (screen : Screen) world =
            match Address.getNames screen.ScreenAddress with
            | [screenName] ->
                let layerDirectoryOpt = UMap.tryFindFast screenName (World.getScreenDirectory world)
                if FOption.isSome layerDirectoryOpt then
                    let layerDirectory = FOption.get layerDirectoryOpt
                    layerDirectory.Value |>
                    UMap.fold (fun state _ layerDirectory -> Layer layerDirectory.Key :: state) [] :>
                    _ seq
                else failwith ("Invalid screen address '" + scstring screen.ScreenAddress + "'.")
            | _ -> failwith ("Invalid screen address '" + scstring screen.ScreenAddress + "'.")

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
            Seq.fold (fun layerDescriptors layer -> World.writeLayer layer LayerDescriptor.empty world :: layerDescriptors) screenDescriptor.Layers |>
            fun layerDescriptors -> { screenDescriptor with Layers = layerDescriptors }

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
                screenDescriptor.Layers
                ([], world)

        /// Read a layer from a file.
        [<FunctionBinding>]
        static member readLayerFromFile (filePath : string) nameOpt screen world =
            let layerDescriptorStr = File.ReadAllText filePath
            let layerDescriptor = scvalue<LayerDescriptor> layerDescriptorStr
            World.readLayer layerDescriptor nameOpt screen world

        /// Transform a stream into existing layers.
        static member streamLayers (mapper : 'a -> LayerContent) (screen : Screen) (stream : Stream<'a list, World>) =
            stream |>
            Stream.optimize |>
            Stream.insert (makeGuid ()) |>
            Stream.map (fun (guid, list) -> List.mapi (fun i a -> PartialComparable.make (makeGuidDeterministic i guid) (mapper a)) list |> Set.ofList) |>
            Stream.fold (fun (p, _, _) c -> (c, Set.difference c p, Set.difference p c)) (Set.empty, Set.empty, Set.empty) |>
            Stream.mapEffect (fun evt world ->
                let (current, added, removed) = evt.Data
                let world =
                    Seq.fold (fun world guidAndContent ->
                        let (guid, content) = PartialComparable.unmake guidAndContent
                        match World.tryGetKeyedValue (scstring guid) world with
                        | None -> World.expandLayerContent (Some guid) content screen world
                        | Some _ -> world)
                        world added
                let world =
                    Seq.fold (fun world guidAndContent ->
                        let (guid, _) = PartialComparable.unmake guidAndContent
                        match World.tryGetKeyedValue (scstring guid) world with
                        | Some layersObj ->
                            let layers = layersObj :?> Layer
                            let world = World.removeKeyedValue (scstring guid) world
                            World.destroyLayer layers world
                        | None -> failwithumf ())
                        world removed
                (current, world))

        /// Turn an layers stream into a series of layers.
        static member expandLayerStream (lens : World Lens) mapper screen world =
            Stream.make (Events.Register --> lens.This.ParticipantAddress) |>
            Stream.sum (Stream.make lens.ChangeEvent) |>
            Stream.mapEvent (fun _ world -> lens.Get world |> Reflection.objToObjList) |>
            World.streamLayers mapper screen |>
            Stream.subscribe (fun _ value -> value) Default.Game $ world

        /// Turn layer content into a layer.
        static member expandLayerContent guidOpt content screen world =
            match LayerContent.expand content screen world with
            | Choice1Of3 (lens, mapper) ->
                World.expandLayerStream lens mapper screen world
            | Choice2Of3 (name, descriptor, equations, streams, entityFilePaths, entityContents) ->
                let (layer, world) = World.readLayer descriptor (Some name) screen world
                let world = match guidOpt with Some guid -> World.addKeyedValue (scstring guid) layer world | None -> world
                let world =
                    List.fold (fun world (_, entityName, filePath) ->
                        World.readEntityFromFile filePath (Some entityName) layer world |> snd)
                        world entityFilePaths
                let world =
                    List.fold (fun world (name, simulant, property, breaking) ->
                        WorldModule.equate5 name simulant property breaking world)
                        world equations
                let world =
                    List.fold (fun world (layer, lens, mapper) ->
                        World.expandEntityStream lens mapper None layer world)
                        world streams
                let world =
                    List.fold (fun world (owner, entityContents) ->
                        List.fold (fun world entityContent ->
                            World.expandEntityContent (Some (makeGuid ())) entityContent (Some owner) layer world)
                            world entityContents)
                        world entityContents
                world
            | Choice3Of3 (layerName, filePath) ->
                let (layer, world) = World.readLayerFromFile filePath (Some layerName) screen world
                match guidOpt with Some guid -> World.addKeyedValue (scstring guid) layer world | None -> world

    /// Represents the property value of an layer as accessible via reflection.
    type [<ReferenceEquality>] LayerPropertyValue =
        | LayerPropertyDescriptor of PropertyDescriptor
        | LayerPropertyInfo of PropertyInfo

        /// Check that an layer contains the given property.
        static member containsProperty (property : PropertyInfo) =
            let properties = typeof<LayerState>.GetProperties property.Name
            Seq.exists (fun item -> item = property) properties

        /// Get the layer's property value.
        static member getValue property (layer : Layer) world =
            let propertyName =
                match property with
                | LayerPropertyDescriptor propertyDescriptor -> propertyDescriptor.PropertyName
                | LayerPropertyInfo propertyInfo -> propertyInfo.Name
            let property = World.getLayerProperty propertyName layer world
            match property.PropertyValue with
            | :? DesignerProperty as dp -> dp.DesignerValue
            | value -> value

        /// Set the layer's property value.
        static member setValue property propertyValue (layer : Layer) world =
            let (propertyName, propertyType) =
                match property with
                | LayerPropertyDescriptor propertyDescriptor -> (propertyDescriptor.PropertyName, propertyDescriptor.PropertyType)
                | LayerPropertyInfo propertyInfo -> (propertyInfo.Name, propertyInfo.PropertyType)
            let propertyOld = World.getLayerProperty propertyName layer world
            match propertyOld.PropertyValue with
            | :? DesignerProperty as dp ->
                let propertyValue = { dp with DesignerValue = propertyValue }
                let propertyValue = { PropertyType = typeof<DesignerProperty>; PropertyValue = propertyValue }
                World.setLayerProperty propertyName propertyValue layer world
            | _ -> World.setLayerProperty propertyName { PropertyType = propertyType; PropertyValue = propertyValue } layer world

        /// Get the property descriptors of as constructed from the given function in the given context.
        static member getPropertyDescriptors makePropertyDescriptor contextOpt =
            // OPTIMIZATION: seqs used for speed.
            let properties = typeof<LayerState>.GetProperties ()
            let typeConverterAttribute = TypeConverterAttribute typeof<SymbolicConverter>
            let properties = Seq.filter (fun (property : PropertyInfo) -> property.PropertyType <> typeof<Xtension>) properties
            let properties = Seq.filter (fun (property : PropertyInfo) -> Seq.isEmpty (property.GetCustomAttributes<ExtensionAttribute> ())) properties
            let properties = Seq.filter (fun (property : PropertyInfo) -> Reflection.isPropertyPersistentByName property.Name) properties
            let propertyDescriptors = Seq.map (fun property -> makePropertyDescriptor (LayerPropertyInfo property, [|typeConverterAttribute|])) properties
            let propertyDescriptors =
                match contextOpt with
                | Some (layer, world) ->
                    let properties' = World.getLayerXtensionProperties layer world
                    let propertyDescriptors' =
                        Seq.fold
                            (fun propertyDescriptors' (propertyName, property : Property) ->
                                let propertyType = property.PropertyType
                                if Reflection.isPropertyPersistentByName propertyName then
                                    let propertyDescriptor = LayerPropertyDescriptor { PropertyName = propertyName; PropertyType = propertyType }
                                    let propertyDescriptor : System.ComponentModel.PropertyDescriptor = makePropertyDescriptor (propertyDescriptor, [|typeConverterAttribute|])
                                    propertyDescriptor :: propertyDescriptors'
                                else propertyDescriptors')
                            []
                            properties'
                    Seq.append propertyDescriptors' propertyDescriptors
                | None -> propertyDescriptors
            List.ofSeq propertyDescriptors

namespace Debug
open Nu
type Layer =

    /// Provides a full view of all the properties of a layer. Useful for debugging such
    /// as with the Watch feature in Visual Studio.
    static member view layer world = World.viewLayerProperties layer world