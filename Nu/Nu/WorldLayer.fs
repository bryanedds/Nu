// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2017.

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
        member this.Id = PropertyTag.makeReadOnly this Property? Id this.GetId
        member this.GetName world = World.getLayerName this world
        member this.Name = PropertyTag.makeReadOnly this Property? Name this.GetName
        member this.GetDispatcherNp world = World.getLayerDispatcherNp this world
        member this.DispatcherNp = PropertyTag.makeReadOnly this Property? DispatcherNp this.GetDispatcherNp
        member this.GetPersistent world = World.getLayerPersistent this world
        member this.SetPersistent value world = World.setLayerPersistent value this world
        member this.Persistent = PropertyTag.make this Property? Persistent this.GetPersistent this.SetPersistent
        member this.GetCreationTimeStampNp world = World.getLayerCreationTimeStampNp this world
        member this.CreationTimeStampNp = PropertyTag.makeReadOnly this Property? CreationTimeStampNp this.GetCreationTimeStampNp
        member this.GetImperative world = World.getLayerImperative this world
        member this.Imperative = PropertyTag.makeReadOnly this Property? Imperative this.GetImperative
        member this.GetScriptOpt world = World.getLayerScriptOpt this world
        member this.SetScriptOpt value world = World.setLayerScriptOpt value this world
        member this.ScriptOpt = PropertyTag.make this Property? ScriptOpt this.GetScriptOpt this.SetScriptOpt
        member this.GetScript world = World.getLayerScript this world
        member this.SetScript value world = World.setLayerScript value this world
        member this.Script = PropertyTag.make this Property? Script this.GetScript this.SetScript
        member this.GetScriptFrameNp world = World.getLayerScriptFrameNp this world
        member this.ScriptFrameNp = PropertyTag.makeReadOnly this Property? Script this.GetScriptFrameNp
        member this.GetOnRegister world = World.getLayerOnRegister this world
        member this.SetOnRegister value world = World.setLayerOnRegister value this world
        member this.OnRegister = PropertyTag.make this Property? OnRegister this.GetOnRegister this.SetOnRegister
        member this.GetOnUnregister world = World.getLayerOnUnregister this world
        member this.SetOnUnregister value world = World.setLayerOnUnregister value this world
        member this.OnUnregister = PropertyTag.make this Property? OnUnregister this.GetOnUnregister this.SetOnUnregister
        member this.GetOnUpdate world = World.getLayerOnUpdate this world
        member this.SetOnUpdate value world = World.setLayerOnUpdate value this world
        member this.OnUpdate = PropertyTag.make this Property? OnUpdate this.GetOnUpdate this.SetOnUpdate
        member this.GetOnPostUpdate world = World.getLayerOnPostUpdate this world
        member this.SetOnPostUpdate value world = World.setLayerOnPostUpdate value this world
        member this.OnPostUpdate = PropertyTag.make this Property? OnPostUpdate this.GetOnPostUpdate this.SetOnPostUpdate
        member this.GetDepth world = World.getLayerDepth this world
        member this.SetDepth value world = World.setLayerDepth value this world
        member this.Depth = PropertyTag.make this Property? Depth this.GetDepth this.SetDepth
        member this.GetVisible world = World.getLayerVisible this world
        member this.SetVisible value world = World.setLayerVisible value this world
        member this.Visible = PropertyTag.make this Property? Visible this.GetVisible this.SetVisible

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
            match (World.getGameState world).OmniscreenOpt with
            | Some omniscreen when Address.head this.LayerAddress = Address.head omniscreen.ScreenAddress -> true
            | _ ->
                match (World.getGameState world).SelectedScreenOpt with
                | Some screen when Address.head this.LayerAddress = Address.head screen.ScreenAddress -> true
                | _ -> false

        /// Check that a layer exists in the world.
        member this.GetExists world = World.layerExists this world

        /// Check that a layer dispatches in the same manner as the dispatcher with the target type.
        member this.DispatchesAs (dispatcherTargetType : Type) world = Reflection.dispatchesAs dispatcherTargetType (this.GetDispatcherNp world)

        /// Resolve a relation in the context of an entity.
        member this.Resolve relation = Layer (Relation.resolve this.LayerAddress relation)

    type World with

        static member internal updateLayer (layer : Layer) world =
            World.withEventContext (fun world ->
                
                // update via dispatcher
                let dispatcher = layer.GetDispatcherNp world
                let world = dispatcher.Update (layer, world)

                // run script update
                let world = World.evalWithLogging (layer.GetOnUpdate world) (layer.GetScriptFrameNp world) layer world |> snd

                // publish update event
                let eventTrace = EventTrace.record "World" "updateLayer" EventTrace.empty
                World.publishPlus World.sortSubscriptionsByHierarchy () (Events.Update ->- layer) eventTrace Simulants.Game true world)
                layer
                world

        static member internal postUpdateLayer (layer : Layer) world =
            World.withEventContext (fun world ->

                // post-update via dispatcher
                let dispatcher = layer.GetDispatcherNp world
                let world = dispatcher.PostUpdate (layer, world)

                // run script post-update
                let world = World.evalWithLogging (layer.GetOnPostUpdate world) (layer.GetScriptFrameNp world) layer world |> snd

                // run script post-update
                let eventTrace = EventTrace.record "World" "postUpdateLayer" EventTrace.empty
                World.publishPlus World.sortSubscriptionsByHierarchy () (Events.PostUpdate ->- layer) eventTrace Simulants.Game true world)
                layer
                world

        static member internal actualizeLayer (layer : Layer) world =
            World.withEventContext (fun world ->
                let dispatcher = layer.GetDispatcherNp world
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
            let tasklet =
                { ScheduledTime = World.getTickTime world
                  Command = { Execute = fun world -> World.destroyLayerImmediate layer world }}
            World.addTasklet tasklet world
            
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
            let tasklet =
                { ScheduledTime = World.getTickTime world
                  Command = { Execute = fun world -> World.destroyLayersImmediate layers world }}
            World.addTasklet tasklet world

        /// Write a layer to a layer descriptor.
        static member writeLayer layer layerDescriptor world =
            let writeEntities layer layerDescriptor world =
                let entities = World.getEntities layer world
                World.writeEntities entities layerDescriptor world
            World.writeLayer4 writeEntities layer layerDescriptor world

        /// Write multiple layers to a screen descriptor.
        static member writeLayers layers screenDescriptor world =
            layers |>
            Seq.sortBy (fun (layer : Layer) -> layer.GetCreationTimeStampNp world) |>
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
                    let (layer, world) = World.readLayer layerDescriptor None screen world
                    (layer :: layers, world))
                screenDescriptor.Layers
                ([], world)

        /// Read a layer from a file.
        [<FunctionBinding>]
        static member readLayerFromFile (filePath : string) nameOpt screen world =
            let layerDescriptorStr = File.ReadAllText filePath
            let layerDescriptor = scvalue<LayerDescriptor> layerDescriptorStr
            World.readLayer layerDescriptor nameOpt screen world

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