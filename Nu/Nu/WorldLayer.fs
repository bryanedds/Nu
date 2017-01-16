// Nu Game Engine.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Nu
open System
open System.IO
open Prime
open Nu

[<AutoOpen>]
module WorldLayerModule =

    type Layer with
    
        member this.GetId world = World.getLayerId this world
        member this.Id = PropertyTag.makeReadOnly this Property? Id this.GetId
        member this.GetName world = World.getLayerName this world
        member this.Name = PropertyTag.makeReadOnly this Property? Name this.GetName
        member this.GetXtension world = World.getLayerXtension this world
        member this.Xtension = PropertyTag.makeReadOnly this Property? Xtension this.GetXtension
        member this.GetDispatcherNp world = World.getLayerDispatcherNp this world
        member this.DispatcherNp = PropertyTag.makeReadOnly this Property? DispatcherNp this.GetDispatcherNp
        member this.GetSpecialization world = World.getLayerSpecialization this world
        member this.Specialization = PropertyTag.makeReadOnly this Property? Specialization this.GetSpecialization
        member this.GetClassification world = Classification.make (getTypeName ^ this.GetDispatcherNp world) (this.GetSpecialization world)
        member this.Classification = PropertyTag.makeReadOnly this Property? Classification this.GetClassification
        member this.GetPersistent world = World.getLayerPersistent this world
        member this.SetPersistent value world = World.setLayerPersistent value this world
        member this.Persistent = PropertyTag.makeReadOnly this Property? Persistent this.GetPersistent
        member this.GetCreationTimeStampNp world = World.getLayerCreationTimeStampNp this world
        member this.CreationTimeStampNp = PropertyTag.makeReadOnly this Property? CreationTimeStampNp this.GetCreationTimeStampNp
        member this.GetImperative world = World.getLayerImperative this world
        member this.Imperative = PropertyTag.makeReadOnly this Property? Imperative this.GetImperative
        member this.GetDepth world = World.getLayerDepth this world
        member this.SetDepth value world = World.setLayerDepth value this world
        member this.Depth = PropertyTag.makeReadOnly this Property? Depth this.GetDepth
        member this.GetVisible world = World.getLayerVisible this world
        member this.SetVisible value world = World.setLayerVisible value this world
        member this.Visible = PropertyTag.makeReadOnly this Property? Visible this.GetVisible

        /// Try to get a property value and type.
        member this.TryGetProperty propertyName world = World.tryGetLayerProperty propertyName this world

        /// Get a property value and type.
        member this.GetProperty propertyName world = World.getLayerProperty propertyName this world

        /// Get a property value.
        member this.Get<'a> propertyName world : 'a = World.getLayerProperty propertyName this world |> fst :?> 'a

        /// Try to set a property value with explicit type.
        member this.TrySetProperty propertyName property world = World.trySetLayerProperty propertyName property this world

        /// Set a property value with explicit type.
        member this.SetProperty propertyName property world = World.setLayerProperty propertyName property this world

        /// Set a property value.
        member this.Set<'a> propertyName (value : 'a) world = World.setLayerProperty propertyName (value :> obj, typeof<'a>) this world

        /// Check that a layer dispatches in the same manner as the dispatcher with the target type.
        member this.DispatchesAs (dispatcherTargetType : Type) world = Reflection.dispatchesAs dispatcherTargetType (this.GetDispatcherNp world)

    type World with

        static member private removeLayer layer world =
            let removeEntities layer world =
                let entities = World.getEntities layer world
                World.destroyEntitiesImmediate entities world
            World.removeLayer3 removeEntities layer world

        static member internal updateLayer (layer : Layer) world =
            World.withEventContext (fun world ->
                let dispatcher = layer.GetDispatcherNp world
                let world = dispatcher.Update (layer, world)
                let eventTrace = EventTrace.record "World" "updateLayer" EventTrace.empty
                World.publish7 World.sortSubscriptionsByHierarchy () (Events.Update ->- layer) eventTrace Simulants.Game true world)
                (atooa layer.LayerAddress)
                world

        static member internal postUpdateLayer (layer : Layer) world =
            World.withEventContext (fun world ->
                let dispatcher = layer.GetDispatcherNp world
                let world = dispatcher.PostUpdate (layer, world)
                let eventTrace = EventTrace.record "World" "postUpdateLayer" EventTrace.empty
                World.publish7 World.sortSubscriptionsByHierarchy () (Events.PostUpdate ->- layer) eventTrace Simulants.Game true world)
                (atooa layer.LayerAddress)
                world

        static member internal actualizeLayer (layer : Layer) world =
            World.withEventContext (fun world ->
                let dispatcher = layer.GetDispatcherNp world
                dispatcher.Actualize (layer, world))
                (atooa layer.LayerAddress)
                world

        /// Get all the layers in a screen.
        static member getLayers screen world =
            match Address.getNames screen.ScreenAddress with
            | [screenName] ->
                match UMap.tryFind screenName ^ World.getScreenDirectory world with
                | Some (_, layerDirectory) ->
                    UMap.fold (fun state _ (layerAddress, _) -> Layer.proxy layerAddress :: state) [] layerDirectory :> _ seq
                | None -> failwith ^ "Invalid screen address '" + scstring screen.ScreenAddress + "'."
            | _ -> failwith ^ "Invalid screen address '" + scstring screen.ScreenAddress + "'."

        /// Destroy a layer in the world immediately. Can be dangerous if existing in-flight publishing depends on the
        /// layer's existence. Consider using World.destroyLayer instead.
        static member destroyLayerImmediate layer world =
            World.removeLayer layer world

        /// Destroy a layer in the world at the end of the current update.
        static member destroyLayer layer world =
            let tasklet =
                { ScheduledTime = World.getTickTime world
                  Command = { Execute = fun world -> World.destroyLayerImmediate layer world }}
            World.addTasklet tasklet world
            
        /// Destroy multiple layers in the world immediately. Can be dangerous if existing in-flight publishing depends
        /// on any of the layers' existences. Consider using World.destroyLayers instead.
        static member destroyLayersImmediate layers world =
            List.foldBack
                (fun layer world -> World.destroyLayerImmediate layer world)
                (List.ofSeq layers)
                world

        /// Destroy multiple layers from the world at the end of the current update.
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
        static member writeLayerToFile (filePath : string) layer world =
            let filePathTmp = filePath + ".tmp"
            let layerDescriptor = World.writeLayer layer LayerDescriptor.empty world
            let layerDescriptorStr = scstring layerDescriptor
            let layerDescriptorPretty = Symbol.prettyPrint String.Empty layerDescriptorStr
            File.WriteAllText (filePathTmp, layerDescriptorPretty)
            File.Delete filePath
            File.Move (filePathTmp, filePath)

        /// Read a layer from a layer descriptor.
        static member readLayer layerDescriptor nameOpt screen world =
            World.readLayer5 World.readEntities layerDescriptor nameOpt screen world

        /// Read a layer from a file.
        static member readLayerFromFile (filePath : string) nameOpt screen world =
            let layerDescriptorStr = File.ReadAllText filePath
            let layerDescriptor = scvalue<LayerDescriptor> layerDescriptorStr
            World.readLayer layerDescriptor nameOpt screen world

        /// Read multiple layers from a screen descriptor.
        static member readLayers screenDescriptor screen world =
            List.foldBack
                (fun layerDescriptor (layers, world) ->
                    let (layer, world) = World.readLayer layerDescriptor None screen world
                    (layer :: layers, world))
                screenDescriptor.Layers
                ([], world)

namespace Debug
open Nu
type Layer =

    /// Provides a full view of all the properties of a layer. Useful for debugging such
    /// as with the Watch feature in Visual Studio.
    static member view layer world = World.viewLayerProperties layer world