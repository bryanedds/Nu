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
        member this.GetDispatcherNp world = World.getLayerDispatcherNp this world
        member this.DispatcherNp = PropertyTag.makeReadOnly this Property? DispatcherNp this.GetDispatcherNp
        member this.GetSpecialization world = World.getLayerSpecialization this world
        member this.Specialization = PropertyTag.makeReadOnly this Property? Specialization this.GetSpecialization
        member this.GetClassification world = Classification.make (getTypeName ^ this.GetDispatcherNp world) (this.GetSpecialization world)
        member this.Classification = PropertyTag.makeReadOnly this Property? Classification this.GetClassification
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
        member this.SetScriptFrameNp value world = World.setLayerScriptFrameNp value this world
        member this.ScriptFrameNp = PropertyTag.make this Property? Script this.GetScriptFrameNp this.SetScriptFrameNp
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
        member this.GetOnActualize world = World.getLayerOnActualize this world
        member this.SetOnActualize value world = World.setLayerOnActualize value this world
        member this.OnActualize = PropertyTag.make this Property? OnActualize this.GetOnActualize this.SetOnActualize
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
                World.publishPlus World.sortSubscriptionsByHierarchy () (Events.Update ->- layer) eventTrace Simulants.Game true world)
                layer
                world

        static member internal postUpdateLayer (layer : Layer) world =
            World.withEventContext (fun world ->
                let dispatcher = layer.GetDispatcherNp world
                let world = dispatcher.PostUpdate (layer, world)
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
        static member getLayers (screen : Screen) world =
            match Address.getNames screen.ScreenAddress with
            | [screenName] ->
                match UMap.tryFind screenName ^ World.getScreenDirectory world with
                | Some (_, layerDirectory) ->
                    UMap.fold (fun state _ (layerAddress, _) -> Layer layerAddress :: state) [] layerDirectory :> _ seq
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
            let layerDescriptorPretty = Symbol.prettyPrint layerDescriptorStr
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