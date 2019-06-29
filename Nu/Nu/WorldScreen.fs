// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2018.

namespace Nu
open System
open System.IO
open Prime
open Nu

[<AutoOpen; ModuleBinding>]
module WorldScreenModule =

    type Screen with
    
        member this.GetId world = World.getScreenId this world
        member this.Id = Lens.makeReadOnly Property? Id this.GetId this
        member this.GetName world = World.getScreenName this world
        member this.Name = Lens.makeReadOnly Property? Name this.GetName this
        member this.GetDispatcher world = World.getScreenDispatcher this world
        member this.Dispatcher = Lens.makeReadOnly Property? Dispatcher this.GetDispatcher this
        member this.GetPersistent world = World.getScreenPersistent this world
        member this.SetPersistent value world = World.setScreenPersistent value this world
        member this.Persistent = Lens.makeReadOnly Property? Persistent this.GetPersistent this
        member this.GetCreationTimeStamp world = World.getScreenCreationTimeStamp this world
        member this.CreationTimeStamp = Lens.makeReadOnly Property? CreationTimeStamp this.GetCreationTimeStamp this
        member this.GetImperative world = World.getScreenImperative this world
        member this.Imperative = Lens.makeReadOnly Property? Imperative this.GetImperative this
        member this.GetScriptOpt world = World.getScreenScriptOpt this world
        member this.SetScriptOpt value world = World.setScreenScriptOpt value this world
        member this.ScriptOpt = Lens.make Property? ScriptOpt this.GetScriptOpt this.SetScriptOpt this
        member this.GetScript world = World.getScreenScript this world
        member this.SetScript value world = World.setScreenScript value this world
        member this.Script = Lens.make Property? Script this.GetScript this.SetScript this
        member this.GetScriptFrame world = World.getScreenScriptFrame this world
        member this.ScriptFrame = Lens.makeReadOnly Property? Script this.GetScriptFrame this
        member internal this.GetScriptUnsubscriptions world = World.getScreenScriptUnsubscriptions this world
        member internal this.SetScriptUnsubscriptions value world = World.setScreenScriptUnsubscriptions value this world
        member internal this.ScriptUnsubscriptions = Lens.make Property? ScriptUnsubscriptions this.GetScriptUnsubscriptions this.SetScriptUnsubscriptions this
        member this.GetOnRegister world = World.getScreenOnRegister this world
        member this.SetOnRegister value world = World.setScreenOnRegister value this world
        member this.OnRegister = Lens.make Property? OnRegister this.GetOnRegister this.SetOnRegister this
        member this.GetOnUnregister world = World.getScreenOnUnregister this world
        member this.SetOnUnregister value world = World.setScreenOnUnregister value this world
        member this.OnUnregister = Lens.make Property? OnUnregister this.GetOnUnregister this.SetOnUnregister this
        member this.GetOnUpdate world = World.getScreenOnUpdate this world
        member this.SetOnUpdate value world = World.setScreenOnUpdate value this world
        member this.OnUpdate = Lens.make Property? OnUpdate this.GetOnUpdate this.SetOnUpdate this
        member this.GetOnPostUpdate world = World.getScreenOnPostUpdate this world
        member this.SetOnPostUpdate value world = World.setScreenOnPostUpdate value this world
        member this.OnPostUpdate = Lens.make Property? OnPostUpdate this.GetOnPostUpdate this.SetOnPostUpdate this
        member this.GetOnSignal world = World.getScreenOnSignal this world
        member this.SetOnSignal value world = World.setScreenOnSignal value this world
        member this.OnSignal = Lens.make Property? OnSignal this.GetOnSignal this.SetOnSignal this
        member internal this.GetEntityTree world = World.getScreenEntityTree this world
        member internal this.SetEntityTreeNoEvent value world = World.setScreenEntityTreeNoEvent value this world
        member internal this.EntityTree = Lens.makeReadOnly Property? EntityTree this.GetEntityTree this
        member this.GetTransitionState world = World.getScreenTransitionState this world
        member this.SetTransitionState value world = World.setScreenTransitionState value this world
        member this.TransitionState = Lens.make Property? TransitionState this.GetTransitionState this.SetTransitionState this
        member this.GetTransitionTicks world = World.getScreenTransitionTicks this world
        member this.SetTransitionTicks value world = World.setScreenTransitionTicks value this world
        member this.TransitionTicks = Lens.make Property? TransitionTicks this.GetTransitionTicks this.SetTransitionTicks this
        member this.GetIncoming world = World.getScreenIncoming this world
        member this.SetIncoming value world = World.setScreenIncoming value this world
        member this.Incoming = Lens.make Property? Incoming this.GetIncoming this.SetIncoming this
        member this.GetOutgoing world = World.getScreenOutgoing this world
        member this.SetOutgoing value world = World.setScreenOutgoing value this world
        member this.Outgoing = Lens.make Property? Outgoing this.GetOutgoing this.SetOutgoing this
        member this.ChangeEvent propertyName = Events.Change propertyName --> this
        member this.RegisterEvent = Events.Register --> this
        member this.UnregisteringEvent = Events.Unregistering --> this
        member this.UpdateEvent = Events.Update --> this
        member this.PostUpdateEvent = Events.PostUpdate --> this
        member this.SignalEvent = Events.Signal --> this
        member this.SelectEvent = Events.Select --> this
        member this.DeselectEvent = Events.Deselect --> this
        member this.IncomingStartEvent = Events.IncomingStart --> this
        member this.IncomingFinishEvent = Events.IncomingFinish --> this
        member this.OutgoingStartEvent = Events.OutgoingStart --> this
        member this.OutgoingFinishEvent = Events.OutgoingFinish --> this

        /// Try to get a property value and type.
        member this.TryGetProperty propertyName world = World.tryGetScreenProperty propertyName this world

        /// Get a property value and type.
        member this.GetProperty propertyName world = World.getScreenProperty propertyName this world

        /// Get a property value.
        member this.Get<'a> propertyName world : 'a = (World.getScreenProperty propertyName this world).PropertyValue :?> 'a

        /// Try to set a property value with explicit type.
        member this.TrySetProperty propertyName property world = World.trySetScreenProperty propertyName property this world

        /// Set a property value with explicit type.
        member this.SetProperty propertyName property world = World.setScreenProperty propertyName property this world

        /// Set a property value.
        member this.Set<'a> propertyName (value : 'a) world = World.setScreenProperty propertyName { PropertyType = typeof<'a>; PropertyValue = value } this world

        /// Check that a screen is in an idling state (not transitioning in nor out).
        member this.IsIdling world = this.GetTransitionState world = IdlingState

        /// Check that a screen is selected.
        member this.GetSelected world =
            match (World.getGameState world).OmniScreenOpt with
            | Some omniScreen when Address.head this.ScreenAddress = Address.head omniScreen.ScreenAddress -> true
            | _ ->
                match (World.getGameState world).SelectedScreenOpt with
                | Some screen when Address.head this.ScreenAddress = Address.head screen.ScreenAddress -> true
                | _ -> false

        /// Check that a screen exists in the world.
        member this.GetExists world = World.getScreenExists this world

        /// Check that a screen dispatches in the same manner as the dispatcher with the given type.
        member this.DispatchesAs (dispatcherType, world) = Reflection.dispatchesAs dispatcherType (this.GetDispatcher world)

        /// Check that a screen dispatches in the same manner as the dispatcher with the given type.
        member this.DispatchesAs<'a> world = this.DispatchesAs (typeof<'a>, world)

        /// Resolve a relation in the context of a screen.
        member this.Resolve relation = Screen (Relation.resolve this.ScreenAddress relation)

        /// Get a screen's change event address.
        member this.GetChangeEvent propertyName = Events.Change propertyName --> this.ScreenAddress

        /// Send a signal to the screen.
        member this.Signal signal world = World.signalScreen signal this world

    type World with

        static member internal updateScreen (screen : Screen) world =
            World.withEventContext (fun world ->
                
                // update via dispatcher
                let dispatcher = World.getScreenDispatcher screen world
                let world = dispatcher.Update (screen, world)

                // run script update
                let world = World.evalWithLogging (screen.GetOnUpdate world) (screen.GetScriptFrame world) screen world |> snd'

                // publish update event
                let eventTrace = EventTrace.record "World" "updateScreen" EventTrace.empty
                World.publishPlus World.sortSubscriptionsByHierarchy () (Events.Update --> screen) eventTrace Default.Game true world)
                screen
                world

        static member internal postUpdateScreen (screen : Screen) world =
            World.withEventContext (fun world ->
                
                // post-update via dispatcher
                let dispatcher = World.getScreenDispatcher screen world
                let world = dispatcher.PostUpdate (screen, world)

                // run script post-update
                let world = World.evalWithLogging (screen.GetOnPostUpdate world) (screen.GetScriptFrame world) screen world |> snd'
                
                // publish post-update event
                let eventTrace = EventTrace.record "World" "postUpdateScreen" EventTrace.empty
                World.publishPlus World.sortSubscriptionsByHierarchy () (Events.PostUpdate --> screen) eventTrace Default.Game true world)
                screen
                world

        static member internal actualizeScreen (screen : Screen) world =
            World.withEventContext (fun world ->
                let dispatcher = screen.GetDispatcher world
                dispatcher.Actualize (screen, world))
                screen
                world

        /// Get all the world's screens.
        [<FunctionBinding>]
        static member getScreens world =
            World.getScreenDirectory world |>
            UMap.fold (fun state _ screenDirectory -> Screen screenDirectory.Key :: state) [] :>
            _ seq

        /// Set the dissolve properties of a screen.
        [<FunctionBinding>]
        static member setScreenDissolve dissolveData (screen : Screen) world =
            let dissolveImageOpt = Some dissolveData.DissolveImage
            let world = screen.SetIncoming { Transition.make Incoming with TransitionLifetime = dissolveData.IncomingTime; DissolveImageOpt = dissolveImageOpt } world
            let world = screen.SetOutgoing { Transition.make Outgoing with TransitionLifetime = dissolveData.OutgoingTime; DissolveImageOpt = dissolveImageOpt } world
            world

        /// Destroy a screen in the world immediately. Can be dangerous if existing in-flight publishing depends on the
        /// screen's existence. Consider using World.destroyScreen instead.
        static member destroyScreenImmediate screen world =
            let destroyLayersImmediate screen world =
                let layers = World.getLayers screen world
                World.destroyLayersImmediate layers world
            World.removeScreen3 destroyLayersImmediate screen world

        /// Destroy a screen in the world at the end of the current update.
        [<FunctionBinding>]
        static member destroyScreen screen world =
            World.schedule2 (World.destroyScreenImmediate screen) world

        /// Create a screen and add it to the world.
        [<FunctionBinding "createScreen">]
        static member createScreen3 dispatcherName nameOpt world =
            let dispatchers = World.getScreenDispatchers world
            let dispatcher =
                match Map.tryFind dispatcherName dispatchers with
                | Some dispatcher -> dispatcher
                | None -> failwith ("Could not find ScreenDispatcher '" + dispatcherName + "'. Did you forget to expose this dispatcher from your NuPlugin?")
            let screenState = ScreenState.make nameOpt dispatcher
            let screenState = Reflection.attachProperties ScreenState.copy screenState.Dispatcher screenState world
            let screen = ntos screenState.Name
            let world =
                if World.getScreenExists screen world then
                    Log.debug "Scheduling screen creation assuming existing screen at the same address is being destroyed."
                    World.schedule2 (World.addScreen false screenState screen) world
                else World.addScreen false screenState screen world
            (screen, world)

        /// Create a screen and add it to the world.
        static member createScreen<'d when 'd :> ScreenDispatcher> nameOpt world =
            World.createScreen3 typeof<'d>.Name nameOpt world

        /// Create a screen with a dissolving transition, and add it to the world.
        [<FunctionBinding "createDissolveScreen">]
        static member createDissolveScreen5 dispatcherName nameOpt dissolveData world =
            let (screen, world) = World.createScreen3 dispatcherName nameOpt world
            let world = World.setScreenDissolve dissolveData screen world
            (screen, world)
        
        /// Create a screen with a dissolving transition, and add it to the world.
        static member createDissolveScreen<'d when 'd :> ScreenDispatcher> nameOpt dissolveData world =
            World.createDissolveScreen5 typeof<'d>.Name nameOpt dissolveData world

        /// Write a screen to a screen descriptor.
        static member writeScreen screen screenDescriptor world =
            let writeLayers screen screenDescriptor world =
                let layers = World.getLayers screen world
                World.writeLayers layers screenDescriptor world
            World.writeScreen4 writeLayers screen screenDescriptor world

        /// Write multiple screens to a game descriptor.
        static member writeScreens screens gameDescriptor world =
            screens |>
            Seq.sortBy (fun (screen : Screen) -> screen.GetCreationTimeStamp world) |>
            Seq.filter (fun (screen : Screen) -> screen.GetPersistent world) |>
            Seq.fold (fun screenDescriptors screen -> World.writeScreen screen ScreenDescriptor.empty world :: screenDescriptors) gameDescriptor.Screens |>
            fun screenDescriptors -> { gameDescriptor with Screens = screenDescriptors }

        /// Write a screen to a file.
        [<FunctionBinding>]
        static member writeScreenToFile (filePath : string) screen world =
            let filePathTmp = filePath + ".tmp"
            let prettyPrinter = (SyntaxAttribute.getOrDefault typeof<GameDescriptor>).PrettyPrinter
            let screenDescriptor = World.writeScreen screen ScreenDescriptor.empty world
            let screenDescriptorStr = scstring screenDescriptor
            let screenDescriptorPretty = PrettyPrinter.prettyPrint screenDescriptorStr prettyPrinter
            File.WriteAllText (filePathTmp, screenDescriptorPretty)
            File.Delete filePath
            File.Move (filePathTmp, filePath)

        /// Read a screen from a screen descriptor.
        static member readScreen screenDescriptor nameOpt world =
            World.readScreen4 World.readLayers screenDescriptor nameOpt world

        /// Read multiple screens from a game descriptor.
        static member readScreens gameDescriptor world =
            List.foldBack
                (fun screenDescriptor (screens, world) ->
                    let screenNameOpt = ScreenDescriptor.getNameOpt screenDescriptor
                    let (screen, world) = World.readScreen screenDescriptor screenNameOpt world
                    (screen :: screens, world))
                gameDescriptor.Screens
                ([], world)

        /// Read a screen from a file.
        [<FunctionBinding>]
        static member readScreenFromFile (filePath : string) nameOpt world =
            let screenDescriptorStr = File.ReadAllText filePath
            let screenDescriptor = scvalue<ScreenDescriptor> screenDescriptorStr
            World.readScreen screenDescriptor nameOpt world

        /// Apply a screen behavior to a screen.
        static member applyScreenBehavior setScreenSplash behavior screen world =
            match behavior with
            | Vanilla -> (screen, world)
            | OmniScreen -> (screen, World.setOmniScreen screen world)
            | Dissolve dissolveData -> (screen, World.setScreenDissolve dissolveData screen world)
            | Splash (dissolveData, splashData, destination) ->
                let world = World.setScreenDissolve dissolveData screen world
                let world = setScreenSplash (Some splashData) destination screen world
                (screen, world)

        /// Turn screen content into a screen.
        static member expandScreenContent setScreenSplash content game world =
            match ScreenContent.expand content game world with
            | Left (name, descriptor, equations, behavior, layerStreams, entityStreams, layerFilePaths, entityFilePaths, entityContents) ->
                let (screen, world) = World.readScreen descriptor (Some name) world
                let world =
                    List.fold (fun world (_ : string, layerName, filePath) ->
                        World.readLayerFromFile filePath (Some layerName) screen world |> snd)
                        world layerFilePaths
                let world =
                    List.fold (fun world (_ : string, layerName, entityName, filePath) ->
                        World.readEntityFromFile filePath (Some entityName) (screen / layerName) world |> snd)
                        world entityFilePaths
                let world =
                    List.fold (fun world (name, simulant, property, breaking) ->
                        WorldModule.equate5 name simulant property breaking world)
                        world equations
                let world =
                    List.fold (fun world (screen, lens, mapper) ->
                        World.expandLayerStream lens mapper screen world)
                        world layerStreams
                let world =
                    List.fold (fun world (layer, lens, mapper) ->
                        World.expandEntityStream lens mapper None layer world)
                        world entityStreams
                let world =
                    List.fold (fun world (owner, entityContents) ->
                        let layer = etol owner
                        List.fold (fun world entityContent ->
                            World.expandEntityContent (Some (makeGuid ())) entityContent (Some owner) layer world)
                            world entityContents)
                        world entityContents
                World.applyScreenBehavior setScreenSplash behavior screen world
            | Right (name, behavior, Some dispatcherType, layerFilePath) ->
                let (screen, world) = World.createScreen3 dispatcherType.Name (Some name) world
                let world = World.readLayerFromFile layerFilePath None screen world |> snd
                World.applyScreenBehavior setScreenSplash behavior screen world
            | Right (name, behavior, None, filePath) ->
                let (screen, world) = World.readScreenFromFile filePath (Some name) world
                World.applyScreenBehavior setScreenSplash behavior screen world

namespace Debug
open Nu
type Screen =

    /// Provides a full view of all the member properties of a screen. Useful for debugging such
    /// as with the Watch feature in Visual Studio.
    static member view screen world = World.viewScreenProperties screen world