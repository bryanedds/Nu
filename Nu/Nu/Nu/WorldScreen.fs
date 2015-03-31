// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2015.

namespace Nu
open System
open System.Collections.Generic
open System.IO
open System.Reflection
open System.Runtime.CompilerServices
open System.Xml
open OpenTK
open Prime
open Nu
open Nu.Constants
open Nu.WorldConstants

[<AutoOpen>]
module WorldScreenModule =

    type Screen with

        member this.GetId world = (World.getScreenState this world).Id
        member this.GetName world = (World.getScreenState this world).Name
        member this.GetCreationTimeStampNp world = (World.getScreenState this world).CreationTimeStampNp
        member this.GetDispatcherNp world = (World.getScreenState this world).DispatcherNp
        member this.GetTransitionStateNp world = (World.getScreenState this world).TransitionStateNp
        member this.SetTransitionStateNp value world = World.updateScreenState (fun screenState -> { screenState with TransitionStateNp = value }) this world
        member this.GetTransitionTicksNp world = (World.getScreenState this world).TransitionTicksNp
        member this.SetTransitionTicksNp value world = World.updateScreenState (fun screenState -> { screenState with TransitionTicksNp = value }) this world
        member this.GetIncoming world = (World.getScreenState this world).Incoming
        member this.SetIncoming value world = World.updateScreenState (fun screenState -> { screenState with Incoming = value }) this world
        member this.GetOutgoing world = (World.getScreenState this world).Outgoing
        member this.SetOutgoing value world = World.updateScreenState (fun screenState -> { screenState with Outgoing = value }) this world
        member this.GetPublishChanges world = (World.getScreenState this world).PublishChanges
        member this.SetPublishChanges value world = World.updateScreenState (fun (screenState : ScreenState) -> { screenState with PublishChanges = value }) this world
        member this.GetPersistent world = (World.getScreenState this world).Persistent
        member this.SetPersistent value world = World.updateScreenState (fun screenState -> { screenState with Persistent = value }) this world
        member this.GetXtension world = (World.getScreenState this world).Xtension
        member this.UpdateXtension updater world = World.updateScreenState (fun screenState -> { screenState with Xtension = updater screenState.Xtension}) this world

        /// Get an xtension field by name.
        member this.GetXField name world =
            let xtension = this.GetXtension world
            let xField = Map.find name xtension.XFields
            xField.FieldValue

        /// Query that a screen is in an idling state (not transitioning in nor out).
        member this.IsIdling world =
            this.GetTransitionStateNp world = IdlingState

        /// Query that a screen dispatches in the same manner as the dispatcher with the target type.
        member this.DispatchesAs (dispatcherTargetType : Type) world =
            Reflection.dispatchesAs dispatcherTargetType (this.GetDispatcherNp world)

    type World with

        static member private registerScreen (screen : Screen) world =
            let dispatcher = screen.GetDispatcherNp world : ScreenDispatcher
            dispatcher.Register screen world

        static member private unregisterScreen (screen : Screen) world =
            let dispatcher = screen.GetDispatcherNp world : ScreenDispatcher
            dispatcher.Unregister screen world

        static member internal addScreen mayReplace screenState screen world =
            let isNew = not <| World.containsScreen screen world
            if isNew || mayReplace then
                let world = World.setScreenStateWithoutEvent screenState screen world
                if isNew then
                    let world = World.registerScreen screen world
                    World.publish4 () (ScreenAddEventAddress ->>- screen.ScreenAddress) screen world
                else world
            else failwith <| "Adding a screen that the world already contains at address '" + acstring screen.ScreenAddress + "'."

        /// Query that the world contains a screen.
        static member containsScreen screen world =
            Option.isSome <| World.getOptScreenState screen world

        /// Get all the world's screens.
        static member proxyScreens world =
            World.getScreenStateMap world |>
                Map.fold (fun screensRev screenName _ -> (Screen.proxy <| ntoa screenName) :: screensRev) [] |>
                List.rev

        /// Destroy a screen in the world immediately. Can be dangerous if existing in-flight
        /// publishing depends on the screen's existence. Use with caution.
        static member destroyScreenImmediate screen world =
            let world = World.publish4 () (ScreenRemovingEventAddress ->>- screen.ScreenAddress) screen world
            if World.containsScreen screen world then
                let world = World.unregisterScreen screen world
                let groups = World.proxyGroups screen world
                let world = World.destroyGroupsImmediate groups world
                World.setOptScreenStateWithoutEvent None screen world
            else world

        /// Destroy a screen in the world on the next tick. Use this rather than
        /// destroyScreenImmediate unless you need the latter's specific behavior.
        static member destroyScreen screen world =
            let task =
                { ScheduledTime = World.getTickTime world
                  Operation = fun world -> World.destroyScreenImmediate screen world }
            World.addTask task world

        /// Create a screen and add it to the world.
        static member createScreen dispatcherName optName world =
            let dispatcher = Map.find dispatcherName world.Components.ScreenDispatchers
            let screenState = ScreenState.make dispatcher optName
            Reflection.attachFields dispatcher screenState
            let screen = Screen.proxy <| ntoa screenState.Name
            let world = World.addScreen false screenState screen world
            (screen, world)
        
        /// Create a screen with a dissolving transition, and add it to the world.
        static member createDissolveScreen dissolveData dispatcherName optName world =
            let optDissolveImage = Some dissolveData.DissolveImage
            let (screen, world) = World.createScreen dispatcherName optName world
            let world = screen.SetIncoming { TransitionDescriptor.make Incoming with TransitionLifetime = dissolveData.IncomingTime; OptDissolveImage = optDissolveImage } world
            let world = screen.SetOutgoing { TransitionDescriptor.make Outgoing with TransitionLifetime = dissolveData.OutgoingTime; OptDissolveImage = optDissolveImage } world
            (screen, world)

        /// Write a screen to an xml writer.
        static member writeScreen (writer : XmlWriter) screen world =
            let screenState = World.getScreenState screen world
            let groups = World.proxyGroups screen world
            writer.WriteAttributeString (DispatcherNameAttributeName, Reflection.getTypeName screenState.DispatcherNp)
            Reflection.writeMemberValuesFromTarget tautology3 writer screenState
            writer.WriteStartElement GroupsNodeName
            World.writeGroups writer groups world
            writer.WriteEndElement ()

        /// Write a screen to an xml file.
        static member writeScreenToFile (filePath : string) screen world =
            let filePathTmp = filePath + ".tmp"
            let writerSettings = XmlWriterSettings ()
            writerSettings.Indent <- true
            use writer = XmlWriter.Create (filePathTmp, writerSettings)
            writer.WriteStartElement RootNodeName
            writer.WriteStartElement ScreenNodeName
            World.writeScreen writer screen world
            writer.WriteEndElement ()
            writer.WriteEndElement ()
            writer.Dispose ()
            File.Delete filePath
            File.Move (filePathTmp, filePath)

        /// Write multiple screens to an xml writer.
        static member writeScreens (writer : XmlWriter) screens world =
            let screensSorted = Seq.sortBy (fun (screen : Screen) -> screen.GetCreationTimeStampNp world) screens
            let screensPersistent = Seq.filter (fun (screen : Screen) -> screen.GetPersistent world) screensSorted
            for screen in screensPersistent do
                writer.WriteStartElement ScreenNodeName
                World.writeScreen writer screen world
                writer.WriteEndElement ()

        /// Read a screen from an xml node.
        static member readScreen (screenNode : XmlNode) defaultDispatcherName defaultGroupDispatcherName defaultEntityDispatcherName optName world =
            let dispatcherName = Reflection.readDispatcherName defaultDispatcherName screenNode
            let dispatcher =
                match Map.tryFind dispatcherName world.Components.ScreenDispatchers with
                | Some dispatcher -> dispatcher
                | None ->
                    note <| "Could not locate dispatcher '" + dispatcherName + "'."
                    let dispatcherName = typeof<ScreenDispatcher>.Name
                    Map.find dispatcherName world.Components.ScreenDispatchers
            let screenState = ScreenState.make dispatcher None
            Reflection.attachFields screenState.DispatcherNp screenState
            Reflection.readMemberValuesToTarget screenNode screenState
            let screenState = match optName with Some name -> { screenState with Name = name } | None -> screenState
            let screen = Screen.proxy <| ntoa screenState.Name
            let world = World.addScreen true screenState screen world
            let world = snd <| World.readGroups (screenNode : XmlNode) defaultGroupDispatcherName defaultEntityDispatcherName screen world
            (screen, world)

        /// Read a screen from an xml file.
        static member readScreenFromFile (filePath : string) optName world =
            use reader = XmlReader.Create filePath
            let document = let emptyDoc = XmlDocument () in (emptyDoc.Load reader; emptyDoc)
            let rootNode = document.[RootNodeName]
            let screenNode = rootNode.[ScreenNodeName]
            World.readScreen screenNode typeof<ScreenDispatcher>.Name typeof<GroupDispatcher>.Name typeof<EntityDispatcher>.Name optName world

        /// Read multiple screens from an xml node.
        static member readScreens (gameNode : XmlNode) defaultDispatcherName defaultGroupDispatcherName defaultEntityDispatcherName world =
            match gameNode.SelectSingleNode ScreensNodeName with
            | null -> ([], world)
            | screensNode ->
                let (screensRev, world) =
                    Seq.fold
                        (fun (screens, world) screenNode ->
                            let (screen, world) = World.readScreen screenNode defaultDispatcherName defaultGroupDispatcherName defaultEntityDispatcherName None world
                            (screen :: screens, world))
                        ([], world)
                        (enumerable <| screensNode.SelectNodes ScreenNodeName)
                (List.rev screensRev, world)

namespace Debug
open Prime
open Nu
open System.Reflection
type Screen =

    /// Provides a view of all the properties of a screen. Useful for debugging such as with
    /// the Watch feature in Visual Studio.
    static member viewProperties screen world =
        let state = World.getScreenState screen world
        let properties = Array.map (fun (property : PropertyInfo) -> (property.Name, property.GetValue state)) ((state.GetType ()).GetProperties ())
        Map.ofSeq properties
        
    /// Provides a view of all the xtension fields of a screen. Useful for debugging such as
    /// with the Watch feature in Visual Studio.
    static member viewXFields screen world =
        let state = World.getScreenState screen world
        Map.map (fun _ field -> field.FieldValue) state.Xtension.XFields

    /// Provides a full view of all the member values of a screen. Useful for debugging such
    /// as with the Watch feature in Visual Studio.
    static member view screen world = Screen.viewProperties screen world @@ Screen.viewXFields screen world

    /// Provides a partitioned view of all the member values of a screen. Useful for debugging
    /// such as with the Watch feature in Visual Studio.
    static member peek screen world = Watchable (Screen.viewProperties screen world, Screen.viewXFields screen world)