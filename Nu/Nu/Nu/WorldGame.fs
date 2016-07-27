// Nu Game Engine.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Nu
open System
open System.Collections.Generic
open System.IO
open System.Reflection
open System.Runtime.CompilerServices
open OpenTK
open Prime
open Nu

[<AutoOpen>]
module WorldGameModule =

    type Game with

        member this.GetId world = World.getGameId world
        member this.GetXtension world = World.getGameXtension world
        member this.GetDispatcherNp world = World.getGameDispatcherNp world
        member this.GetCreationTimeStampNp world = World.getGameCreationTimeStampNp world
        member this.GetOptSpecialization world = World.getGameOptSpecialization world
        member this.GetOptSelectedScreen world = World.getGameOptSelectedScreen world
        member this.SetOptSelectedScreen value world = World.setGameOptSelectedScreen value world
        member this.GetOptScreenTransitionDestination world = World.getGameOptScreenTransitionDestination world
        member this.SetOptScreenTransitionDestination value world = World.setGameOptScreenTransitionDestination value world
        member this.GetEyeCenter world = World.getEyeCenter world
        member this.SetEyeCenter value world = World.setEyeCenter value world
        member this.GetEyeSize world = World.getEyeSize world
        member this.SetEyeSize value world = World.setEyeSize value world

        /// Get a property value and type.
        member this.GetProperty propertyName world = World.getGameProperty propertyName world

        /// Get a property value.
        member this.Get propertyName world : 'a = World.getGamePropertyValue propertyName world

        /// Set a property value.
        member this.Set propertyName (value : 'a) world = World.setGamePropertyValue propertyName value world

        /// Get the view of the eye in absolute terms (world space).
        member this.GetViewAbsolute (_ : World) = World.getViewAbsolute
        
        /// Get the view of the eye in absolute terms (world space) with translation sliced on
        /// integers.
        member this.GetViewAbsoluteI (_ : World) = World.getViewAbsoluteI

        /// The relative view of the eye with original single values. Due to the problems with
        /// SDL_RenderCopyEx as described in Math.fs, using this function to decide on sprite
        /// coordinates is very, very bad for rendering.
        member this.GetViewRelative world = World.getViewRelative world

        /// The relative view of the eye with translation sliced on integers. Good for rendering.
        member this.GetViewRelativeI world = World.getViewRelativeI world

        /// Get the bounds of the eye's sight relative to its position.
        member this.GetViewBoundsRelative world = World.getViewBoundsRelative world

        /// Get the bounds of the eye's sight not relative to its position.
        member this.GetViewBoundsAbsolute world = World.getViewAbsolute world

        /// Get the bounds of the eye's sight.
        member this.GetViewBounds viewType world = World.getViewBounds viewType world

        /// Query that the given bounds is within the eye's sight.
        member this.InView viewType bounds world = World.inView viewType bounds world

        /// Transform the given mouse position to screen space.
        member this.MouseToScreen mousePosition world = World.mouseToScreen mousePosition world

        /// Transform the given mouse position to world space.
        member this.MouseToWorld viewType mousePosition world = World.mouseToWorld viewType mousePosition world

        /// Transform the given mouse position to entity space.
        member this.MouseToEntity viewType entityPosition mousePosition world = World.mouseToEntity viewType entityPosition mousePosition world

        /// Query that a group dispatches in the same manner as the dispatcher with the target type.
        member this.DispatchesAs (dispatcherTargetType : Type) world = Reflection.dispatchesAs dispatcherTargetType (this.GetDispatcherNp world)

    type World with

        static member internal registerGame (world : World) : World =
            let dispatcher = Simulants.Game.GetDispatcherNp world
            dispatcher.Register (Simulants.Game, world)
        
        static member internal updateGame world =
            let dispatcher = Simulants.Game.GetDispatcherNp world
            let world = dispatcher.Update (Simulants.Game, world)
            let eventTrace = EventTrace.record "World" "updateGame" EventTrace.empty
            World.publish7 World.getSubscriptionsSorted World.sortSubscriptionsByHierarchy () Events.Update eventTrace Simulants.Game world
        
        static member internal actualizeGame world =
            let dispatcher = Simulants.Game.GetDispatcherNp world
            dispatcher.Actualize (Simulants.Game, world)

        // Get all the entities in the world.
        static member proxyEntities1 world =
            World.proxyGroups1 world |>
            Seq.map (fun group -> World.proxyEntities group world) |>
            Seq.concat

        // Get all the groups in the world.
        static member proxyGroups1 world =
            World.proxyScreens world |>
            Seq.map (fun screen -> World.proxyGroups screen world) |>
            Seq.concat

        /// Try to get the currently selected screen.
        static member getOptSelectedScreen world =
            Simulants.Game.GetOptSelectedScreen world

        /// Set the currently selected screen or None. Be careful using this function directly as
        /// you may be wanting to use the higher-level World.transitionScreen function instead.
        static member setOptSelectedScreen optScreen world =
            Simulants.Game.SetOptSelectedScreen optScreen world

        /// Get the currently selected screen (failing with an exception if there isn't one).
        static member getSelectedScreen world =
            Option.get ^ World.getOptSelectedScreen world
        
        /// Set the currently selected screen. Be careful using this function directly as you may
        /// be wanting to use the higher-level World.transitionScreen function instead.
        static member setSelectedScreen screen world =
            World.setOptSelectedScreen (Some screen) world

        /// Determine if an entity is selected by being in a group of the currently selected screeen.
        static member isEntitySelected entity world =
            let screenName = Address.head entity.EntityAddress
            match World.getOptSelectedScreen world with
            | Some selectedScreen -> screenName = selectedScreen.ScreenName
            | None -> false

        /// Determine if a group is selected by being in the currently selected screeen.
        static member isGroupSelected group world =
            let screenName = Address.head group.GroupAddress
            match World.getOptSelectedScreen world with
            | Some selectedScreen -> screenName = selectedScreen.ScreenName
            | None -> false

        /// Determine if a screen is the currently selected screeen.
        static member isScreenSelected screen world =
            World.getOptSelectedScreen world = Some screen

        /// Determine if a simulant is contained by, or is the same as, the currently selected screen.
        /// Game is always considered 'selected' as well.
        static member isSimulantSelected (simulant : Simulant) world =
            match Address.getNames simulant.SimulantAddress with
            | [] -> true
            | screenName :: _ ->
                match World.getOptSelectedScreen world with
                | Some screen -> screen.ScreenName = screenName
                | None -> false

        /// Write a game to a game descriptor.
        static member writeGame gameDescriptor world =
            let writeScreens gameDescriptor world =
                let screens = World.proxyScreens world
                World.writeScreens screens gameDescriptor world
            World.writeGame3 writeScreens gameDescriptor world

        /// Write a game to a file.
        static member writeGameToFile (filePath : string) world =
            let filePathTmp = filePath + ".tmp"
            let gameDescriptor = World.writeGame GameDescriptor.empty world
            let gameDescriptorStr = scstring gameDescriptor
            let gameDescriptorPretty = Symbol.prettyPrint String.Empty gameDescriptorStr
            File.WriteAllText (filePathTmp, gameDescriptorPretty)
            File.Delete filePath
            File.Move (filePathTmp, filePath)

        /// Read a game from a game descriptor.
        static member readGame gameDescriptor world =
            World.readGame3 World.readScreens gameDescriptor world

        /// Read a game from a file.
        static member readGameFromFile (filePath : string) world =
            let gameDescriptorStr = File.ReadAllText filePath
            let gameDescriptor = scvalue<GameDescriptor> gameDescriptorStr
            World.readGame gameDescriptor world

namespace Debug
open Prime
open Nu
open System.Reflection
open System.Collections.Generic
type Game =

    /// Provides a view of all the member properties of a game. Useful for debugging such as with
    /// the Watch feature in Visual Studio.
    static member viewMemberProperties world = World.viewGameMemberProperties world

    /// Provides a view of all the xtension properties of a game. Useful for debugging such as
    /// with the Watch feature in Visual Studio.
    static member viewXProperties world = World.viewGameXProperties world

    /// Provides a full view of all the member values of a game. Useful for debugging such
    /// as with the Watch feature in Visual Studio.
    static member view world = World.viewGame world