// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2017.

namespace Nu
open System
open System.IO
open Prime
open Nu

[<AutoOpen; ModuleBinding>]
module WorldGameModule =

    type Game with

        member this.GetId world = World.getGameId world
        member this.Id = PropertyTag.makeReadOnly this Property? Id this.GetId
        member this.GetDispatcherNp world = World.getGameDispatcherNp world
        member this.DispatcherNp = PropertyTag.makeReadOnly this Property? DispatcherNp this.GetDispatcherNp
        member this.GetCreationTimeStampNp world = World.getGameCreationTimeStampNp world
        member this.CreationTimeStampNp = PropertyTag.makeReadOnly this Property? CreationTimeStampNp this.GetCreationTimeStampNp
        member this.GetImperative world = World.getGameImperative world
        member this.Imperative = PropertyTag.makeReadOnly this Property? Imperative this.GetImperative
        member this.GetScriptOpt world = World.getGameScriptOpt world
        member this.SetScriptOpt value world = World.setGameScriptOpt value world
        member this.ScriptOpt = PropertyTag.make this Property? ScriptOpt this.GetScriptOpt this.SetScriptOpt
        member this.GetScript world = World.getGameScript world
        member this.SetScript value world = World.setGameScript value world
        member this.Script = PropertyTag.make this Property? Script this.GetScript this.SetScript
        member this.GetScriptFrameNp world = World.getGameScriptFrameNp world
        member this.ScriptFrameNp = PropertyTag.makeReadOnly this Property? Script this.GetScriptFrameNp
        member this.GetOnRegister world = World.getGameOnRegister world
        member this.SetOnRegister value world = World.setGameOnRegister value world
        member this.OnRegister = PropertyTag.make this Property? OnRegister this.GetOnRegister this.SetOnRegister
        member this.GetOnUnregister world = World.getGameOnUnregister world
        member this.SetOnUnregister value world = World.setGameOnUnregister value world
        member this.OnUnregister = PropertyTag.make this Property? OnUnregister this.GetOnUnregister this.SetOnUnregister
        member this.GetOnUpdate world = World.getGameOnUpdate world
        member this.SetOnUpdate value world = World.setGameOnUpdate value world
        member this.OnUpdate = PropertyTag.make this Property? OnUpdate this.GetOnUpdate this.SetOnUpdate
        member this.GetOnPostUpdate world = World.getGameOnPostUpdate world
        member this.SetOnPostUpdate value world = World.setGameOnPostUpdate value world
        member this.OnPostUpdate = PropertyTag.make this Property? OnPostUpdate this.GetOnPostUpdate this.SetOnPostUpdate
        member this.GetSelectedScreenOpt world = World.getSelectedScreenOpt world
        member this.SetSelectedScreenOpt value world = World.setSelectedScreenOpt value world
        member this.SelectedScreenOpt = PropertyTag.make this Property? SelectedScreenOpt this.GetSelectedScreenOpt this.SetSelectedScreenOpt
        member this.GetScreenTransitionDestinationOpt world = World.getScreenTransitionDestinationOpt world
        member this.SetScreenTransitionDestinationOpt value world = World.setScreenTransitionDestinationOpt value world
        member this.ScreenTransitionDestinationOpt = PropertyTag.make this Property? ScreenTransitionDestinationOpt this.GetScreenTransitionDestinationOpt this.SetScreenTransitionDestinationOpt
        member this.GetEyeCenter world = World.getEyeCenter world
        member this.SetEyeCenter value world = World.setEyeCenter value world
        member this.EyeCenter = PropertyTag.make this Property? EyeCenter this.GetEyeCenter this.SetEyeCenter
        member this.GetEyeSize world = World.getEyeSize world
        member this.SetEyeSize value world = World.setEyeSize value world
        member this.EyeSize = PropertyTag.make this Property? EyeSize this.GetEyeSize this.SetEyeSize

        /// Try to get a property value and type.
        member this.TryGetProperty propertyName world = World.tryGetGameProperty propertyName world

        /// Get a property value and type.
        member this.GetProperty propertyName world = World.getGameProperty propertyName world

        /// Get a property value.
        member this.Get<'a> propertyName world : 'a = (World.getGameProperty propertyName world).PropertyValue :?> 'a

        /// Try to set a property value with explicit type.
        member this.TrySetProperty propertyName property world = World.trySetGameProperty propertyName property world

        /// Set a property value with explicit type.
        member this.SetProperty propertyName property world = World.setGameProperty propertyName property world

        /// Set a property value.
        member this.Set<'a> propertyName (value : 'a) world = World.setGameProperty propertyName { PropertyType = typeof<'a>; PropertyValue = value } world

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

        /// Check that the given bounds is within the eye's sight.
        member this.GetInView viewType bounds world = World.isBoundsInView viewType bounds world

        /// Transform the given mouse position to screen space.
        member this.MouseToScreen mousePosition world = World.mouseToScreen mousePosition world

        /// Transform the given mouse position to world space.
        member this.MouseToWorld viewType mousePosition world = World.mouseToWorld viewType mousePosition world

        /// Transform the given mouse position to entity space.
        member this.MouseToEntity viewType entityPosition mousePosition world = World.mouseToEntity viewType entityPosition mousePosition world

        /// Check that a layer dispatches in the same manner as the dispatcher with the target type.
        member this.DispatchesAs (dispatcherTargetType : Type) world = Reflection.dispatchesAs dispatcherTargetType (this.GetDispatcherNp world)

        /// Resolve a relation in the context of an entity.
        member this.Resolve relation = Game (Relation.resolve this.GameAddress relation)

    type World with

        static member private gameOnRegisterChanged _ world =
            let world = World.unregisterGame world
            World.registerGame world

        static member private gameScriptOptChanged evt world =
            let game = evt.Subscriber : Game
            match game.GetScriptOpt world with
            | Some script ->
                match World.assetTagToValueOpt<Scripting.Expr array> true script world with
                | (Some script, world) -> game.SetScript script world
                | (None, world) -> world
            | None -> world

        static member internal registerGame world =
            let game = Simulants.Game
            let world = World.monitor World.gameOnRegisterChanged (Events.GameChange Property? OnRegister) game world
            let world = World.monitor World.gameScriptOptChanged (Events.GameChange Property? ScriptOpt) game world
            let world =
                World.withEventContext (fun world ->
                    let dispatcher = game.GetDispatcherNp world
                    let world = dispatcher.Register (game, world)
                    World.eval (game.GetOnUnregister world) (game.GetScriptFrameNp world) game world |> snd)
                    game
                    world
            World.choose world

        static member internal unregisterGame world =
            let game = Simulants.Game
            let world =
                World.withEventContext (fun world ->
                    let dispatcher = game.GetDispatcherNp world
                    let world = World.eval (game.GetOnRegister world) (game.GetScriptFrameNp world)game world |> snd
                    dispatcher.Unregister (game, world))
                    game
                    world
            World.choose world

        static member internal updateGame world =
            let game = Simulants.Game
            World.withEventContext (fun world ->

                // update via dispatcher
                let dispatcher = game.GetDispatcherNp world
                let world = dispatcher.Update (game, world)

                // run script update
                let world = World.evalWithLogging (game.GetOnUpdate world) (game.GetScriptFrameNp world) game world |> snd

                // publish update event
                let eventTrace = EventTrace.record "World" "updateGame" EventTrace.empty
                let world = World.publishPlus World.sortSubscriptionsByHierarchy () Events.Update eventTrace game true world
                World.choose world)
                game
                world

        static member internal postUpdateGame world =
            let game = Simulants.Game
            World.withEventContext (fun world ->
                
                // post-update via dispatcher
                let dispatcher = game.GetDispatcherNp world
                let world = dispatcher.PostUpdate (game, world)

                // run script post-update
                let world = World.evalWithLogging (game.GetOnPostUpdate world) (game.GetScriptFrameNp world) game world |> snd

                // publish post-update event
                let eventTrace = EventTrace.record "World" "postUpdateGame" EventTrace.empty
                let world = World.publishPlus World.sortSubscriptionsByHierarchy () Events.PostUpdate eventTrace game true world
                World.choose world)
                game
                world

        static member internal actualizeGame world =
            let game = Simulants.Game
            World.withEventContext (fun world ->
                let dispatcher = game.GetDispatcherNp world
                let world = dispatcher.Actualize (game, world)
                World.choose world)
                game
                world

        // Get all the entities in the world.
        [<FunctionBinding>]
        static member getEntities1 world =
            World.getLayers1 world |>
            Seq.map (fun layer -> World.getEntities layer world) |>
            Seq.concat

        // Get all the layers in the world.
        [<FunctionBinding>]
        static member getLayers1 world =
            World.getScreens world |>
            Seq.map (fun screen -> World.getLayers screen world) |>
            Seq.concat

        /// Determine if a simulant is contained by, or is the same as, the currently selected screen.
        /// Game is always considered 'selected' as well.
        [<FunctionBinding>]
        static member isSimulantSelected (simulant : Simulant) world =
            match Address.getNames simulant.SimulantAddress with
            | [] -> true
            | screenName :: _ ->
                match World.getSelectedScreenOpt world with
                | Some screen -> Address.getName screen.ScreenAddress = screenName
                | None -> false

        /// Write a game to a game descriptor.
        static member writeGame gameDescriptor world =
            let writeScreens gameDescriptor world =
                let screens = World.getScreens world
                World.writeScreens screens gameDescriptor world
            World.writeGame3 writeScreens gameDescriptor world

        /// Write a game to a file.
        [<FunctionBinding>]
        static member writeGameToFile (filePath : string) world =
            let filePathTmp = filePath + ".tmp"
            let prettyPrinter = (SyntaxAttribute.getOrDefault typeof<GameDescriptor>).PrettyPrinter
            let gameDescriptor = World.writeGame GameDescriptor.empty world
            let gameDescriptorStr = scstring gameDescriptor
            let gameDescriptorPretty = PrettyPrinter.prettyPrint gameDescriptorStr prettyPrinter
            File.WriteAllText (filePathTmp, gameDescriptorPretty)
            File.Delete filePath
            File.Move (filePathTmp, filePath)

        /// Read a game from a game descriptor.
        static member readGame gameDescriptor world =
            World.readGame3 World.readScreens gameDescriptor world

        /// Read a game from a file.
        [<FunctionBinding>]
        static member readGameFromFile (filePath : string) world =
            let gameDescriptorStr = File.ReadAllText filePath
            let gameDescriptor = scvalue<GameDescriptor> gameDescriptorStr
            World.readGame gameDescriptor world            

namespace Debug
open Nu
type Game =

    /// Provides a full view of all the properties of a game. Useful for debugging such as with the
    /// Watch feature in Visual Studio.
    static member view world = World.viewGameProperties world