// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu
open System
open System.Collections.Generic
open System.Numerics
open Prime
open Nu

[<AutoOpen; ModuleBinding>]
module WorldModuleGame =

    /// Dynamic property getters / setters.
    let internal GameGetters = Dictionary<string, Game -> World -> Property> StringComparer.Ordinal
    let internal GameSetters = Dictionary<string, Property -> Game -> World -> struct (bool * World)> StringComparer.Ordinal

    type World with

        static member private publishGameChange propertyName (propertyPrevious : obj) (propertyValue : obj) game world =
            let changeData = { Name = propertyName; Previous = propertyPrevious; Value = propertyValue }
            let changeEventAddress = rtoa<ChangeData> [|Constants.Lens.ChangeName; propertyName; Constants.Lens.EventName; Constants.Engine.GameName|]
            let eventTrace = EventTrace.debug "World" "publishGameChange" "" EventTrace.empty
            World.publishPlus changeData changeEventAddress eventTrace game false false world

        static member internal getGameState game world =
            ignore<Game> game
            world.GameState

        static member internal setGameState gameState game world =
            ignore<Game> game
            World.choose { world with GameState = gameState }

        static member internal getGameId game world = (World.getGameState game world).Id
        static member internal getGameOrder game world = (World.getGameState game world).Order
        static member internal getGameDispatcher game world = (World.getGameState game world).Dispatcher
        static member internal getGameModelProperty game world = (World.getGameState game world).Model
        static member internal getGameContent game world = (World.getGameState game world).Content
        static member internal getGameScriptFrame game world = (World.getGameState game world).ScriptFrame

        static member internal setGameModelProperty initializing (value : DesignerProperty) game world =
            let gameState = World.getGameState game world
            let previous = gameState.Model
            if value.DesignerValue =/= previous.DesignerValue || initializing then
                let struct (gameState, world) =
                    let gameState = { gameState with Model = { DesignerType = value.DesignerType; DesignerValue = value.DesignerValue }}
                    struct (gameState, World.setGameState gameState game world)
                let world = gameState.Dispatcher.TrySynchronize (initializing, game, world)
                let world =
                    if initializing then
                        let content = World.getGameContent game world
                        let desiredScreen =
                            match Seq.tryHead content.ScreenContents with
                            | Some screen -> Desire (game / screen.Key)
                            | None -> DesireNone
                        World.setGameDesiredScreenPlus desiredScreen game world |> snd'
                    else world
                let world = World.publishGameChange Constants.Engine.ModelPropertyName previous.DesignerValue value.DesignerValue game world
                struct (true, world)
            else struct (false, world)

        static member internal getGameModel<'a> game world =
            let gameState = World.getGameState game world
            match gameState.Model.DesignerValue with
            | :? 'a as model -> model
            | null -> null :> obj :?> 'a
            | modelObj ->
                try modelObj |> valueToSymbol |> symbolToValue
                with _ ->
                    Log.debugOnce "Could not convert existing model to new type. Falling back on initial model value."
                    match gameState.Dispatcher.TryGetInitialModelValue<'a> world with
                    | None -> failwithnie ()
                    | Some value -> value

        static member internal setGameModel<'a> initializing (value : 'a) (game : Game) world =
            let gameState = World.getGameState game world
            let valueObj = value :> obj
            let previous = gameState.Model
            if valueObj =/= previous.DesignerValue || initializing then
                let struct (gameState, world) =
                    let gameState = { gameState with Model = { DesignerType = typeof<'a>; DesignerValue = valueObj }}
                    struct (gameState, World.setGameState gameState game world)
                let world = gameState.Dispatcher.TrySynchronize (initializing, game, world)
                let world =
                    if initializing then
                        let content = World.getGameContent game world
                        let desiredScreen =
                            match Seq.tryHead content.ScreenContents with
                            | Some screen -> Desire (game / screen.Key)
                            | None -> DesireNone
                        World.setGameDesiredScreenPlus desiredScreen game world |> snd'
                    else world
                let world = World.publishGameChange Constants.Engine.ModelPropertyName previous.DesignerValue value game world
                struct (true, world)
            else struct (false, world)

        static member internal setGameContent (value : GameContent) game world =
            let gameState = World.getGameState game world
            let gameState = { gameState with Content = value}
            World.setGameState gameState game world

        static member internal setGameScriptFrame value game world =
            let gameState = World.getGameState game world
            let previous = gameState.ScriptFrame
            if value <> previous
            then struct (true, world |> World.setGameState { gameState with ScriptFrame = value } game |> World.publishGameChange (nameof gameState.ScriptFrame) previous value game)
            else struct (false, world)

        /// Get the current 2d eye center.
        [<FunctionBinding>]
        static member getEyeCenter2d world =
            (World.getGameState Game.Handle world).EyeCenter2d

        /// Set the current 2d eye center.
        static member internal setEyeCenter2dPlus value world =
            let gameState = World.getGameState Game.Handle world
            let previous = gameState.EyeCenter2d
            if v2Neq previous value
            then struct (true, world |> World.setGameState { gameState with EyeCenter2d = value } Game.Handle |> World.publishGameChange (nameof gameState.EyeCenter2d) previous value Game.Handle)
            else struct (false, world)

        /// Set the current 2d eye center.
        [<FunctionBinding>]
        static member setEyeCenter2d value world =
            World.setEyeCenter2dPlus value world |> snd'

        /// Get the current 2d eye size.
        [<FunctionBinding>]
        static member getEyeSize2d world =
            (World.getGameState Game.Handle world).EyeSize2d

        /// Set the current 2d eye size.
        static member internal setEyeSize2dPlus value world =
            let gameState = World.getGameState Game.Handle world
            let previous = gameState.EyeSize2d
            if v2Neq previous value
            then struct (true, world |> World.setGameState { gameState with EyeSize2d = value } Game.Handle |> World.publishGameChange (nameof gameState.EyeSize2d) previous value Game.Handle)
            else struct (false, world)

        /// Set the current 2d eye size.
        [<FunctionBinding>]
        static member setEyeSize2d value world =
            World.setEyeSize2dPlus value world |> snd'

        /// Get the current 2d eye bounds.
        [<FunctionBinding>]
        static member getEyeBounds2d world =
            let eyeCenter = World.getEyeCenter2d world
            let eyeSize = World.getEyeSize2d world
            box2 (eyeCenter - eyeSize * 0.5f) eyeSize

        /// Get the current 3d eye center.
        [<FunctionBinding>]
        static member getEyeCenter3d world =
            (World.getGameState Game.Handle world).EyeCenter3d

        /// Set the current 3d eye center.
        static member internal setEyeCenter3dPlus (value : Vector3) world =
            let gameState = World.getGameState Game.Handle world
            let previous = gameState.EyeCenter3d
            if v3Neq previous value then
                let viewport = Constants.Render.Viewport
                let gameState =
                    { gameState with
                        EyeCenter3d = value
                        EyeFrustum3dEnclosed = viewport.Frustum (Constants.Render.NearPlaneDistanceEnclosed, Constants.Render.FarPlaneDistanceEnclosed, value, gameState.EyeRotation3d)
                        EyeFrustum3dExposed = viewport.Frustum (Constants.Render.NearPlaneDistanceExposed, Constants.Render.FarPlaneDistanceExposed, value, gameState.EyeRotation3d)
                        EyeFrustum3dImposter = viewport.Frustum (Constants.Render.NearPlaneDistanceImposter, Constants.Render.FarPlaneDistanceImposter, value, gameState.EyeRotation3d) }
                struct (true, world |> World.setGameState gameState Game.Handle |> World.publishGameChange (nameof gameState.EyeCenter3d) previous value Game.Handle)
            else struct (false, world)

        /// Set the current 3d eye center.
        [<FunctionBinding>]
        static member setEyeCenter3d value world =
            World.setEyeCenter3dPlus value world |> snd'

        /// Get the current 3d eye rotation.
        [<FunctionBinding>]
        static member getEyeRotation3d world =
            (World.getGameState Game.Handle world).EyeRotation3d

        /// Set the current 3d eye rotation.
        static member internal setEyeRotation3dPlus value world =
            let gameState = World.getGameState Game.Handle world
            let previous = gameState.EyeRotation3d
            if quatNeq previous value then
                let viewport = Constants.Render.Viewport
                let gameState =
                    { gameState with
                        EyeRotation3d = value
                        EyeFrustum3dEnclosed = viewport.Frustum (Constants.Render.NearPlaneDistanceEnclosed, Constants.Render.FarPlaneDistanceEnclosed, gameState.EyeCenter3d, value)
                        EyeFrustum3dExposed = viewport.Frustum (Constants.Render.NearPlaneDistanceExposed, Constants.Render.FarPlaneDistanceExposed, gameState.EyeCenter3d, value)
                        EyeFrustum3dImposter = viewport.Frustum (Constants.Render.NearPlaneDistanceImposter, Constants.Render.FarPlaneDistanceImposter, gameState.EyeCenter3d, value) }
                struct (true, world |> World.setGameState gameState Game.Handle |> World.publishGameChange (nameof gameState.EyeRotation3d) previous value Game.Handle)
            else struct (false, world)

        /// Set the current 3d eye rotation.
        [<FunctionBinding>]
        static member setEyeRotation3d value world =
            World.setEyeRotation3dPlus value world |> snd'

        /// Get the current enclosed 3d eye frustum.
        [<FunctionBinding>]
        static member getEyeFrustum3dEnclosed world =
            (World.getGameState Game.Handle world).EyeFrustum3dEnclosed

        /// Get the current unenclosed 3d eye frustum.
        [<FunctionBinding>]
        static member getEyeFrustum3dExposed world =
            (World.getGameState Game.Handle world).EyeFrustum3dExposed

        /// Get the current imposter 3d eye frustum.
        [<FunctionBinding>]
        static member getEyeFrustum3dImposter world =
            (World.getGameState Game.Handle world).EyeFrustum3dImposter

        /// Get the current 3d light box.
        [<FunctionBinding>]
        static member getLightBox3d world =
            let lightBoxSize = Constants.Render.LightBoxSize3d
            box3 ((World.getGameState Game.Handle world).EyeCenter3d - lightBoxSize * 0.5f) lightBoxSize

        /// Get the omni-screen, if any.
        [<FunctionBinding>]
        static member getOmniScreenOpt world =
            (World.getGameState Game.Handle world).OmniScreenOpt
        
        /// Set the omni-screen or None.
        static member internal setOmniScreenOptPlus value world =
            if Option.isSome value && World.getSelectedScreenOpt world = value then failwith "Cannot set OmniScreenOpt to [Some SelectedScreen]."
            let gameState = World.getGameState Game.Handle world
            let previous = gameState.OmniScreenOpt
            if value <> previous
            then struct (true, world |> World.setGameState { gameState with OmniScreenOpt = value } Game.Handle |> World.publishGameChange (nameof gameState.OmniScreenOpt) previous value Game.Handle)
            else struct (false, world)

        /// Set the omni-screen or None.
        [<FunctionBinding>]
        static member setOmniScreenOpt value world =
            World.setOmniScreenOptPlus value world |> snd'

        /// Get the omniScreen (failing with an exception if there isn't one).
        [<FunctionBinding>]
        static member getOmniScreen world =
            Option.get (World.getOmniScreenOpt world)

        /// Set the omniScreen.
        static member internal setOmniScreenPlus value world =
            World.setOmniScreenOptPlus (Some value) world
        
        /// Set the omniScreen.
        [<FunctionBinding>]
        static member setOmniScreen value world =
            World.setOmniScreenPlus value world |> snd'

        /// Constrain the eye to the given 2d bounds.
        [<FunctionBinding>]
        static member constrainEyeBounds2d (bounds : Box2) world =
            let mutable eyeBounds = World.getEyeBounds2d world
            eyeBounds.Min <-
                v2
                    (if eyeBounds.Min.X < bounds.Min.X then bounds.Min.X
                        elif eyeBounds.Right.X > bounds.Right.X then bounds.Right.X - eyeBounds.Size.X
                        else eyeBounds.Min.X)
                    (if eyeBounds.Min.Y < bounds.Min.Y then bounds.Min.Y
                        elif eyeBounds.Top.Y > bounds.Top.Y then bounds.Top.Y - eyeBounds.Size.Y
                        else eyeBounds.Min.Y)
            let eyeCenter = eyeBounds.Center
            World.setEyeCenter2d eyeCenter world

        /// Set the currently selected screen or None.
        static member internal setSelectedScreenOptPlus value world =

            // disallow omni-screen selection
            if  Option.isSome value &&
                World.getOmniScreenOpt world = value then
                failwith "Cannot set SelectedScreen to OmniScreen."

            // update game state if changed
            let gameState = World.getGameState Game.Handle world
            let previous = gameState.SelectedScreenOpt
            if value <> previous then

                // raise change event for None case
                let world =
                    match value with
                    | None -> World.publishGameChange (nameof gameState.SelectedScreenOpt) previous None Game.Handle world
                    | _ -> world

                // clear out singleton states
                let world =
                    match (World.getGameState Game.Handle world).SelectedScreenOpt with
                    | Some screen ->
                        let world = WorldModule.unregisterScreenPhysics screen world
                        let world = WorldModule.evictScreenElements screen world
                        world
                    | None -> world
                
                // actually set selected screen (no events)
                let gameState = World.getGameState Game.Handle world
                let gameState = { gameState with SelectedScreenOpt = value }
                let world = World.setGameState gameState Game.Handle world

                // set selected ecs opt
                let world =
                    match value with
                    | Some screen -> World.setSelectedEcsOpt (Some (WorldModule.getScreenEcs screen world)) world
                    | None -> World.setSelectedEcsOpt None world

                // raise change event for Some case
                match value with
                | Some screen ->

                    // populate singleton states
                    let world = WorldModule.admitScreenElements screen world
                    let world = WorldModule.registerScreenPhysics screen world

                    // raise change event for some selection
                    let world = World.publishGameChange (nameof gameState.SelectedScreenOpt) previous value Game.Handle world
                    (true, world)

                // fin
                | None -> (true, world)
            else (false, world)
            
        static member internal setSelectedScreenPlus value world =
            World.setSelectedScreenOptPlus (Some value) world

        /// Get the currently selected screen, if any.
        [<FunctionBinding>]
        static member getSelectedScreenOpt world =
            (World.getGameState Game.Handle world).SelectedScreenOpt

        /// Set the currently selected screen or None.
        static member setSelectedScreenOpt value world =
            World.setSelectedScreenOptPlus value world |> snd

        /// Get the currently selected screen (failing with an exception if there isn't one).
        [<FunctionBinding>]
        static member getSelectedScreen world =
            Option.get (World.getSelectedScreenOpt world)

        /// Set the currently selected screen.
        [<FunctionBinding>]
        static member setSelectedScreen value world =
            World.setSelectedScreenPlus value world |> snd

        static member internal setGameDesiredScreenPlus value game world =
            let gameState = World.getGameState Game.Handle world
            let previous = gameState.DesiredScreen
            if value <> previous
            then struct (true, world |> World.setGameState { gameState with DesiredScreen = value } game |> World.publishGameChange (nameof gameState.DesiredScreen) previous value game)
            else struct (false, world)

        static member internal getGameDesiredScreen game world =
            (World.getGameState game world).DesiredScreen

        static member internal setGameDesiredScreen value game world =
            World.setGameDesiredScreenPlus value game world |> snd'

        /// Get the current destination screen if a screen transition is currently underway.
        [<FunctionBinding>]
        static member getScreenTransitionDestinationOpt world =
            (World.getGameState Game.Handle world).ScreenTransitionDestinationOpt

        /// Set the current destination screen or None.
        [<FunctionBinding>]
        static member setScreenTransitionDestinationOpt value world =
            let gameState = World.getGameState Game.Handle world
            let previous = gameState.ScreenTransitionDestinationOpt
            if value <> previous
            then struct (true, world |> World.setGameState { gameState with ScreenTransitionDestinationOpt = value } Game.Handle |> World.publishGameChange (nameof gameState.ScreenTransitionDestinationOpt) previous value Game.Handle)
            else struct (false, world)

        /// Get the bounds of the 2d eye's sight irrespective of its position.
        [<FunctionBinding>]
        static member getViewBounds2dAbsolute world =
            let gameState = World.getGameState Game.Handle world
            box2
                (v2 (gameState.EyeSize2d.X * -0.5f) (gameState.EyeSize2d.Y * -0.5f))
                (v2 gameState.EyeSize2d.X gameState.EyeSize2d.Y)

        /// Get the bounds of the 2d play zone irrespective of eye center.
        [<FunctionBinding>]
        static member getPlayBounds2dAbsolute world =
            World.getViewBounds2d world

        /// Get the bounds of the 2d eye's sight relative to its position.
        [<FunctionBinding>]
        static member getViewBounds2d world =
            let gameState = World.getGameState Game.Handle world
            let min = v2 (gameState.EyeCenter2d.X - gameState.EyeSize2d.X * 0.5f) (gameState.EyeCenter2d.Y - gameState.EyeSize2d.Y * 0.5f)
            box2 min gameState.EyeSize2d

        /// Get the bounds of the 2d play zone.
        [<FunctionBinding>]
        static member getPlayBounds2d world =
            World.getViewBounds2d world

        /// Check that the given bounds is within the 2d eye's sight.
        [<FunctionBinding>]
        static member boundsInView2d (bounds : Box2) world =
            let viewBounds = World.getViewBounds2d world
            bounds.Intersects viewBounds

        /// Get the bounds of the 3d play zone.
        [<FunctionBinding>]
        static member getPlayBounds3d world =
            let eyeCenter = World.getEyeCenter3d world
            let eyeBox = box3 (eyeCenter - Constants.Render.PlayBoxSize3d * 0.5f) Constants.Render.PlayBoxSize3d
            let eyeFrustum = World.getEyeFrustum3dEnclosed world
            struct (eyeBox, eyeFrustum)

        /// Check that the given bounds is within the 3d eye's sight.
        [<FunctionBinding>]
        static member boundsInView3d lightProbe light presence (bounds : Box3) world =
            Presence.intersects3d
                (World.getEyeFrustum3dEnclosed world)
                (World.getEyeFrustum3dExposed world)
                (World.getEyeFrustum3dImposter world)
                (World.getLightBox3d world)
                lightProbe
                light
                bounds
                presence

        /// Check that the given bounds is within the 3d eye's play bounds.
        [<FunctionBinding>]
        static member boundsInPlay3d (bounds : Box3) world =
            let struct (viewBox, viewFrustum) = World.getPlayBounds3d world
            if bounds.Intersects viewBox then true
            else
                let containment = viewFrustum.Contains bounds
                containment = ContainmentType.Contains ||
                containment = ContainmentType.Intersects

        /// Fetch an asset with the given tag and convert it to a value of type 'a.
        static member assetTagToValueOpt<'a> assetTag metadata world =
            match World.tryGetSymbol assetTag metadata world with
            | Some symbol ->
                try let script = symbolToValue<'a> symbol in Some script
                with exn -> Log.debug ("Failed to convert symbol '" + scstring symbol + "' to value due to: " + scstring exn); None
            | None -> None

        /// Fetch assets with the given tags and convert it to values of type 'a.
        static member assetTagsToValueOpts<'a> assetTags metadata world =
            List.map (fun assetTag -> World.assetTagToValueOpt<'a> assetTag metadata world) assetTags

        static member internal tryGetGameXtensionProperty (propertyName, game, world, property : _ outref) =
            GameState.tryGetProperty (propertyName, World.getGameState game world, &property)

        static member internal tryGetGameXtensionValue<'a> propertyName game world =
            let gameState = World.getGameState game world
            let mutable property = Unchecked.defaultof<Property>
            if GameState.tryGetProperty (propertyName, gameState, &property) then
                match property.PropertyValue with
                | :? 'a as value -> value
                | null -> null :> obj :?> 'a
                | valueObj -> valueObj |> valueToSymbol |> symbolToValue
            else Unchecked.defaultof<'a>

        static member internal getGameXtensionProperty propertyName game world =
            let mutable property = Unchecked.defaultof<_>
            match GameState.tryGetProperty (propertyName, World.getGameState game world, &property) with
            | true -> property
            | false -> failwithf "Could not find property '%s'." propertyName

        static member internal getGameXtensionValue<'a> propertyName game world =
            let gameState = World.getGameState game world
            let property = GameState.getProperty propertyName gameState
            match property.PropertyValue with
            | :? 'a as value -> value
            | null -> null :> obj :?> 'a
            | valueObj -> valueObj |> valueToSymbol |> symbolToValue

        static member internal tryGetGameProperty (propertyName, game, world, property : _ outref) =
            match GameGetters.TryGetValue propertyName with
            | (true, getter) ->
                property <- getter game world
                true
            | (false, _) ->
                World.tryGetGameXtensionProperty (propertyName, game, world, &property)

        static member internal getGameProperty propertyName game world =
            match GameGetters.TryGetValue propertyName with
            | (true, getter) -> getter game world
            | (false, _) -> World.getGameXtensionProperty propertyName game world

        static member internal trySetGameXtensionPropertyFast propertyName (property : Property) game world =
            let gameState = World.getGameState game world
            match GameState.tryGetProperty (propertyName, gameState) with
            | (true, propertyOld) ->
                if property.PropertyValue =/= propertyOld.PropertyValue then
                    let struct (success, gameState) = GameState.trySetProperty propertyName property gameState
                    let world = World.setGameState gameState game world
                    if success then World.publishGameChange propertyName propertyOld.PropertyValue property.PropertyValue game world else world
                else world
            | (false, _) -> world

        static member internal trySetGameXtensionProperty propertyName (property : Property) game world =
            let gameState = World.getGameState game world
            match GameState.tryGetProperty (propertyName, gameState) with
            | (true, propertyOld) ->
                if property.PropertyValue =/= propertyOld.PropertyValue then
                    let struct (success, gameState) = GameState.trySetProperty propertyName property gameState
                    let world = World.setGameState gameState game world
                    if success
                    then struct (success, true, World.publishGameChange propertyName propertyOld.PropertyValue property.PropertyValue game world)
                    else struct (false, true, world)
                else struct (false, false, world)
            | (false, _) -> struct (false, false, world)

        static member internal setGameXtensionProperty propertyName (property : Property) game world =
            let gameState = World.getGameState game world
            let propertyOld = GameState.getProperty propertyName gameState
            if property.PropertyValue =/= propertyOld.PropertyValue then
                let gameState = GameState.setProperty propertyName property gameState
                let world = World.setGameState gameState game world
                struct (true, World.publishGameChange propertyName propertyOld.PropertyValue property.PropertyValue game world)
            else struct (false, world)

        static member internal trySetGamePropertyFast propertyName property game world =
            match GameSetters.TryGetValue propertyName with
            | (true, setter) -> setter property game world |> snd'
            | (false, _) -> World.trySetGameXtensionPropertyFast propertyName property game world

        static member internal trySetGameProperty propertyName property game world =
            match GameSetters.TryGetValue propertyName with
            | (true, setter) ->
                let struct (changed, world) = setter property game world
                struct (true, changed, world)
            | (false, _) ->
                World.trySetGameXtensionProperty propertyName property game world

        static member internal setGameProperty propertyName property game world =
            match GameSetters.TryGetValue propertyName with
            | (true, setter) -> setter property game world
            | (false, _) -> World.setGameXtensionProperty propertyName property game world

        static member internal attachGameProperty propertyName property game world =
            let gameState = World.getGameState game world
            let gameState = GameState.attachProperty propertyName property gameState
            let world = World.setGameState gameState game world
            World.publishGameChange propertyName property.PropertyValue property.PropertyValue game world

        static member internal detachGameProperty propertyName game world =
            let gameState = World.getGameState game world
            let gameState = GameState.detachProperty propertyName gameState
            World.setGameState gameState game world

        static member internal viewGameProperties game world =
            let state = World.getGameState game world
            World.viewSimulantStateProperties state

    /// Initialize property getters.
    let private initGetters () =
        GameGetters.Add ("Dispatcher", fun game world -> { PropertyType = typeof<GameDispatcher>; PropertyValue = World.getGameDispatcher game world })
        GameGetters.Add ("Model", fun game world -> let designerProperty = World.getGameModelProperty game world in { PropertyType = designerProperty.DesignerType; PropertyValue = designerProperty.DesignerValue })
        GameGetters.Add ("DesiredScreen", fun game world -> { PropertyType = typeof<DesiredScreen>; PropertyValue = World.getGameDesiredScreen game world })
        GameGetters.Add ("OmniScreenOpt", fun _ world -> { PropertyType = typeof<Screen option>; PropertyValue = World.getOmniScreenOpt world })
        GameGetters.Add ("SelectedScreenOpt", fun _ world -> { PropertyType = typeof<Screen option>; PropertyValue = World.getSelectedScreenOpt world })
        GameGetters.Add ("ScreenTransitionDestinationOpt", fun _ world -> { PropertyType = typeof<Screen option>; PropertyValue = World.getScreenTransitionDestinationOpt world })
        GameGetters.Add ("EyeCenter2d", fun _ world -> { PropertyType = typeof<Vector2>; PropertyValue = World.getEyeCenter2d world })
        GameGetters.Add ("EyeSize2d", fun _ world -> { PropertyType = typeof<Vector2>; PropertyValue = World.getEyeSize2d world })
        GameGetters.Add ("EyeCenter3d", fun _ world -> { PropertyType = typeof<Vector3>; PropertyValue = World.getEyeCenter3d world })
        GameGetters.Add ("EyeRotation3d", fun _ world -> { PropertyType = typeof<Quaternion>; PropertyValue = World.getEyeRotation3d world })
        GameGetters.Add ("EyeFrustum3dEnclosed", fun _ world -> { PropertyType = typeof<Frustum>; PropertyValue = World.getEyeFrustum3dEnclosed world })
        GameGetters.Add ("EyeFrustum3dExposed", fun _ world -> { PropertyType = typeof<Frustum>; PropertyValue = World.getEyeFrustum3dExposed world })
        GameGetters.Add ("EyeFrustum3dImposter", fun _ world -> { PropertyType = typeof<Frustum>; PropertyValue = World.getEyeFrustum3dImposter world })
        GameGetters.Add ("ScriptFrame", fun game world -> { PropertyType = typeof<Scripting.ProceduralFrame list>; PropertyValue = World.getGameScriptFrame game world })
        GameGetters.Add ("Order", fun game world -> { PropertyType = typeof<int64>; PropertyValue = World.getGameOrder game world })
        GameGetters.Add ("Id", fun game world -> { PropertyType = typeof<Guid>; PropertyValue = World.getGameId game world })

    /// Initialize property setters.
    let private initSetters () =
        GameSetters.Add ("Model", fun property game world -> World.setGameModelProperty false { DesignerType = property.PropertyType; DesignerValue = property.PropertyValue } game world)
        GameSetters.Add ("DesiredScreen", fun property game world -> World.setGameDesiredScreenPlus (property.PropertyValue :?> DesiredScreen) game world)
        GameSetters.Add ("OmniScreenOpt", fun property _ world -> World.setOmniScreenOptPlus (property.PropertyValue :?> Screen option) world)
        GameSetters.Add ("EyeCenter2d", fun property _ world -> World.setEyeCenter2dPlus (property.PropertyValue :?> Vector2) world)
        GameSetters.Add ("EyeSize2d", fun property _ world -> World.setEyeSize2dPlus (property.PropertyValue :?> Vector2) world)
        GameSetters.Add ("EyeCenter3d", fun property _ world -> World.setEyeCenter3dPlus (property.PropertyValue :?> Vector3) world)
        GameSetters.Add ("EyeRotation3d", fun property _ world -> World.setEyeRotation3dPlus (property.PropertyValue :?> Quaternion) world)

    /// Initialize getters and setters
    let internal init () =
        initGetters ()
        initSetters ()