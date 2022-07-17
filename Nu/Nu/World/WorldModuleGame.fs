// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu
open System
open System.Collections.Generic
open System.Numerics
open Prime
open Nu

[<AutoOpen; ModuleBinding>]
module WorldModuleGame =

    /// Dynamic property getters / setters.
    let internal GameGetters = Dictionary<string, World -> Property> StringComparer.Ordinal
    let internal GameSetters = Dictionary<string, Property -> World -> struct (bool * World)> StringComparer.Ordinal

    type World with

        static member private publishGameChange propertyName (propertyValue : obj) world =

            // the game reference
            let game = Game ()

            // publish change binding
            let world =
                World.publishChangeBinding propertyName game world

            // publish change event
            let world =
                let changeData = { Name = propertyName; Value = propertyValue }
                let changeEventAddress = rtoa<ChangeData> [|"Change"; propertyName; "Event"|]
                let eventTrace = EventTrace.debug "World" "publishGameChange" "" EventTrace.empty
                World.publishPlus changeData changeEventAddress eventTrace game false false world

            // fin
            world

        static member internal getGameState world =
            world.GameState

        static member internal setGameState gameState world =
            World.choose { world with GameState = gameState }

        static member private updateGameStateWithoutEvent updater world =
            let gameStateOpt = updater (World.getGameState world)
            match gameStateOpt :> obj with
            | null -> struct (false, world)
            | _ -> struct (true, World.setGameState gameStateOpt world)

        static member private updateGameState updater propertyName propertyValue world =
            let struct (changed, world) = World.updateGameStateWithoutEvent updater world
            let world =
                if changed
                then World.publishGameChange propertyName propertyValue world
                else world
            struct (changed, world)

        static member internal getGameId world = (World.getGameState world).Id
        static member internal getGameOrder world = (World.getGameState world).Order
        static member internal getGameDispatcher world = (World.getGameState world).Dispatcher
        static member internal getGameModelProperty world = (World.getGameState world).Model
        static member internal getGameModel<'a> world = (World.getGameState world).Model.DesignerValue :?> 'a
        static member internal getGameScriptFrame world = (World.getGameState world).ScriptFrame
        static member internal setGameScriptFrame value world = World.updateGameState (fun gameState -> if value <> gameState.ScriptFrame then { gameState with ScriptFrame = value } else Unchecked.defaultof<_>) Property? ScriptFrame value world

        static member internal setGameModelProperty (value : DesignerProperty) world =
            World.updateGameState
                (fun gameState ->
                    if value.DesignerValue =/= gameState.Model.DesignerValue
                    then { gameState with Model = { gameState.Model with DesignerValue = value.DesignerValue }}
                    else Unchecked.defaultof<_>)
                Property? Model value.DesignerValue world

        static member internal setGameModel<'a> (value : 'a) world =
            World.updateGameState
                (fun gameState ->
                    let valueObj = value :> obj
                    if valueObj =/= gameState.Model.DesignerValue
                    then { gameState with Model = { DesignerType = typeof<'a>; DesignerValue = valueObj }}
                    else Unchecked.defaultof<_>)
                Property? Model value world

        /// Get the current 2d eye position.
        [<FunctionBinding>]
        static member getEye2dPosition world =
            (World.getGameState world).Eye2dPosition

        /// Set the current 2d eye position.
        static member internal setEye2dPositionPlus value world =
            World.updateGameState (fun gameState -> if v2Neq value gameState.Eye2dPosition then { gameState with Eye2dPosition = value } else Unchecked.defaultof<_>) Property? Eye2dPosition value world

        /// Set the current 2d eye position.
        [<FunctionBinding>]
        static member setEye2dPosition value world =
            World.setEye2dPositionPlus value world |> snd'

        /// Get the current 2d eye size.
        [<FunctionBinding>]
        static member getEye2dSize world =
            (World.getGameState world).Eye2dSize

        /// Set the current 2d eye size.
        static member internal setEye2dSizePlus value world =
            World.updateGameState (fun gameState -> if v2Neq value gameState.Eye2dSize then { gameState with Eye2dSize = value } else Unchecked.defaultof<_>) Property? Eye2dSize value world

        /// Set the current 2d eye size.
        [<FunctionBinding>]
        static member setEye2dSize value world =
            World.setEye2dSizePlus value world |> snd'

        /// Get the current 2d eye bounds.
        [<FunctionBinding>]
        static member getEyeBounds2d world =
            let eyePosition = World.getEye2dPosition world
            let eyeSize = World.getEye2dSize world
            box2 (eyePosition - eyeSize * 0.5f) eyeSize

        /// Get the current 3d eye position.
        [<FunctionBinding>]
        static member getEye3dPosition world =
            (World.getGameState world).Eye3dPosition

        /// Set the current 3d eye position.
        static member internal setEye3dPositionPlus (value : Vector3) world =
            World.updateGameState (fun gameState ->
                if v3Neq value gameState.Eye3dPosition then
                    let viewport = Constants.Render.Viewport
                    { gameState with
                        Eye3dPosition = value
                        EyeFrustumEnclosed3d = viewport.Frustum (Constants.Render.NearPlaneDistance, Constants.Render.FarPlaneDistanceEnclosed, value, gameState.Eye3dRotation)
                        EyeFrustumUnenclosed3d = viewport.Frustum (Constants.Render.NearPlaneDistance, Constants.Render.FarPlaneDistanceUnenclosed, value, gameState.Eye3dRotation)
                        EyeFrustumProminent3d = viewport.Frustum (Constants.Render.NearPlaneDistance, Constants.Render.FarPlaneDistanceProminent, value, gameState.Eye3dRotation) }
                else Unchecked.defaultof<_>) Property? Eye3dPosition value world

        /// Set the current 3d eye position.
        [<FunctionBinding>]
        static member setEye3dPosition value world =
            World.setEye3dPositionPlus value world |> snd'

        /// Get the current 3d eye rotation.
        [<FunctionBinding>]
        static member getEye3dRotation world =
            (World.getGameState world).Eye3dRotation

        /// Set the current 3d eye rotation.
        static member internal setEye3dRotationPlus value world =
            World.updateGameState (fun gameState ->
                if quatNeq value gameState.Eye3dRotation then
                    let viewport = Constants.Render.Viewport
                    { gameState with
                        Eye3dRotation = value
                        EyeFrustumEnclosed3d = viewport.Frustum (Constants.Render.NearPlaneDistance, Constants.Render.FarPlaneDistanceEnclosed, gameState.Eye3dPosition, value)
                        EyeFrustumUnenclosed3d = viewport.Frustum (Constants.Render.NearPlaneDistance, Constants.Render.FarPlaneDistanceUnenclosed, gameState.Eye3dPosition, value)
                        EyeFrustumProminent3d = viewport.Frustum (Constants.Render.NearPlaneDistance, Constants.Render.FarPlaneDistanceProminent, gameState.Eye3dPosition, value) }
                else Unchecked.defaultof<_>) Property? Eye3dRotation value world

        /// Set the current 3d eye rotation.
        [<FunctionBinding>]
        static member setEye3dRotation value world =
            World.setEye3dRotationPlus value world |> snd'

        /// Get the current enclosed 3d eye frustum.
        [<FunctionBinding>]
        static member getEye3dFrustumEnclosed world =
            (World.getGameState world).EyeFrustumEnclosed3d

        /// Get the current unenclosed 3d eye frustum.
        [<FunctionBinding>]
        static member getEye3dFrustumUnenclosed world =
            (World.getGameState world).EyeFrustumUnenclosed3d

        /// Get the current prominent 3d eye frustum.
        [<FunctionBinding>]
        static member getEye3dFrustumProminent world =
            (World.getGameState world).EyeFrustumProminent3d

        /// Get the current 3d light box.
        [<FunctionBinding>]
        static member getLightbox3d world =
            let lightBoxSize = Constants.Render.LightBox3dSize
            box3 ((World.getGameState world).Eye3dPosition - lightBoxSize * 0.5f) lightBoxSize

        /// Get the omni-screen, if any.
        [<FunctionBinding>]
        static member getOmniScreenOpt world =
            (World.getGameState world).OmniScreenOpt
        
        /// Set the omni-screen or None.
        static member internal setOmniScreenOptPlus value world =
            if Option.isSome value && World.getSelectedScreenOpt world = value then failwith "Cannot set OmniScreen to SelectedScreen."
            World.updateGameState (fun gameState -> if value <> gameState.OmniScreenOpt then { gameState with OmniScreenOpt = value } else Unchecked.defaultof<_>) Property? OmniScreenOpt value world

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

        /// Get the currently selected screen, if any.
        [<FunctionBinding>]
        static member getSelectedScreenOpt world =
            (World.getGameState world).SelectedScreenOpt

        /// Constrain the eye to the given 2d bounds.
        [<FunctionBinding>]
        static member constrainEyeBounds2d (bounds : Box2) world =
            let mutable eyeBounds = World.getEyeBounds2d world
            eyeBounds.Position <-
                v2
                    (if eyeBounds.Position.X < bounds.Position.X then bounds.Position.X
                        elif eyeBounds.Right.X > bounds.Right.X then bounds.Right.X - eyeBounds.Size.X
                        else eyeBounds.Position.X)
                    (if eyeBounds.Position.Y < bounds.Position.Y then bounds.Position.Y
                        elif eyeBounds.Top.Y > bounds.Top.Y then bounds.Top.Y - eyeBounds.Size.Y
                        else eyeBounds.Position.Y)
            let eyePosition = eyeBounds.Center
            World.setEye2dPosition eyePosition world

        /// Set the currently selected screen or None. Be careful using this function directly as
        /// you may be wanting to use the higher-level World.transitionScreen function instead.
        static member internal setSelectedScreenOptPlus value world =

            // disallow omni-screen selection
            if  Option.isSome value &&
                World.getOmniScreenOpt world = value then
                failwith "Cannot set SelectedScreen to OmniScreen."

            // raise change event for none selection
            let struct (_, world) = World.updateGameState id Property? SelectedScreenOpt None world

            // clear out singleton states
            let world =
                match (World.getGameState world).SelectedScreenOpt with
                | Some screen ->
                    let world = WorldModule.unregisterScreenPhysics screen world
                    let world = WorldModule.evictScreenElements screen world
                    world
                | None -> world
                
            // actually set selected screen (no events)
            let struct (_, world) =
                World.updateGameStateWithoutEvent
                    (fun gameState ->
                        if value <> gameState.SelectedScreenOpt
                        then { gameState with SelectedScreenOpt = value }
                        else Unchecked.defaultof<_>)
                    world

            // set selected ecs opt
            let world =
                match value with
                | Some screen -> World.setSelectedEcsOpt (Some (WorldModule.getScreenEcs screen world)) world
                | None -> World.setSelectedEcsOpt None world

            // handle some case
            match value with
            | Some screen ->

                // populate singleton states
                let world = WorldModule.admitScreenElements screen world
                let world = WorldModule.registerScreenPhysics screen world

                // raise change event for some selection
                let world = World.updateGameState id Property? SelectedScreenOpt (Some screen) world |> snd'
                (true, world)

            // fin
            | None -> (true, world)

        /// Set the currently selected screen or None. Be careful using this function directly as
        /// you may be wanting to use the higher-level World.transitionScreen function instead.
        static member internal setSelectedScreenOpt value world =
            World.setSelectedScreenOptPlus value world |> snd

        /// Get the currently selected screen (failing with an exception if there isn't one).
        [<FunctionBinding>]
        static member getSelectedScreen world =
            Option.get (World.getSelectedScreenOpt world)
            
        /// Set the currently selected screen. Be careful using this function directly as you may
        /// be wanting to use the higher-level World.transitionScreen function instead.
        static member internal setSelectedScreenPlus value world =
            World.setSelectedScreenOptPlus (Some value) world
        
        /// Set the currently selected screen. Be careful using this function directly as you may
        /// be wanting to use the higher-level World.transitionScreen function instead.
        [<FunctionBinding>]
        static member setSelectedScreen value world =
            World.setSelectedScreenPlus value world |> snd

        /// Get the current destination screen if a screen transition is currently underway.
        static member getScreenTransitionDestinationOpt world =
            (World.getGameState world).ScreenTransitionDestinationOpt

        /// Get the screens desired to which to transition.
        static member getDesiredScreenOpt world =
            (World.getGameState world).DesiredScreenOpt

        /// Set the screen desired to which to transition.
        static member setDesiredScreenOpt screens world =
            World.updateGameState
                (fun gameState ->
                    if screens <> gameState.DesiredScreenOpt
                    then { gameState with DesiredScreenOpt = screens }
                    else Unchecked.defaultof<_>)
                Property? DesiredScreenOpt
                screens
                world

        /// Set the current destination screen or None. Be careful using this function as calling
        /// it is predicated that no screen transition is currently underway.
        /// TODO: consider asserting such predication here.
        static member internal setScreenTransitionDestinationOpt destination world =
            World.updateGameState
                (fun gameState ->
                    if destination <> gameState.ScreenTransitionDestinationOpt
                    then { gameState with ScreenTransitionDestinationOpt = destination }
                    else Unchecked.defaultof<_>)
                Property? ScreenTransitionDestinationOpt
                destination
                world

        /// Get the bounds of the 2d eye's sight irrespective of its position.
        [<FunctionBinding>]
        static member getViewBoundsAbsolute2d world =
            let gameState = World.getGameState world
            box2
                (v2 (gameState.Eye2dSize.X * -0.5f) (gameState.Eye2dSize.Y * -0.5f))
                (v2 gameState.Eye2dSize.X gameState.Eye2dSize.Y)

        /// Get the bounds of the 2d play zone irrespective of eye position.
        [<FunctionBinding>]
        static member getPlayBoundsAbsolute2d world =
            World.getViewBounds2d world

        /// Get the bounds of the 2d eye's sight relative to its position.
        [<FunctionBinding>]
        static member getViewBounds2d world =
            let gameState = World.getGameState world
            box2
                (v2 (gameState.Eye2dPosition.X - gameState.Eye2dSize.X * 0.5f) (gameState.Eye2dPosition.Y - gameState.Eye2dSize.Y * 0.5f))
                (v2 gameState.Eye2dSize.X gameState.Eye2dSize.Y)

        /// Get the bounds of the 2d play zone.
        [<FunctionBinding>]
        static member getPlayBounds2d world =
            World.getViewBounds2d world

        /// Check that the given bounds is within the 2d eye's sight.
        [<FunctionBinding>]
        static member isBoundsInView2d (bounds : Box2) world =
            let viewBounds = World.getViewBounds2d world
            bounds.Intersects viewBounds

        /// Get the view bounds of the 3d eye's sight.
        [<FunctionBinding>]
        static member getViewBounds3d presence world =
            match presence with
            | Enclosed -> struct (World.getEye3dFrustumEnclosed world, World.getLightbox3d world)
            | Unenclosed -> struct (World.getEye3dFrustumUnenclosed world, World.getLightbox3d world)
            | Prominent -> struct (World.getEye3dFrustumProminent world, World.getLightbox3d world)
            | Omnipresent -> failwith "Cannot get the view bounds of an Omnipresent thing."

        /// Get the bounds of the 3d play zone.
        [<FunctionBinding>]
        static member getPlayBounds3d world =
            let eyePosition = World.getEye3dPosition world
            let eyeBox = box3 (eyePosition - Constants.Render.PlayBox3dSize * 0.5f) Constants.Render.PlayBox3dSize
            let eyeFrustum = World.getEye3dFrustumEnclosed world
            struct (eyeBox, eyeFrustum)

        /// Check that the given bounds is within the 3d eye's sight.
        [<FunctionBinding>]
        static member isBoundsInView3d light presence (bounds : Box3) world =
            let struct (frustum, lightBox) = World.getViewBounds3d presence world
            if light && lightBox.Intersects bounds then true
            else
                let containment = frustum.Contains bounds
                containment = ContainmentType.Contains ||
                containment = ContainmentType.Intersects

        /// Check that the given bounds is within the 3d eye's play bounds.
        [<FunctionBinding>]
        static member isBoundsInPlay3d (bounds : Box3) world =
            let struct (viewBox, viewFrustum) = World.getPlayBounds3d world
            if bounds.Intersects viewBox then true
            else
                let containment = viewFrustum.Contains bounds
                containment = ContainmentType.Contains ||
                containment = ContainmentType.Intersects

        /// Fetch an asset with the given tag and convert it to a value of type 'a.
        static member assetTagToValueOpt<'a> assetTag metadata world =
            match World.tryFindSymbol assetTag metadata world with
            | Some symbol ->
                try let script = symbolToValue<'a> symbol in Some script
                with exn -> Log.debug ("Failed to convert symbol '" + scstring symbol + "' to value due to: " + scstring exn); None
            | None -> None

        /// Fetch assets with the given tags and convert it to values of type 'a.
        static member assetTagsToValueOpts<'a> assetTags metadata world =
            List.map (fun assetTag -> World.assetTagToValueOpt<'a> assetTag metadata world) assetTags

        static member internal tryGetGameXtensionProperty (propertyName, world, property : _ outref) =
            GameState.tryGetProperty (propertyName, World.getGameState world, &property)

        static member internal getGameXtensionProperty propertyName world =
            let mutable property = Unchecked.defaultof<_>
            match GameState.tryGetProperty (propertyName, World.getGameState world, &property) with
            | true -> property
            | false -> failwithf "Could not find property '%s'." propertyName

        static member internal getGameXtensionValue<'a> propertyName world =
            let gameState = World.getGameState world
            let property = GameState.getProperty propertyName gameState
            property.PropertyValue :?> 'a

        static member internal tryGetGameProperty (propertyName, world, property : _ outref) =
            match GameGetters.TryGetValue propertyName with
            | (true, getter) ->
                property <- getter world
                true
            | (false, _) ->
                World.tryGetGameXtensionProperty (propertyName, world, &property)

        static member internal getGameProperty propertyName world =
            match GameGetters.TryGetValue propertyName with
            | (true, getter) -> getter world
            | (false, _) -> World.getGameXtensionProperty propertyName world

        static member internal trySetGameXtensionPropertyFast propertyName property world =
            let mutable success = false // bit of a hack to get additional state out of the lambda
            let struct (_, world) =
                World.updateGameState
                    (fun gameState ->
                        let mutable propertyOld = Unchecked.defaultof<_>
                        match GameState.tryGetProperty (propertyName, gameState, &propertyOld) with
                        | true ->
                            if property.PropertyValue =/= propertyOld.PropertyValue then
                                let struct (successInner, gameState) = GameState.trySetProperty propertyName property gameState
                                success <- successInner
                                gameState
                            else Unchecked.defaultof<_>
                        | false -> Unchecked.defaultof<_>)
                    propertyName property.PropertyValue world
            world

        static member internal trySetGameXtensionProperty propertyName property world =
            let mutable success = false // bit of a hack to get additional state out of the lambda
            let struct (changed, world) =
                World.updateGameState
                    (fun gameState ->
                        let mutable propertyOld = Unchecked.defaultof<_>
                        match GameState.tryGetProperty (propertyName, gameState, &propertyOld) with
                        | true ->
                            if property.PropertyValue =/= propertyOld.PropertyValue then
                                let struct (successInner, gameState) = GameState.trySetProperty propertyName property gameState
                                success <- successInner
                                gameState
                            else Unchecked.defaultof<_>
                        | false -> Unchecked.defaultof<_>)
                    propertyName property.PropertyValue world
            struct (success, changed, world)

        static member internal setGameXtensionProperty propertyName property world =
            World.updateGameState
                (fun gameState ->
                    let propertyOld = GameState.getProperty propertyName gameState
                    if property.PropertyValue =/= propertyOld.PropertyValue
                    then GameState.setProperty propertyName property gameState
                    else Unchecked.defaultof<_>)
                propertyName property.PropertyValue world

        static member internal trySetGamePropertyFast propertyName property world =
            match GameSetters.TryGetValue propertyName with
            | (true, setter) -> setter property world |> snd'
            | (false, _) -> World.trySetGameXtensionPropertyFast propertyName property world

        static member internal trySetGameProperty propertyName property world =
            match GameSetters.TryGetValue propertyName with
            | (true, setter) ->
                let struct (changed, world) = setter property world
                struct (true, changed, world)
            | (false, _) ->
                World.trySetGameXtensionProperty propertyName property world

        static member internal setGameProperty propertyName property world =
            match GameSetters.TryGetValue propertyName with
            | (true, setter) -> setter property world
            | (false, _) -> World.setGameXtensionProperty propertyName property world

        static member internal attachGameProperty propertyName property world =
            let struct (_, world) =
                World.updateGameState
                    (fun gameState -> GameState.attachProperty propertyName property gameState)
                    propertyName property.PropertyValue world
            world

        static member internal detachGameProperty propertyName world =
            let struct (_, world) =
                World.updateGameStateWithoutEvent
                    (fun gameState -> GameState.detachProperty propertyName gameState)
                    world
            world

        /// View all of the properties of a game.
        static member internal viewGameProperties world =
            let state = World.getGameState world
            World.viewProperties state

    /// Initialize property getters.
    let private initGetters () =
        GameGetters.Add ("Dispatcher", fun world -> { PropertyType = typeof<GameDispatcher>; PropertyValue = World.getGameDispatcher world })
        GameGetters.Add ("Model", fun world -> let designerProperty = World.getGameModelProperty world in { PropertyType = designerProperty.DesignerType; PropertyValue = designerProperty.DesignerValue })
        GameGetters.Add ("OmniScreenOpt", fun world -> { PropertyType = typeof<Screen option>; PropertyValue = World.getOmniScreenOpt world })
        GameGetters.Add ("SelectedScreenOpt", fun world -> { PropertyType = typeof<Screen option>; PropertyValue = World.getSelectedScreenOpt world })
        GameGetters.Add ("DesiredScreenOpt", fun world -> { PropertyType = typeof<Screen option>; PropertyValue = World.getDesiredScreenOpt world })
        GameGetters.Add ("Eye2dPosition", fun world -> { PropertyType = typeof<Vector2>; PropertyValue = World.getEye2dPosition world })
        GameGetters.Add ("Eye2dSize", fun world -> { PropertyType = typeof<Vector2>; PropertyValue = World.getEye2dSize world })
        GameGetters.Add ("Eye3dPosition", fun world -> { PropertyType = typeof<Vector3>; PropertyValue = World.getEye3dPosition world })
        GameGetters.Add ("Eye3dRotation", fun world -> { PropertyType = typeof<Quaternion>; PropertyValue = World.getEye3dRotation world })
        GameGetters.Add ("ScriptFrame", fun world -> { PropertyType = typeof<Scripting.ProceduralFrame list>; PropertyValue = World.getGameScriptFrame world })
        GameGetters.Add ("Order", fun world -> { PropertyType = typeof<int64>; PropertyValue = World.getGameOrder world })
        GameGetters.Add ("Id", fun world -> { PropertyType = typeof<Guid>; PropertyValue = World.getGameId world })

    /// Initialize property setters.
    let private initSetters () =
        GameSetters.Add ("Model", fun property world -> World.setGameModelProperty { DesignerType = property.PropertyType; DesignerValue = property.PropertyValue } world)
        GameSetters.Add ("OmniScreenOpt", fun property world -> World.setOmniScreenOptPlus (property.PropertyValue :?> Screen option) world)
        GameSetters.Add ("DesiredScreenOpt", fun property world -> World.setDesiredScreenOpt (property.PropertyValue :?> Screen option) world)
        GameSetters.Add ("Eye2dPosition", fun property world -> World.setEye2dPositionPlus (property.PropertyValue :?> Vector2) world)
        GameSetters.Add ("Eye2dSize", fun property world -> World.setEye2dSizePlus (property.PropertyValue :?> Vector2) world)
        GameSetters.Add ("Eye3dPosition", fun property world -> World.setEye3dPositionPlus (property.PropertyValue :?> Vector3) world)
        GameSetters.Add ("Eye3dRotation", fun property world -> World.setEye3dRotationPlus (property.PropertyValue :?> Quaternion) world)

    /// Initialize getters and setters
    let internal init () =
        initGetters ()
        initSetters ()