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

        static member private publishGameChange propertyName (propertyValue : obj) (previousValue : obj) world =

            // the game reference
            let game = Game ()

            // publish change binding
            let world =
                World.publishChangeBinding propertyName game world

            // publish change event
            let world =
                let changeData = { Name = propertyName; Value = propertyValue; Previous = previousValue }
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

        static member private updateGameState updater propertyName propertyValue previousValue world =
            let struct (changed, world) = World.updateGameStateWithoutEvent updater world
            let world =
                if changed
                then World.publishGameChange propertyName propertyValue previousValue world
                else world
            struct (changed, world)

        static member internal getGameId world = (World.getGameState world).Id
        static member internal getGameOrder world = (World.getGameState world).Order
        static member internal getGameDispatcher world = (World.getGameState world).Dispatcher
        static member internal getGameModelProperty world = (World.getGameState world).Model
        static member internal getGameModel<'a> world = (World.getGameState world).Model.DesignerValue :?> 'a
        static member internal getGameScriptFrame world = (World.getGameState world).ScriptFrame
        static member internal setGameScriptFrame value world = World.updateGameState (fun gameState -> if value <> gameState.ScriptFrame then { gameState with ScriptFrame = value } else Unchecked.defaultof<_>) "ScriptFrame" value world

        static member internal setGameModelProperty (value : DesignerProperty) world =
            World.updateGameState
                (fun gameState ->
                    if value.DesignerValue =/= gameState.Model.DesignerValue
                    then { gameState with Model = { gameState.Model with DesignerValue = value.DesignerValue }}
                    else Unchecked.defaultof<_>)
                "Model" value.DesignerValue world

        static member internal setGameModel<'a> (value : 'a) world =
            World.updateGameState
                (fun gameState ->
                    let valueObj = value :> obj
                    if valueObj =/= gameState.Model.DesignerValue
                    then { gameState with Model = { DesignerType = typeof<'a>; DesignerValue = valueObj }}
                    else Unchecked.defaultof<_>)
                "Model" value world

        /// Get the current 2d eye position.
        [<FunctionBinding>]
        static member getEyePosition2d world =
            (World.getGameState world).EyePosition2d

        /// Set the current 2d eye position.
        static member internal setEyePosition2dPlus value world =
            let gameState = World.getGameState world
            let previous = gameState.EyePosition2d
            if v2Neq value previous
            then struct (true, world |> World.setGameState { gameState with EyePosition2d = value } |> World.publishGameChange (nameof gameState.EyePosition2d) value previous)
            else struct (false, world)

        /// Set the current 2d eye position.
        [<FunctionBinding>]
        static member setEyePosition2d value world =
            World.setEyePosition2dPlus value world |> snd'

        /// Get the current 2d eye size.
        [<FunctionBinding>]
        static member getEyeSize2d world =
            (World.getGameState world).EyeSize2d

        /// Set the current 2d eye size.
        static member internal setEyeSize2dPlus value world =
            let gameState = World.getGameState world
            let previous = gameState.EyeSize2d
            if v2Neq value previous
            then struct (true, world |> World.setGameState { gameState with EyeSize2d = value } |> World.publishGameChange (nameof gameState.EyeSize2d) value previous)
            else struct (false, world)

        /// Set the current 2d eye size.
        [<FunctionBinding>]
        static member setEyeSize2d value world =
            World.setEyeSize2dPlus value world |> snd'

        /// Get the current 2d eye bounds.
        [<FunctionBinding>]
        static member getEyeBounds2d world =
            let eyePosition = World.getEyePosition2d world
            let eyeSize = World.getEyeSize2d world
            box2 (eyePosition - eyeSize * 0.5f) eyeSize

        /// Get the current 3d eye position.
        [<FunctionBinding>]
        static member getEyePosition3d world =
            (World.getGameState world).EyePosition3d

        /// Set the current 3d eye position.
        static member internal setEyePosition3dPlus (value : Vector3) world =
            World.updateGameState (fun gameState ->
                if v3Neq value gameState.EyePosition3d then
                    let viewport = Constants.Render.Viewport
                    { gameState with
                        EyePosition3d = value
                        EyeFrustum3dEnclosed = viewport.Frustum (Constants.Render.NearPlaneDistanceEnclosed, Constants.Render.FarPlaneDistanceEnclosed, value, gameState.EyeRotation3d)
                        EyeFrustum3dExposed = viewport.Frustum (Constants.Render.NearPlaneDistanceExposed, Constants.Render.FarPlaneDistanceExposed, value, gameState.EyeRotation3d)
                        EyeFrustum3dImposter = viewport.Frustum (Constants.Render.NearPlaneDistanceImposter, Constants.Render.FarPlaneDistanceImposter, value, gameState.EyeRotation3d) }
                else Unchecked.defaultof<_>) "EyePosition3d" value world

        /// Set the current 3d eye position.
        [<FunctionBinding>]
        static member setEyePosition3d value world =
            World.setEyePosition3dPlus value world |> snd'

        /// Get the current 3d eye rotation.
        [<FunctionBinding>]
        static member getEyeRotation3d world =
            (World.getGameState world).EyeRotation3d

        /// Set the current 3d eye rotation.
        static member internal setEyeRotation3dPlus value world =
            World.updateGameState (fun gameState ->
                if quatNeq value gameState.EyeRotation3d then
                    let viewport = Constants.Render.Viewport
                    { gameState with
                        EyeRotation3d = value
                        EyeFrustum3dEnclosed = viewport.Frustum (Constants.Render.NearPlaneDistanceEnclosed, Constants.Render.FarPlaneDistanceEnclosed, gameState.EyePosition3d, value)
                        EyeFrustum3dExposed = viewport.Frustum (Constants.Render.NearPlaneDistanceExposed, Constants.Render.FarPlaneDistanceExposed, gameState.EyePosition3d, value)
                        EyeFrustum3dImposter = viewport.Frustum (Constants.Render.NearPlaneDistanceImposter, Constants.Render.FarPlaneDistanceImposter, gameState.EyePosition3d, value) }
                else Unchecked.defaultof<_>) "EyeRotation3d" value world

        /// Set the current 3d eye rotation.
        [<FunctionBinding>]
        static member setEyeRotation3d value world =
            World.setEyeRotation3dPlus value world |> snd'

        /// Get the current enclosed 3d eye frustum.
        [<FunctionBinding>]
        static member getEyeFrustum3dEnclosed world =
            (World.getGameState world).EyeFrustum3dEnclosed

        /// Get the current unenclosed 3d eye frustum.
        [<FunctionBinding>]
        static member getEyeFrustum3dExposed world =
            (World.getGameState world).EyeFrustum3dExposed

        /// Get the current imposter 3d eye frustum.
        [<FunctionBinding>]
        static member getEyeFrustum3dImposter world =
            (World.getGameState world).EyeFrustum3dImposter

        /// Get the current 3d light box.
        [<FunctionBinding>]
        static member getLightBox3d world =
            let lightBoxSize = Constants.Render.LightBoxSize3d
            box3 ((World.getGameState world).EyePosition3d - lightBoxSize * 0.5f) lightBoxSize

        /// Get the omni-screen, if any.
        [<FunctionBinding>]
        static member getOmniScreenOpt world =
            (World.getGameState world).OmniScreenOpt
        
        /// Set the omni-screen or None.
        static member internal setOmniScreenOptPlus value world =
            if Option.isSome value && World.getSelectedScreenOpt world = value then failwith "Cannot set OmniScreen to SelectedScreen."
            World.updateGameState (fun gameState -> if value <> gameState.OmniScreenOpt then { gameState with OmniScreenOpt = value } else Unchecked.defaultof<_>) "OmniScreenOpt" value world

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
            World.setEyePosition2d eyePosition world

        /// Set the currently selected screen or None. Be careful using this function directly as
        /// you may be wanting to use the higher-level World.transitionScreen function instead.
        static member internal setSelectedScreenOptPlus value world =

            // disallow omni-screen selection
            if  Option.isSome value &&
                World.getOmniScreenOpt world = value then
                failwith "Cannot set SelectedScreen to OmniScreen."

            // raise change event for none selection
            let struct (_, world) = World.updateGameState id "SelectedScreenOpt" None world

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
                let world = World.updateGameState id "SelectedScreenOpt" (Some screen) world |> snd'
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

        /// Get the screen desired to which to transition.
        static member getDesiredScreen world =
            (World.getGameState world).DesiredScreen

        /// Set the screen desired to which to transition.
        static member setDesiredScreen desiredScreen world =
            World.updateGameState
                (fun gameState ->
                    if desiredScreen <> gameState.DesiredScreen
                    then { gameState with DesiredScreen = desiredScreen }
                    else Unchecked.defaultof<_>)
                "DesiredScreen"
                desiredScreen
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
                "ScreenTransitionDestinationOpt"
                destination
                world

        /// Get the bounds of the 2d eye's sight irrespective of its position.
        [<FunctionBinding>]
        static member getViewBounds2dAbsolute world =
            let gameState = World.getGameState world
            box2
                (v2 (gameState.EyeSize2d.X * -0.5f) (gameState.EyeSize2d.Y * -0.5f))
                (v2 gameState.EyeSize2d.X gameState.EyeSize2d.Y)

        /// Get the bounds of the 2d play zone irrespective of eye position.
        [<FunctionBinding>]
        static member getPlayBounds2dAbsolute world =
            World.getViewBounds2d world

        /// Get the bounds of the 2d eye's sight relative to its position.
        [<FunctionBinding>]
        static member getViewBounds2d world =
            let gameState = World.getGameState world
            box2
                (v2 (gameState.EyePosition2d.X - gameState.EyeSize2d.X * 0.5f) (gameState.EyePosition2d.Y - gameState.EyeSize2d.Y * 0.5f))
                (v2 gameState.EyeSize2d.X gameState.EyeSize2d.Y)

        /// Get the bounds of the 2d play zone.
        [<FunctionBinding>]
        static member getPlayBounds2d world =
            World.getViewBounds2d world

        /// Check that the given bounds is within the 2d eye's sight.
        [<FunctionBinding>]
        static member isBoundsInView2d (bounds : Box2) world =
            let viewBounds = World.getViewBounds2d world
            bounds.Intersects viewBounds

        /// Get the bounds of the 3d play zone.
        [<FunctionBinding>]
        static member getPlayBounds3d world =
            let eyePosition = World.getEyePosition3d world
            let eyeBox = box3 (eyePosition - Constants.Render.PlayBoxSize3d * 0.5f) Constants.Render.PlayBoxSize3d
            let eyeFrustum = World.getEyeFrustum3dEnclosed world
            struct (eyeBox, eyeFrustum)

        /// Check that the given bounds is within the 3d eye's sight.
        [<FunctionBinding>]
        static member isBoundsInView3d light presence (bounds : Box3) world =
            if light then
                let lightBox = World.getLightBox3d world
                lightBox.Intersects bounds
            else
                match presence with
                | Enclosed ->
                    let enclosedFrustum = World.getEyeFrustum3dEnclosed world
                    let containment = enclosedFrustum.Contains bounds
                    containment = ContainmentType.Contains ||
                    containment = ContainmentType.Intersects
                | Exposed ->
                    let enclosedFrustum = World.getEyeFrustum3dEnclosed world
                    let containment = enclosedFrustum.Contains bounds
                    if  containment = ContainmentType.Contains ||
                        containment = ContainmentType.Intersects then true
                    else
                        let exposedFrustum = World.getEyeFrustum3dExposed world
                        let containment = exposedFrustum.Contains bounds
                        containment = ContainmentType.Contains ||
                        containment = ContainmentType.Intersects
                | Imposter ->
                    let imposterFrustum = World.getEyeFrustum3dImposter world
                    let containment = imposterFrustum.Contains bounds
                    containment = ContainmentType.Contains ||
                    containment = ContainmentType.Intersects
                | Prominent | Omnipresent ->
                    let enclosedFrustum = World.getEyeFrustum3dEnclosed world
                    let containment = enclosedFrustum.Contains bounds
                    if  containment = ContainmentType.Contains ||
                        containment = ContainmentType.Intersects then true
                    else
                        let exposedFrustum = World.getEyeFrustum3dExposed world
                        let containment = exposedFrustum.Contains bounds
                        if  containment = ContainmentType.Contains ||
                            containment = ContainmentType.Intersects then true
                        else
                            let imposterFrustum = World.getEyeFrustum3dImposter world
                            let containment = imposterFrustum.Contains bounds
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
            match World.tryGetSymbol assetTag metadata world with
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
        static member viewGameProperties world =
            let state = World.getGameState world
            World.viewProperties state

    /// Initialize property getters.
    let private initGetters () =
        GameGetters.Add ("Dispatcher", fun world -> { PropertyType = typeof<GameDispatcher>; PropertyValue = World.getGameDispatcher world })
        GameGetters.Add ("Model", fun world -> let designerProperty = World.getGameModelProperty world in { PropertyType = designerProperty.DesignerType; PropertyValue = designerProperty.DesignerValue })
        GameGetters.Add ("OmniScreenOpt", fun world -> { PropertyType = typeof<Screen option>; PropertyValue = World.getOmniScreenOpt world })
        GameGetters.Add ("SelectedScreenOpt", fun world -> { PropertyType = typeof<Screen option>; PropertyValue = World.getSelectedScreenOpt world })
        GameGetters.Add ("DesiredScreen", fun world -> { PropertyType = typeof<DesiredScreen>; PropertyValue = World.getDesiredScreen world })
        GameGetters.Add ("EyePosition2d", fun world -> { PropertyType = typeof<Vector2>; PropertyValue = World.getEyePosition2d world })
        GameGetters.Add ("EyeSize2d", fun world -> { PropertyType = typeof<Vector2>; PropertyValue = World.getEyeSize2d world })
        GameGetters.Add ("EyePosition3d", fun world -> { PropertyType = typeof<Vector3>; PropertyValue = World.getEyePosition3d world })
        GameGetters.Add ("EyeRotation3d", fun world -> { PropertyType = typeof<Quaternion>; PropertyValue = World.getEyeRotation3d world })
        GameGetters.Add ("ScriptFrame", fun world -> { PropertyType = typeof<Scripting.ProceduralFrame list>; PropertyValue = World.getGameScriptFrame world })
        GameGetters.Add ("Order", fun world -> { PropertyType = typeof<int64>; PropertyValue = World.getGameOrder world })
        GameGetters.Add ("Id", fun world -> { PropertyType = typeof<Guid>; PropertyValue = World.getGameId world })

    /// Initialize property setters.
    let private initSetters () =
        GameSetters.Add ("Model", fun property world -> World.setGameModelProperty { DesignerType = property.PropertyType; DesignerValue = property.PropertyValue } world)
        GameSetters.Add ("OmniScreenOpt", fun property world -> World.setOmniScreenOptPlus (property.PropertyValue :?> Screen option) world)
        GameSetters.Add ("DesiredScreen", fun property world -> World.setDesiredScreen (property.PropertyValue :?> DesiredScreen) world)
        GameSetters.Add ("EyePosition2d", fun property world -> World.setEyePosition2dPlus (property.PropertyValue :?> Vector2) world)
        GameSetters.Add ("EyeSize2d", fun property world -> World.setEyeSize2dPlus (property.PropertyValue :?> Vector2) world)
        GameSetters.Add ("EyePosition3d", fun property world -> World.setEyePosition3dPlus (property.PropertyValue :?> Vector3) world)
        GameSetters.Add ("EyeRotation3d", fun property world -> World.setEyeRotation3dPlus (property.PropertyValue :?> Quaternion) world)

    /// Initialize getters and setters
    let internal init () =
        initGetters ()
        initSetters ()