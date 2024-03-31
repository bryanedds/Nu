// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu
open System
open System.Collections.Generic
open System.Numerics
open Prime

[<AutoOpen>]
module WorldModuleGame =

    /// Dynamic property getters / setters.
    let private GameGetters = Dictionary<string, Game -> World -> Property> StringComparer.Ordinal
    let private GameSetters = Dictionary<string, Property -> Game -> World -> struct (bool * World)> StringComparer.Ordinal

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
                        World.setGameDesiredScreen desiredScreen game world |> snd'
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
                try let model = modelObj |> valueToSymbol |> symbolToValue
                    gameState.Model.DesignerValue <- model
                    model
                with _ ->
                    Log.debugOnce "Could not convert existing model to new type. Falling back on initial model value."
                    match gameState.Dispatcher.TryGetInitialModel<'a> world with
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
                        World.setGameDesiredScreen desiredScreen game world |> snd'
                    else world
                let world = World.publishGameChange Constants.Engine.ModelPropertyName previous.DesignerValue value game world
                struct (true, world)
            else struct (false, world)

        static member internal setGameContent (value : GameContent) game world =
            let gameState = World.getGameState game world
            let gameState = { gameState with Content = value}
            World.setGameState gameState game world

        static member internal getGameOmniScreenOpt game world =
            (World.getGameState game world).OmniScreenOpt

        static member internal setGameOmniScreenOpt value game world =
            if Option.isSome value && World.getGameSelectedScreenOpt game world = value then failwith "Cannot set OmniScreenOpt to [Some SelectedScreen]."
            let gameState = World.getGameState game world
            let previous = gameState.OmniScreenOpt
            if value <> previous
            then struct (true, world |> World.setGameState { gameState with OmniScreenOpt = value } game |> World.publishGameChange (nameof gameState.OmniScreenOpt) previous value game)
            else struct (false, world)

        /// Get the omni-screen, if any.
        static member getOmniScreenOpt world =
            World.getGameOmniScreenOpt Game.Handle world

        /// Set the omni-screen or None.
        static member setOmniScreenOpt value world =
            World.setGameOmniScreenOpt value Game.Handle world |> snd'

        static member internal getGameOmniScreen game world =
            (World.getGameOmniScreenOpt game world).Value

        static member internal setGameOmniScreen value game world =
            World.setGameOmniScreenOpt (Some value) game world

        /// Get the omni-screen.
        static member getOmniScreen world =
            World.getGameOmniScreen Game.Handle world

        /// Set the omni-screen.
        static member setOmniScreen value world =
            World.setGameOmniScreen value Game.Handle world |> snd'

        static member internal getGameSelectedScreenOpt game world =
            (World.getGameState game world).SelectedScreenOpt

        static member internal setGameSelectedScreenOpt value game world =

            // disallow omni-screen selection
            if  Option.isSome value &&
                World.getGameOmniScreenOpt game world = value then
                failwith "Cannot set SelectedScreen to OmniScreen."

            // update game state if changed
            let gameState = World.getGameState game world
            let previous = gameState.SelectedScreenOpt
            if value <> previous then

                // raise change event for None case
                let world =
                    match value with
                    | None -> World.publishGameChange (nameof gameState.SelectedScreenOpt) previous None game world
                    | _ -> world

                // clear out singleton states
                let world =
                    match (World.getGameState game world).SelectedScreenOpt with
                    | Some screen ->
                        let world = WorldModule.unregisterScreenPhysics screen world
                        let world = WorldModule.evictScreenElements screen world
                        world
                    | None -> world
                
                // actually set selected screen (no events)
                let gameState = World.getGameState game world
                let gameState = { gameState with SelectedScreenOpt = value }
                let world = World.setGameState gameState game world

                // raise change event for Some case
                match value with
                | Some screen ->

                    // populate singleton states
                    let world = WorldModule.admitScreenElements screen world
                    let world = WorldModule.registerScreenPhysics screen world

                    // raise change event for some selection
                    let world = World.publishGameChange (nameof gameState.SelectedScreenOpt) previous value game world
                    (true, world)

                // fin
                | None -> (true, world)
            else (false, world)

        /// Get the currently selected screen, if any.
        static member getSelectedScreenOpt world =
            World.getGameSelectedScreenOpt Game.Handle world

        /// Set the currently selected screen or None.
        static member setSelectedScreenOpt value world =
            World.setGameSelectedScreenOpt value Game.Handle world |> snd

        static member internal getGameSelectedScreen game world =
            (World.getGameSelectedScreenOpt game world).Value

        static member internal setGameSelectedScreen screen game world =
            World.setGameSelectedScreenOpt (Some screen) game world

        /// Get the currently selected screen.
        static member getSelectedScreen world =
            World.getGameSelectedScreen Game.Handle world

        /// Set the currently selected screen.
        static member setSelectedScreen value world =
            World.setGameSelectedScreen value Game.Handle world |> snd

        static member internal getGameDesiredScreen game world =
            (World.getGameState game world).DesiredScreen

        static member internal setGameDesiredScreen value game world =
            let gameState = World.getGameState game world
            let previous = gameState.DesiredScreen
            if value <> previous
            then struct (true, world |> World.setGameState { gameState with DesiredScreen = value } game |> World.publishGameChange (nameof gameState.DesiredScreen) previous value game)
            else struct (false, world)

        /// Get the desired screen, if applicable.
        static member getDesiredScreen world =
            World.getGameDesiredScreen Game.Handle world

        /// Set the desired screen, if applicable.
        static member setDesiredScreen value world =
            World.setGameDesiredScreen value Game.Handle world |> snd'

        static member internal getGameScreenTransitionDestinationOpt game world =
            (World.getGameState game world).ScreenTransitionDestinationOpt

        static member internal setGameScreenTransitionDestinationOpt value game world =
            let gameState = World.getGameState game world
            let previous = gameState.ScreenTransitionDestinationOpt
            if value <> previous
            then struct (true, world |> World.setGameState { gameState with ScreenTransitionDestinationOpt = value } game |> World.publishGameChange (nameof gameState.ScreenTransitionDestinationOpt) previous value game)
            else struct (false, world)

        /// Get the current destination screen if a screen transition is currently underway.
        static member getScreenTransitionDestinationOpt world =
            World.getGameScreenTransitionDestinationOpt Game.Handle world

        /// Set the current destination screen or None.
        static member setScreenTransitionDestinationOpt value world =
            World.setGameScreenTransitionDestinationOpt value Game.Handle world |> snd'

        static member internal getGameEye2dCenter game world =
            (World.getGameState game world).Eye2dCenter

        static member internal setGameEye2dCenter value game world =
            let gameState = World.getGameState game world
            let previous = gameState.Eye2dCenter
            if v2Neq previous value
            then struct (true, world |> World.setGameState { gameState with Eye2dCenter = value } game |> World.publishGameChange (nameof gameState.Eye2dCenter) previous value game)
            else struct (false, world)

        /// Get the current 2d eye center.
        static member getEye2dCenter world =
            World.getGameEye2dCenter Game.Handle world

        /// Set the current 2d eye center.
        static member setEye2dCenter value world =
            World.setGameEye2dCenter value Game.Handle world |> snd'

        static member internal getGameEye2dSize game world =
            (World.getGameState game world).Eye2dSize

        static member internal setGameEye2dSize value game world =
            let gameState = World.getGameState game world
            let previous = gameState.Eye2dSize
            if v2Neq previous value
            then struct (true, world |> World.setGameState { gameState with Eye2dSize = value } game |> World.publishGameChange (nameof gameState.Eye2dSize) previous value game)
            else struct (false, world)

        /// Get the current 2d eye size.
        static member getEye2dSize world =
            World.getGameEye2dSize Game.Handle world

        /// Set the current 2d eye size.
        static member setEye2dSize value world =
            World.setGameEye2dSize value Game.Handle world |> snd'

        /// Get the current 2d eye bounds.
        static member getEyeBounds2d world =
            let eyeCenter = World.getGameEye2dCenter Game.Handle world
            let eyeSize = World.getGameEye2dSize Game.Handle world
            box2 (eyeCenter - eyeSize * 0.5f) eyeSize

        /// Constrain the eye to the given 2d bounds.
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
            World.setGameEye2dCenter eyeCenter Game.Handle world |> snd'

        /// Get the bounds of the 2d eye's sight irrespective of its position.
        static member getViewBounds2dAbsolute world =
            let gameState = World.getGameState Game.Handle world
            box2
                (v2 (gameState.Eye2dSize.X * -0.5f) (gameState.Eye2dSize.Y * -0.5f))
                (v2 gameState.Eye2dSize.X gameState.Eye2dSize.Y)

        /// Get the bounds of the 2d eye's sight relative to its position.
        static member getViewBounds2dRelative world =
            let gameState = World.getGameState Game.Handle world
            let min = v2 (gameState.Eye2dCenter.X - gameState.Eye2dSize.X * 0.5f) (gameState.Eye2dCenter.Y - gameState.Eye2dSize.Y * 0.5f)
            box2 min gameState.Eye2dSize

        /// Get the bounds of the 2d play zone irrespective of eye center.
        static member getPlayBounds2dAbsolute world =
            World.getViewBounds2dAbsolute world

        /// Get the bounds of the 2d play zone relative to eye center.
        static member getPlayBounds2dRelative world =
            World.getViewBounds2dRelative world

        /// Check that the given bounds is within the 2d eye's sight irrespective of eye center.
        static member boundsInView2dAbsolute (bounds : Box2) world =
            let viewBounds = World.getViewBounds2dAbsolute world
            bounds.Intersects viewBounds

        /// Check that the given bounds is within the 2d eye's sight relative to eye center.
        static member boundsInView2dRelative (bounds : Box2) world =
            let viewBounds = World.getViewBounds2dRelative world
            bounds.Intersects viewBounds

        static member internal getGameEye3dCenter game world =
            (World.getGameState game world).Eye3dCenter

        static member internal setGameEye3dCenter (value : Vector3) game world =
            let gameState = World.getGameState game world
            let previous = gameState.Eye3dCenter
            if v3Neq previous value then
                let viewportInterior = Viewport (Constants.Render.NearPlaneDistanceInterior, Constants.Render.FarPlaneDistanceInterior, v2iZero, Constants.Render.Resolution)
                let viewportExterior = Viewport (Constants.Render.NearPlaneDistanceExterior, Constants.Render.FarPlaneDistanceExterior, v2iZero, Constants.Render.Resolution)
                let viewportImposter = Viewport (Constants.Render.NearPlaneDistanceImposter, Constants.Render.FarPlaneDistanceImposter, v2iZero, Constants.Render.Resolution)
                let gameState =
                    { gameState with
                        Eye3dCenter = value
                        Eye3dFrustumInterior = viewportInterior.Frustum (value, gameState.Eye3dRotation)
                        Eye3dFrustumExterior = viewportExterior.Frustum (value, gameState.Eye3dRotation)
                        Eye3dFrustumImposter = viewportImposter.Frustum (value, gameState.Eye3dRotation) }
                struct (true, world |> World.setGameState gameState game |> World.publishGameChange (nameof gameState.Eye3dCenter) previous value game)
            else struct (false, world)

        /// Get the current 3d eye center.
        static member getEye3dCenter world =
            World.getGameEye3dCenter Game.Handle world

        /// Set the current 3d eye center.
        static member setEye3dCenter value world =
            World.setGameEye3dCenter value Game.Handle world |> snd'

        static member internal getGameEye3dRotation game world =
            (World.getGameState game world).Eye3dRotation

        static member internal setGameEye3dRotation value game world =
            let gameState = World.getGameState game world
            let previous = gameState.Eye3dRotation
            if quatNeq previous value then
                let viewportInterior = Viewport (Constants.Render.NearPlaneDistanceInterior, Constants.Render.FarPlaneDistanceInterior, v2iZero, Constants.Render.Resolution)
                let viewportExterior = Viewport (Constants.Render.NearPlaneDistanceExterior, Constants.Render.FarPlaneDistanceExterior, v2iZero, Constants.Render.Resolution)
                let viewportImposter = Viewport (Constants.Render.NearPlaneDistanceImposter, Constants.Render.FarPlaneDistanceImposter, v2iZero, Constants.Render.Resolution)
                let gameState =
                    { gameState with
                        Eye3dRotation = value
                        Eye3dFrustumInterior = viewportInterior.Frustum (gameState.Eye3dCenter, value)
                        Eye3dFrustumExterior = viewportExterior.Frustum (gameState.Eye3dCenter, value)
                        Eye3dFrustumImposter = viewportImposter.Frustum (gameState.Eye3dCenter, value) }
                struct (true, world |> World.setGameState gameState game |> World.publishGameChange (nameof gameState.Eye3dRotation) previous value game)
            else struct (false, world)

        /// Get the current 3d eye rotation.
        static member getEye3dRotation world =
            World.getGameEye3dRotation Game.Handle world

        /// Set the current 3d eye rotation.
        static member setEye3dRotation value world =
            World.setGameEye3dRotation value Game.Handle world |> snd'

        static member internal getGameEye3dFrustumInterior game world =
            (World.getGameState game world).Eye3dFrustumInterior

        static member internal getGameEye3dFrustumExterior game world =
            (World.getGameState game world).Eye3dFrustumExterior

        static member internal getGameEye3dFrustumImposter game world =
            (World.getGameState game world).Eye3dFrustumImposter

        static member internal getGameEye3dFrustumView game world =
            let viewport = Constants.Render.Viewport
            viewport.Frustum (World.getGameEye3dCenter game world, World.getGameEye3dRotation game world)

        /// Get the current interior 3d eye frustum.
        static member getEye3dFrustumInterior world =
            World.getGameEye3dFrustumInterior Game.Handle world

        /// Get the current exterior 3d eye frustum.
        static member getEye3dFrustumExterior world =
            World.getGameEye3dFrustumExterior Game.Handle world

        /// Get the current imposter 3d eye frustum.
        static member getEye3dFrustumImposter world =
            World.getGameEye3dFrustumImposter Game.Handle world

        /// Get the current 3d eye view frustum.
        static member getEye3dFrustumView world =
            World.getGameEye3dFrustumView Game.Handle world

        /// Convert the given relative 3d position to the absolute 2d position.
        /// Useful for gui entities that track 3d entities.
        /// Where Z > 1.0f, position is behind view.
        static member position3dToPosition2d position world =
            let eyeCenter = World.getEye3dCenter world
            let eyeRotation = World.getEye3dRotation world
            let viewport = Constants.Render.Viewport
            viewport.Position3dToPosition2d (position, eyeCenter, eyeRotation, Constants.Render.ResolutionX, Constants.Render.ResolutionY)

        /// Get the current 3d light box.
        static member getLight3dBox world =
            let lightBoxSize = Constants.Render.Light3dBoxSize
            box3 ((World.getGameState Game.Handle world).Eye3dCenter - lightBoxSize * 0.5f) lightBoxSize

        /// Get the bounds of the 3d play zone.
        static member getPlayBounds3d world =
            let eyeCenter = World.getGameEye3dCenter Game.Handle world
            let eyeBox = box3 (eyeCenter - Constants.Render.Play3dBoxSize * 0.5f) Constants.Render.Play3dBoxSize
            let eyeFrustum = World.getGameEye3dFrustumInterior Game.Handle world
            struct (eyeBox, eyeFrustum)

        /// Check that the given bounds is within the 3d eye's sight (or a light probe / light in the light box that may be lighting something within it).
        static member boundsInView3d lightProbe light presence (bounds : Box3) world =
            Presence.intersects3d
                (Some (World.getGameEye3dFrustumInterior Game.Handle world))
                (World.getGameEye3dFrustumExterior Game.Handle world)
                (World.getGameEye3dFrustumImposter Game.Handle world)
                (Some (World.getLight3dBox world))
                lightProbe light presence bounds

        /// Check that the given bounds is within the 3d eye's play bounds.
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
        GameGetters.Add ("OmniScreenOpt", fun game world -> { PropertyType = typeof<Screen option>; PropertyValue = World.getGameOmniScreenOpt game world })
        GameGetters.Add ("SelectedScreenOpt", fun game world -> { PropertyType = typeof<Screen option>; PropertyValue = World.getGameSelectedScreenOpt game world })
        GameGetters.Add ("DesiredScreen", fun game world -> { PropertyType = typeof<DesiredScreen>; PropertyValue = World.getGameDesiredScreen game world })
        GameGetters.Add ("ScreenTransitionDestinationOpt", fun game world -> { PropertyType = typeof<Screen option>; PropertyValue = World.getGameScreenTransitionDestinationOpt game world })
        GameGetters.Add ("Eye2dCenter", fun game world -> { PropertyType = typeof<Vector2>; PropertyValue = World.getGameEye2dCenter game world })
        GameGetters.Add ("Eye2dSize", fun game world -> { PropertyType = typeof<Vector2>; PropertyValue = World.getGameEye2dSize game world })
        GameGetters.Add ("Eye3dCenter", fun game world -> { PropertyType = typeof<Vector3>; PropertyValue = World.getGameEye3dCenter game world })
        GameGetters.Add ("Eye3dRotation", fun game world -> { PropertyType = typeof<Quaternion>; PropertyValue = World.getGameEye3dRotation game world })
        GameGetters.Add ("Order", fun game world -> { PropertyType = typeof<int64>; PropertyValue = World.getGameOrder game world })
        GameGetters.Add ("Id", fun game world -> { PropertyType = typeof<Guid>; PropertyValue = World.getGameId game world })

    /// Initialize property setters.
    let private initSetters () =
        GameSetters.Add ("Model", fun property game world -> World.setGameModelProperty false { DesignerType = property.PropertyType; DesignerValue = property.PropertyValue } game world)
        GameSetters.Add ("OmniScreenOpt", fun property game world -> World.setGameOmniScreenOpt (property.PropertyValue :?> Screen option) game world)
        GameSetters.Add ("DesiredScreen", fun property game world -> World.setGameDesiredScreen (property.PropertyValue :?> DesiredScreen) game world)
        GameSetters.Add ("ScreenTransitionDestinationOpt", fun property game world -> World.setGameScreenTransitionDestinationOpt (property.PropertyValue :?> Screen option) game world)
        GameSetters.Add ("Eye2dCenter", fun property game world -> World.setGameEye2dCenter (property.PropertyValue :?> Vector2) game world)
        GameSetters.Add ("Eye2dSize", fun property game world -> World.setGameEye2dSize (property.PropertyValue :?> Vector2) game world)
        GameSetters.Add ("Eye3dCenter", fun property game world -> World.setGameEye3dCenter (property.PropertyValue :?> Vector3) game world)
        GameSetters.Add ("Eye3dRotation", fun property game world -> World.setGameEye3dRotation (property.PropertyValue :?> Quaternion) game world)

    /// Initialize getters and setters
    let internal init () =
        initGetters ()
        initSetters ()