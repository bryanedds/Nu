// Nu Game Engine.
// Copyright (C) Bryan Edds.

namespace Nu
open System
open System.Collections.Frozen
open System.Numerics
open Prime

/// Game functions for the world (1/2).
[<AutoOpen>]
module WorldModuleGame =

    /// Dynamic property getter and setter.
    type private PropertyGetter = Game -> World -> Property
    type private PropertySetter = Property -> Game -> World -> bool

    /// Dynamic property getters / setters.
    let mutable private GameGetters = Unchecked.defaultof<FrozenDictionary<string, PropertyGetter>>
    let mutable private GameSetters = Unchecked.defaultof<FrozenDictionary<string, PropertySetter>>

    type World with

        static member private publishGameChange propertyName (propertyPrevious : obj) (propertyValue : obj) game world =
            let changeData = { Name = propertyName; Previous = propertyPrevious; Value = propertyValue }
            let changeEventAddress = rtoa<ChangeData> [|Constants.Lens.ChangeName; propertyName; Constants.Lens.EventName; Constants.Engine.GameName|]
            let eventTrace = EventTrace.debug "World" "publishGameChange" "" EventTrace.empty
            World.publishPlus changeData changeEventAddress eventTrace game false false world

        static member internal getGameState game (world : World) =
            ignore<Game> game
            world.GameState

        static member internal setGameState gameState game world =
            ignore<Game> game
            world.WorldState <- { world.WorldState with GameState = gameState }

        static member internal getGameXtension game world =
            let gameState = World.getGameState game world
            gameState.Xtension

        static member internal getGameId game world = (World.getGameState game world).Id
        static member internal getGameOrder game world = (World.getGameState game world).Order
        static member internal getGameDispatcher game world = (World.getGameState game world).Dispatcher
        static member internal getGameModelProperty game world = (World.getGameState game world).Model
        static member internal getGameContent game world = (World.getGameState game world).Content

        static member internal setGameModelProperty initializing reinitializing (value : DesignerProperty) game world =
            let gameState = World.getGameState game world
            let previous = gameState.Model
            if value.DesignerValue =/= previous.DesignerValue || initializing then
                let gameState = { gameState with Model = { DesignerType = value.DesignerType; DesignerValue = value.DesignerValue }}
                World.setGameState gameState game world
                gameState.Dispatcher.TrySynchronize (initializing, reinitializing, game, world)
                if initializing then
                    let content = World.getGameContent game world
                    let desiredScreen =
                        match Seq.tryHead content.ScreenContents with
                        | Some screen -> Desire (game / screen.Key)
                        | None -> DesireNone
                    World.setGameDesiredScreen desiredScreen game world |> ignore<bool>
                World.publishGameChange Constants.Engine.ModelPropertyName previous.DesignerValue value.DesignerValue game world
                true
            else false

        static member internal getGameModelGeneric<'a> game world =
            let gameState = World.getGameState game world
            match gameState.Model.DesignerValue with
            | :? 'a as model -> model
            | null -> null :> obj :?> 'a
            | modelObj ->
                let modelSymbol = valueToSymbol modelObj
                try let model = symbolToValue modelSymbol
                    gameState.Model <- { DesignerType = typeof<'a>; DesignerValue = model }
                    model
                with _ ->
                    Log.warn "Could not convert existing game model value to new type; attempting to use fallback model value instead."
                    match gameState.Dispatcher.TryGetFallbackModel<'a> (modelSymbol, game, world) with
                    | None -> typeof<'a>.GetDefaultValue () :?> 'a
                    | Some model ->
                        gameState.Model <- { DesignerType = typeof<'a>; DesignerValue = model }
                        model

        static member internal setGameModelGeneric<'a> initializing reinitializing (value : 'a) (game : Game) world =
            let gameState = World.getGameState game world
            let valueObj = value :> obj
            let previous = gameState.Model
            if valueObj =/= previous.DesignerValue || initializing then
                let gameState = { gameState with Model = { DesignerType = typeof<'a>; DesignerValue = valueObj }}
                World.setGameState gameState game world
                gameState.Dispatcher.TrySynchronize (initializing, reinitializing, game, world)
                if initializing then
                    let content = World.getGameContent game world
                    let desiredScreen =
                        match Seq.tryHead content.ScreenContents with
                        | Some screen -> Desire (game / screen.Key)
                        | None -> DesireNone
                    World.setGameDesiredScreen desiredScreen game world |> ignore<bool>
                World.publishGameChange Constants.Engine.ModelPropertyName previous.DesignerValue value game world
                true
            else false

        static member internal setGameContent (value : GameContent) game world =
            let gameState = World.getGameState game world
            let gameState = { gameState with Content = value}
            World.setGameState gameState game world

        static member internal getGameSelectedScreenOpt game world =
            (World.getGameState game world).SelectedScreenOpt

        static member internal setGameSelectedScreenOpt value game world =

            // update game state if changed
            let gameState = World.getGameState game world
            let previous = gameState.SelectedScreenOpt
            if value <> previous then

                // raise change event for None case
                match value with
                | None -> World.publishGameChange (nameof gameState.SelectedScreenOpt) previous None game world
                | _ -> ()

                // clear out singleton states
                match (World.getGameState game world).SelectedScreenOpt with
                | Some screen ->
                    WorldModule.unregisterScreenPhysics screen world
                    WorldModule.evictScreenElements screen world
                | None -> ()
                
                // actually set selected screen (no events)
                let gameState = World.getGameState game world
                let gameState = { gameState with SelectedScreenOpt = value }
                World.setGameState gameState game world

                // raise change event for Some case
                match value with
                | Some screen ->

                    // populate singleton states
                    WorldModule.admitScreenElements screen world
                    WorldModule.registerScreenPhysics screen world

                    // raise change event for some selection
                    World.publishGameChange (nameof gameState.SelectedScreenOpt) previous value game world
                    true

                // fin
                | None -> true
            else false

        /// Get the currently selected screen, if any.
        static member getSelectedScreenOpt world =
            World.getGameSelectedScreenOpt Game.Handle world

        /// Set the currently selected screen or None.
        static member setSelectedScreenOpt value world =
            World.setGameSelectedScreenOpt value Game.Handle world |> ignore<bool>

        static member internal getGameSelectedScreen game world =
            (World.getGameSelectedScreenOpt game world).Value

        static member internal setGameSelectedScreen screen game world =
            World.setGameSelectedScreenOpt (Some screen) game world

        /// Get the currently selected screen.
        static member getSelectedScreen world =
            World.getGameSelectedScreen Game.Handle world

        /// Set the currently selected screen.
        static member setSelectedScreen value world =
            World.setGameSelectedScreen value Game.Handle world |> ignore<bool>

        static member internal getGameDesiredScreen game world =
            (World.getGameState game world).DesiredScreen

        static member internal setGameDesiredScreen value game world : bool =
            let gameState = World.getGameState game world
            let previous = gameState.DesiredScreen
            if value <> previous then
                World.setGameState { gameState with DesiredScreen = value } game world
                World.publishGameChange (nameof gameState.DesiredScreen) previous value game world
                true
            else false

        /// Get the desired screen, if applicable.
        static member getDesiredScreen world =
            World.getGameDesiredScreen Game.Handle world

        /// Set the desired screen, if applicable.
        static member setDesiredScreen value world =
            World.setGameDesiredScreen value Game.Handle world |> ignore<bool>

        static member internal getGameScreenTransitionDestinationOpt game world =
            (World.getGameState game world).ScreenTransitionDestinationOpt

        static member internal setGameScreenTransitionDestinationOpt value game world =
            let gameState = World.getGameState game world
            let previous = gameState.ScreenTransitionDestinationOpt
            if value <> previous then
                World.setGameState { gameState with ScreenTransitionDestinationOpt = value } game world
                World.publishGameChange (nameof gameState.ScreenTransitionDestinationOpt) previous value game world
                true
            else false

        /// Get the current destination screen if a screen transition is currently underway.
        static member getScreenTransitionDestinationOpt world =
            World.getGameScreenTransitionDestinationOpt Game.Handle world

        /// Set the current destination screen or None.
        static member setScreenTransitionDestinationOpt value world =
            World.setGameScreenTransitionDestinationOpt value Game.Handle world |> ignore<bool>

        static member internal getGameEye2dCenter game world =
            (World.getGameState game world).Eye2dCenter

        static member internal setGameEye2dCenter value game world =
            let gameState = World.getGameState game world
            let previous = gameState.Eye2dCenter
            if v2Neq previous value then
                World.setGameState { gameState with Eye2dCenter = value } game world
                World.publishGameChange (nameof gameState.Eye2dCenter) previous value game world
                true
            else false

        /// Get the current 2d eye center.
        static member getEye2dCenter world =
            World.getGameEye2dCenter Game.Handle world

        /// Set the current 2d eye center.
        static member setEye2dCenter value world =
            World.setGameEye2dCenter value Game.Handle world |> ignore<bool>

        static member internal getGameEye2dSize game world =
            (World.getGameState game world).Eye2dSize

        static member internal setGameEye2dSize value game world =
            let gameState = World.getGameState game world
            let previous = gameState.Eye2dSize
            if v2Neq previous value then
                World.setGameState { gameState with Eye2dSize = value } game world
                World.publishGameChange (nameof gameState.Eye2dSize) previous value game world
                true
            else false

        /// Get the current 2d eye size.
        static member getEye2dSize world =
            World.getGameEye2dSize Game.Handle world

        /// Set the current 2d eye size.
        static member setEye2dSize value world =
            World.setGameEye2dSize value Game.Handle world |> ignore<bool>

        /// Get the current 2d eye bounds.
        static member getEye2dBounds world =
            let eyeCenter = World.getGameEye2dCenter Game.Handle world
            let eyeSize = World.getGameEye2dSize Game.Handle world
            box2 (eyeCenter - eyeSize * 0.5f) eyeSize

        /// Constrain the eye to the given 2d bounds.
        static member constrainEye2dBounds (bounds : Box2) world =
            let mutable eyeBounds = World.getEye2dBounds world
            eyeBounds.Min <-
                v2
                    (if eyeBounds.Min.X < bounds.Min.X then bounds.Min.X
                        elif eyeBounds.Right.X > bounds.Right.X then bounds.Right.X - eyeBounds.Size.X
                        else eyeBounds.Min.X)
                    (if eyeBounds.Min.Y < bounds.Min.Y then bounds.Min.Y
                        elif eyeBounds.Top.Y > bounds.Top.Y then bounds.Top.Y - eyeBounds.Size.Y
                        else eyeBounds.Min.Y)
            let eyeCenter = eyeBounds.Center
            World.setGameEye2dCenter eyeCenter Game.Handle world |> ignore<bool>

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

        /// Check that the given bounds is within view.
        static member boundsInView2d absolute (bounds : Box2) world =
            if absolute
            then World.boundsInView2dAbsolute bounds world
            else World.boundsInView2dRelative bounds world

        /// Check that the given bounds is within the 2d eye's sight relative to eye center.
        static member boundsInView2dRelative (bounds : Box2) world =
            let viewBounds = World.getViewBounds2dRelative world
            bounds.Intersects viewBounds

        /// Query the quadtree's spatial bounds for 2D entities.
        static member getSpatialBounds2d (world : World) =
            Quadtree.getBounds world.Quadtree

        static member internal getGameEye3dCenter game world =
            (World.getGameState game world).Eye3dCenter

        static member internal setGameEye3dCenter (value : Vector3) game world =
            let gameState = World.getGameState game world
            let previous = gameState.Eye3dCenter
            if v3Neq previous value then
                let viewportInterior = Viewport.makeInterior ()
                let viewportExterior = Viewport.makeExterior ()
                let viewportImposter = Viewport.makeImposter ()
                let gameState =
                    { gameState with
                        Eye3dCenter = value
                        Eye3dFrustumInterior = Viewport.getFrustum value gameState.Eye3dRotation gameState.Eye3dFieldOfView viewportInterior
                        Eye3dFrustumExterior = Viewport.getFrustum value gameState.Eye3dRotation gameState.Eye3dFieldOfView viewportExterior
                        Eye3dFrustumImposter = Viewport.getFrustum value gameState.Eye3dRotation gameState.Eye3dFieldOfView viewportImposter }
                World.setGameState gameState game world
                World.publishGameChange (nameof gameState.Eye3dCenter) previous value game world
                true
            else false

        /// Get the current 3d eye center.
        static member getEye3dCenter world =
            World.getGameEye3dCenter Game.Handle world

        /// Set the current 3d eye center.
        static member setEye3dCenter value world =
            World.setGameEye3dCenter value Game.Handle world |> ignore<bool>

        static member internal getGameEye3dRotation game world =
            (World.getGameState game world).Eye3dRotation

        static member internal setGameEye3dRotation value game world =
            let gameState = World.getGameState game world
            let previous = gameState.Eye3dRotation
            if quatNeq previous value then
                let viewportInterior = Viewport.makeInterior ()
                let viewportExterior = Viewport.makeExterior ()
                let viewportImposter = Viewport.makeImposter ()
                let gameState =
                    { gameState with
                        Eye3dRotation = value
                        Eye3dFrustumInterior = Viewport.getFrustum gameState.Eye3dCenter value gameState.Eye3dFieldOfView viewportInterior
                        Eye3dFrustumExterior = Viewport.getFrustum gameState.Eye3dCenter value gameState.Eye3dFieldOfView viewportExterior
                        Eye3dFrustumImposter = Viewport.getFrustum gameState.Eye3dCenter value gameState.Eye3dFieldOfView viewportImposter }
                World.setGameState gameState game world
                World.publishGameChange (nameof gameState.Eye3dRotation) previous value game world
                true
            else false
            
        /// Get the current 3d eye rotation.
        static member getEye3dRotation world =
            World.getGameEye3dRotation Game.Handle world
            
        /// Set the current 3d eye rotation.
        static member setEye3dRotation value world =
            World.setGameEye3dRotation value Game.Handle world |> ignore<bool>

        static member internal getGameEye3dFieldOfView game world =
            (World.getGameState game world).Eye3dFieldOfView

        static member internal setGameEye3dFieldOfView value game world =
            let gameState = World.getGameState game world
            let previous = gameState.Eye3dFieldOfView
            if previous <> value then
                let viewportInterior = Viewport.makeInterior ()
                let viewportExterior = Viewport.makeExterior ()
                let viewportImposter = Viewport.makeImposter ()
                let gameState =
                    { gameState with
                        Eye3dFieldOfView = value
                        Eye3dFrustumInterior = Viewport.getFrustum gameState.Eye3dCenter gameState.Eye3dRotation value viewportInterior
                        Eye3dFrustumExterior = Viewport.getFrustum gameState.Eye3dCenter gameState.Eye3dRotation value viewportExterior
                        Eye3dFrustumImposter = Viewport.getFrustum gameState.Eye3dCenter gameState.Eye3dRotation value viewportImposter }
                World.setGameState gameState game world
                World.publishGameChange (nameof gameState.Eye3dFieldOfView) previous value game world
                true
            else false

        static member internal getGameEye3dAspectRatio game world =
            ignore<Game> game
            ignore<World> world
            single Constants.Render.DisplayVirtualResolution.X /
            single Constants.Render.DisplayVirtualResolution.Y

        /// Get the current 3d eye field of view.
        static member getEye3dFieldOfView world =
            World.getGameEye3dFieldOfView Game.Handle world

        /// Set the current 3d eye field of view.
        static member setEye3dFieldOfView value world =
            World.setGameEye3dFieldOfView value Game.Handle world |> ignore<bool>

        /// Get the current 3d eye aspect ratio.
        static member getEye3dAspectRatio world =
            World.getGameEye3dAspectRatio Game.Handle world

        static member internal getGameEye3dFrustumInterior game world =
            (World.getGameState game world).Eye3dFrustumInterior

        static member internal getGameEye3dFrustumExterior game world =
            (World.getGameState game world).Eye3dFrustumExterior

        static member internal getGameEye3dFrustumImposter game world =
            (World.getGameState game world).Eye3dFrustumImposter

        static member internal getGameEye3dFrustum game (world : World) =
            let eyeCenter = World.getGameEye3dCenter game world
            let eyeRotation = World.getGameEye3dRotation game world
            let eyeFieldOfView = World.getGameEye3dFieldOfView game world
            Viewport.getFrustum eyeCenter eyeRotation eyeFieldOfView world.RasterViewport

        /// Get the current interior 3d eye frustum.
        static member getEye3dFrustumInterior world =
            World.getGameEye3dFrustumInterior Game.Handle world

        /// Get the current exterior 3d eye frustum.
        static member getEye3dFrustumExterior world =
            World.getGameEye3dFrustumExterior Game.Handle world

        /// Get the current imposter 3d eye frustum.
        static member getEye3dFrustumImposter world =
            World.getGameEye3dFrustumImposter Game.Handle world

        /// Get the current 3d eye frustum.
        static member getEye3dFrustum world =
            World.getGameEye3dFrustum Game.Handle world

        /// Convert the given relative 3d position to the absolute 2d position.
        /// Useful for gui entities that track 3d entities.
        /// Where Z > 1.0f, position is behind view.
        static member position3dToPosition2d position (world : World) =
            let rasterViewport = world.RasterViewport
            let eyeCenter = World.getEye3dCenter world
            let eyeRotation = World.getEye3dRotation world
            let eyeFieldOfView = World.getEye3dFieldOfView world
            Viewport.position3dToPosition2d eyeCenter eyeRotation eyeFieldOfView position rasterViewport

        /// Convert the given absolute 2d position to the relative 3d ray.
        /// TODO: also implement position2dToPosition3d.
        static member position2dToRay3d position (world : World) =
            let rasterViewport = world.RasterViewport
            let eyeCenter = World.getEye3dCenter world
            let eyeRotation = World.getEye3dRotation world
            let eyeFieldOfView = World.getEye3dFieldOfView world
            Viewport.position2dToRay3d eyeCenter eyeRotation eyeFieldOfView position rasterViewport

        /// Get the current 3d light box.
        static member getLight3dViewBox world =
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
                (ValueSome (World.getGameEye3dFrustumInterior Game.Handle world))
                (World.getGameEye3dFrustumExterior Game.Handle world)
                (World.getGameEye3dFrustumImposter Game.Handle world)
                (ValueSome (World.getLight3dViewBox world))
                lightProbe light presence bounds

        /// Check that the given bounds is within the 3d eye's play bounds.
        static member boundsInPlay3d (bounds : Box3) world =
            let struct (viewBox, viewFrustum) = World.getPlayBounds3d world
            if bounds.Intersects viewBox then true
            else
                let containment = viewFrustum.Contains bounds
                containment = ContainmentType.Contains ||
                containment = ContainmentType.Intersects

        /// Query the octree's spatial bounds for 3D entities.
        static member getSpatialBounds3d world =
            Octree.getBounds world.Octree

        static member internal getElements2dBy (getElementsFromQuadree : Entity Quadtree -> unit) (world : World) =
            getElementsFromQuadree world.Quadtree

        static member internal getElements2dInView set world =
            let viewBounds = World.getViewBounds2dRelative world
            World.getElements2dBy (Quadtree.getElementsInView viewBounds set) world

        static member internal getElements2dInPlay set world =
            let playBounds = World.getPlayBounds2dRelative world
            World.getElements2dBy (Quadtree.getElementsInPlay playBounds set) world

        /// Get all 2d entities in the given bounds, including all uncullable entities.
        static member getEntities2dInBounds bounds set (world : World) =
            Quadtree.getElementsInBounds bounds set world.Quadtree
            Seq.map (fun (element : Entity Quadelement) -> element.Entry) set

        /// Get all 2d entities at the given point, including all uncullable entities.
        static member getEntities2dAtPoint point set (world : World) =
            Quadtree.getElementsAtPoint point set world.Quadtree
            Seq.map (fun (element : Entity Quadelement) -> element.Entry) set

        /// Get all 2d entities in the current selected screen, including all uncullable entities.
        static member getEntities2d set (world : World) =
            Quadtree.getElements set world.Quadtree
            Seq.map (fun (element : Entity Quadelement) -> element.Entry) set

        /// Get all 2d entities in the current 2d view, including all uncullable entities.
        static member getEntities2dInView set (world : World) =
            let viewBounds = World.getViewBounds2dRelative world
            Quadtree.getElementsInView viewBounds set world.Quadtree
            Seq.map (fun (element : Entity Quadelement) -> element.Entry) set

        /// Get all 2d entities needing to update for the current 2d play zone, including all uncullable entities.
        static member getEntities2dInPlay set (world : World) =
            let playBounds = World.getPlayBounds2dRelative world
            Quadtree.getElementsInPlay playBounds set world.Quadtree
            Seq.map (fun (element : Entity Quadelement) -> element.Entry) set

        static member internal getElements3dInViewFrustum interior exterior frustum set (world : World) =
            Octree.getElementsInViewFrustum interior exterior frustum set world.Octree

        static member internal getElements3dInViewBox box set (world : World) =
            Octree.getElementsInViewBox box set world.Octree

        static member internal getElements3dInView set (world : World) =
            let lightBox = World.getLight3dViewBox world
            Octree.getElementsInView world.Eye3dFrustumInterior world.Eye3dFrustumExterior world.Eye3dFrustumImposter lightBox set world.Octree

        static member internal getElements3dInPlay set (world : World) =
            let struct (playBox, playFrustum) = World.getPlayBounds3d world
            Octree.getElementsInPlay playBox playFrustum set world.Octree

        /// Get all 3d entities in the given bounds, including all uncullable entities.
        static member getEntities3dInBounds bounds set (world : World) =
            Octree.getElementsInBounds bounds set world.Octree
            Seq.map (fun (element : Entity Octelement) -> element.Entry) set

        /// Get all 3d entities at the given point, including all uncullable entities.
        static member getEntities3dAtPoint point set (world : World) =
            Octree.getElementsAtPoint point set world.Octree
            Seq.map (fun (element : Entity Octelement) -> element.Entry) set

        /// Get all 3d entities in the current selected screen, including all uncullable entities.
        static member getEntities3d set (world : World) =
            Octree.getElements set world.Octree
            Seq.map (fun (element : Entity Octelement) -> element.Entry) set

        /// Get all 3d entities in the current 3d view, including all uncullable entities.
        static member getEntities3dInView set (world : World) =
            let lightBox = World.getLight3dViewBox world
            Octree.getElementsInView world.Eye3dFrustumInterior world.Eye3dFrustumExterior world.Eye3dFrustumImposter lightBox set world.Octree
            Seq.map (fun (element : Entity Octelement) -> element.Entry) set

        /// Get all 3d light probe entities in the current 3d light box, including all uncullable light probes.
        static member getLightProbes3dInViewFrustum frustum set (world : World) =
            Octree.getLightProbesInViewFrustum frustum set world.Octree
            Seq.map (fun (element : Entity Octelement) -> element.Entry) set

        /// Get all 3d light probe entities in the current 3d light box, including all uncullable lights.
        static member getLightProbes3dInViewBox box set (world : World) =
            Octree.getLightProbesInViewBox box set world.Octree
            Seq.map (fun (element : Entity Octelement) -> element.Entry) set

        /// Get all 3d light probe entities in the current 3d light box, including all uncullable lights.
        static member getLightProbes3dInView set (world : World) =
            Octree.getLightProbesInView set world.Octree
            Seq.map (fun (element : Entity Octelement) -> element.Entry) set

        /// Get all 3d light entities in the current 3d light box, including all uncullable lights.
        static member getLights3dInViewFrustum frustum set (world : World) =
            Octree.getLightsInViewFrustum frustum set world.Octree
            Seq.map (fun (element : Entity Octelement) -> element.Entry) set

        /// Get all 3d light entities in the current 3d light box, including all uncullable lights.
        static member getLights3dInViewBox box set (world : World) =
            Octree.getLightsInViewBox box set world.Octree
            Seq.map (fun (element : Entity Octelement) -> element.Entry) set

        /// Get all 3d entities in the current 3d play zone, including all uncullable entities.
        static member getEntities3dInPlay set (world : World) =
            let struct (playBox, playFrustum) = World.getPlayBounds3d world
            Octree.getElementsInPlay playBox playFrustum set world.Octree
            Seq.map (fun (element : Entity Octelement) -> element.Entry) set

        /// Fetch an asset with the given tag and convert it to a value of type 'a.
        static member assetTagToValueOpt<'a> assetTag metadata world =
            match World.tryGetSymbol assetTag metadata world with
            | Some symbol ->
                try let script = symbolToValue<'a> symbol in Some script
                with exn -> Log.error ("Failed to convert symbol '" + scstring symbol + "' to value due to: " + scstring exn); None
            | None -> None

        /// Fetch assets with the given tags and convert it to values of type 'a.
        static member assetTagsToValueOpts<'a> assetTags metadata world =
            List.map (fun assetTag -> World.assetTagToValueOpt<'a> assetTag metadata world) assetTags

        static member internal tryGetGameXtensionProperty (propertyName, game, world, property : _ outref) =
            GameState.tryGetProperty (propertyName, World.getGameState game world, &property)

        static member internal getGameXtensionProperty propertyName game world =
            let mutable property = Unchecked.defaultof<_>
            match GameState.tryGetProperty (propertyName, World.getGameState game world, &property) with
            | true -> property
            | false -> failwithf "Could not find property '%s'." propertyName

        static member internal tryGetGameProperty (propertyName, game, world, property : _ outref) =
            match GameGetters.TryGetValue propertyName with
            | (true, getter) ->
                property <- getter game world
                true
            | (false, _) ->
                let gameState = World.getGameState game world
                if GameState.tryGetProperty (propertyName, gameState, &property) then
                    match property.PropertyValue with
                    | :? DesignerProperty as dp -> property <- { PropertyType = dp.DesignerType; PropertyValue = dp.DesignerValue }; true
                    | :? ComputedProperty as cp -> property <- { PropertyType = cp.ComputedType; PropertyValue = cp.ComputedGet (game :> obj) (world :> obj) }; true
                    | _ -> true
                else false

        static member internal getGameXtensionValue<'a> propertyName game world =
            let gameState = World.getGameState game world
            let mutable property = Unchecked.defaultof<_>
            if GameState.tryGetProperty (propertyName, gameState, &property) then
                let valueObj =
                    match property.PropertyValue with
                    | :? DesignerProperty as dp -> dp.DesignerValue
                    | :? ComputedProperty as cp -> cp.ComputedGet game world
                    | _ -> property.PropertyValue
                match valueObj with
                | :? 'a as value -> value
                | null -> null :> obj :?> 'a
                | value ->
                    let value =
                        try value |> valueToSymbol |> symbolToValue
                        with _ ->
                            let value = typeof<'a>.GetDefaultValue ()
                            Log.warn "Could not gracefully promote value to the required type, so using a default value instead."
                            value :?> 'a
                    match property.PropertyValue with
                    | :? DesignerProperty as dp -> dp.DesignerType <- typeof<'a>; dp.DesignerValue <- value
                    | :? ComputedProperty -> () // nothing to do
                    | _ -> property.PropertyType <- typeof<'a>; property.PropertyValue <- value
                    value
            else
                let definitions = Reflection.getPropertyDefinitions (getType gameState.Dispatcher)
                let value =
                    match List.tryFind (fun (pd : PropertyDefinition) -> pd.PropertyName = propertyName) definitions with
                    | Some definition ->
                        match definition.PropertyExpr with
                        | DefineExpr value -> value :?> 'a
                        | VariableExpr eval -> eval world :?> 'a
                        | ComputedExpr property -> property.ComputedGet game world :?> 'a
                    | None -> failwithumf ()
                let property = { PropertyType = typeof<'a>; PropertyValue = value }
                gameState.Xtension <- Xtension.attachProperty propertyName property gameState.Xtension
                value

        static member internal tryGetGameXtensionValue<'a> propertyName game world : 'a voption =
            // NOTE: we're only using exceptions as flow control in order to avoid code duplication and perf costs.
            // TODO: P1: see if we can find a way to refactor this situation without incurring any additional overhead on the getGameXtensionValue call.
            try World.getGameXtensionValue<'a> propertyName game world |> ValueSome
            with _ -> ValueNone

        static member internal getGameProperty propertyName game world =
            match GameGetters.TryGetValue propertyName with
            | (true, getter) -> getter game world
            | (false, _) -> World.getGameXtensionProperty propertyName game world

        static member internal trySetGameXtensionPropertyWithoutEvent propertyName (property : Property) gameState game world =
            let mutable propertyOld = Unchecked.defaultof<_>
            match GameState.tryGetProperty (propertyName, gameState, &propertyOld) with
            | true ->
                match propertyOld.PropertyValue with
                | :? DesignerProperty as dp ->
                    let previous = dp.DesignerValue
                    if property.PropertyValue =/= previous then
                        let property = { property with PropertyValue = { dp with DesignerValue = property.PropertyValue }}
                        match GameState.trySetProperty propertyName property gameState with
                        | struct (true, gameState) ->
                            World.setGameState gameState game world
                            struct (true, true, previous)
                        | struct (false, _) -> struct (false, false, previous)
                    else (true, false, previous)
                | :? ComputedProperty as cp ->
                    match cp.ComputedSetOpt with
                    | Some computedSet ->
                        let previous = cp.ComputedGet (box game) (box world)
                        if property.PropertyValue =/= previous then
                            computedSet property.PropertyValue game world |> ignore<obj> // TODO: P0: move related type definitions into Nu from Prime and modify them to match mutable usage.
                            struct (true, true, previous)
                        else struct (true, false, previous)
                    | None -> struct (false, false, Unchecked.defaultof<_>)
                | _ ->
                    let previous = propertyOld.PropertyValue
                    if property.PropertyValue =/= previous then
                        match GameState.trySetProperty propertyName property gameState with
                        | struct (true, gameState) ->
                            World.setGameState gameState game world
                            struct (true, true, previous)
                        | struct (false, _) -> struct (false, false, previous)
                    else struct (true, false, previous)
            | false -> struct (false, false, Unchecked.defaultof<_>)

        static member internal trySetGameXtensionPropertyFast propertyName (property : Property) game world =
            let gameState = World.getGameState game world
            match World.trySetGameXtensionPropertyWithoutEvent propertyName property gameState game world with
            | struct (true, changed, previous) ->
                if changed then
                    World.publishGameChange propertyName previous property.PropertyValue game world
            | struct (false, _, _) -> ()

        static member internal trySetGameXtensionProperty propertyName (property : Property) game world =
            let gameState = World.getGameState game world
            match World.trySetGameXtensionPropertyWithoutEvent propertyName property gameState game world with
            | struct (true, changed, previous) ->
                if changed then
                    World.publishGameChange propertyName previous property.PropertyValue game world
                struct (true, changed)
            | struct (false, changed, _) -> struct (false, changed)

        static member internal trySetGameXtensionValue<'a> propertyName (value : 'a) game world =
            let property = { PropertyType = typeof<'a>; PropertyValue = value }
            World.trySetGameXtensionProperty propertyName property game world

        static member internal setGameXtensionValue<'a> propertyName (value : 'a) game world =
            let gameState = World.getGameState game world
            let propertyOld = GameState.getProperty propertyName gameState
            let mutable previous = Unchecked.defaultof<obj> // OPTIMIZATION: avoid passing around structs.
            let mutable changed = false // OPTIMIZATION: avoid passing around structs.
            match propertyOld.PropertyValue with
            | :? DesignerProperty as dp ->
                previous <- dp.DesignerValue
                if value =/= previous then
                    changed <- true
                    let property = { propertyOld with PropertyValue = { dp with DesignerValue = value }}
                    let gameState = GameState.setProperty propertyName property gameState
                    World.setGameState gameState game world
            | :? ComputedProperty as cp ->
                match cp.ComputedSetOpt with
                | Some computedSet ->
                    previous <- cp.ComputedGet (box game) (box world)
                    if value =/= previous then
                        changed <- true
                        computedSet propertyOld.PropertyValue game world |> ignore<obj> // TODO: P0: move related type definitions into Nu from Prime and modify them to match mutable usage.
                | None -> ()
            | _ ->
                previous <- propertyOld.PropertyValue
                if value =/= previous then
                    changed <- true
                    let property = { propertyOld with PropertyValue = value }
                    let gameState = GameState.setProperty propertyName property gameState
                    World.setGameState gameState game world
            if changed then
                World.publishGameChange propertyName previous value game world

        static member internal setGameXtensionProperty propertyName (property : Property) game world =
            let gameState = World.getGameState game world
            let propertyOld = GameState.getProperty propertyName gameState
            if property.PropertyValue =/= propertyOld.PropertyValue then
                let gameState = GameState.setProperty propertyName property gameState
                World.setGameState gameState game world
                World.publishGameChange propertyName propertyOld.PropertyValue property.PropertyValue game world
                true
            else false

        static member internal trySetGamePropertyFast propertyName property game world =
            match GameSetters.TryGetValue propertyName with
            | (true, setter) -> setter property game world |> ignore<bool>
            | (false, _) -> World.trySetGameXtensionPropertyFast propertyName property game world

        static member internal trySetGameProperty propertyName property game world =
            match GameSetters.TryGetValue propertyName with
            | (true, setter) ->
                let changed = setter property game world
                struct (true, changed)
            | (false, _) ->
                World.trySetGameXtensionProperty propertyName property game world

        static member internal setGameProperty propertyName property game world =
            match GameSetters.TryGetValue propertyName with
            | (true, setter) -> setter property game world
            | (false, _) -> World.setGameXtensionProperty propertyName property game world

        static member internal attachGameMissingProperties game world =
            let gameState = World.getGameState game world
            let definitions = Reflection.getReflectivePropertyDefinitions gameState
            let gameState =
                Map.fold (fun gameState propertyName (propertyDefinition : PropertyDefinition) ->
                    let mutable property = Unchecked.defaultof<_>
                    if not (World.tryGetGameProperty (propertyName, game, world, &property)) then
                        let propertyValue = PropertyExpr.eval propertyDefinition.PropertyExpr world
                        let property = { PropertyType = propertyDefinition.PropertyType; PropertyValue = propertyValue }
                        GameState.attachProperty propertyName property gameState
                    else gameState)
                    gameState definitions
            World.setGameState gameState game world

        static member internal viewGameProperties game world =
            let state = World.getGameState game world
            World.viewSimulantStateProperties state

        static member notifyGameModelChange game world =
            let gameState = World.getGameState game world
            gameState.Dispatcher.TrySynchronize (false, false, game, world)
            World.publishGameChange Constants.Engine.ModelPropertyName gameState.Model.DesignerValue gameState.Model.DesignerValue game world

    /// Initialize property getters.
    let private initGetters () =
        let gameGetters =
            dictPlus StringComparer.Ordinal
                [("Dispatcher", fun game world -> { PropertyType = typeof<GameDispatcher>; PropertyValue = World.getGameDispatcher game world })
                 ("Model", fun game world -> let designerProperty = World.getGameModelProperty game world in { PropertyType = designerProperty.DesignerType; PropertyValue = designerProperty.DesignerValue })
                 ("SelectedScreenOpt", fun game world -> { PropertyType = typeof<Screen option>; PropertyValue = World.getGameSelectedScreenOpt game world })
                 ("DesiredScreen", fun game world -> { PropertyType = typeof<DesiredScreen>; PropertyValue = World.getGameDesiredScreen game world })
                 ("ScreenTransitionDestinationOpt", fun game world -> { PropertyType = typeof<Screen option>; PropertyValue = World.getGameScreenTransitionDestinationOpt game world })
                 ("Eye2dCenter", fun game world -> { PropertyType = typeof<Vector2>; PropertyValue = World.getGameEye2dCenter game world })
                 ("Eye2dSize", fun game world -> { PropertyType = typeof<Vector2>; PropertyValue = World.getGameEye2dSize game world })
                 ("Eye3dCenter", fun game world -> { PropertyType = typeof<Vector3>; PropertyValue = World.getGameEye3dCenter game world })
                 ("Eye3dRotation", fun game world -> { PropertyType = typeof<Quaternion>; PropertyValue = World.getGameEye3dRotation game world })
                 ("Eye3dFieldOfView", fun game world -> { PropertyType = typeof<single>; PropertyValue = World.getGameEye3dFieldOfView game world })
                 ("Order", fun game world -> { PropertyType = typeof<int64>; PropertyValue = World.getGameOrder game world })
                 ("Id", fun game world -> { PropertyType = typeof<Guid>; PropertyValue = World.getGameId game world })]
        GameGetters <- gameGetters.ToFrozenDictionary ()

    /// Initialize property setters.
    let private initSetters () =
        let gameSetters =
            dictPlus StringComparer.Ordinal
                [("Model", fun property game world -> World.setGameModelProperty false false { DesignerType = property.PropertyType; DesignerValue = property.PropertyValue } game world)
                 ("DesiredScreen", fun property game world -> World.setGameDesiredScreen (property.PropertyValue :?> DesiredScreen) game world)
                 ("ScreenTransitionDestinationOpt", fun property game world -> World.setGameScreenTransitionDestinationOpt (property.PropertyValue :?> Screen option) game world)
                 ("Eye2dCenter", fun property game world -> World.setGameEye2dCenter (property.PropertyValue :?> Vector2) game world)
                 ("Eye2dSize", fun property game world -> World.setGameEye2dSize (property.PropertyValue :?> Vector2) game world)
                 ("Eye3dCenter", fun property game world -> World.setGameEye3dCenter (property.PropertyValue :?> Vector3) game world)
                 ("Eye3dRotation", fun property game world -> World.setGameEye3dRotation (property.PropertyValue :?> Quaternion) game world)
                 ("Eye3dFieldOfView", fun property game world -> World.setGameEye3dFieldOfView (property.PropertyValue :?> single) game world)]
        GameSetters <- gameSetters.ToFrozenDictionary ()

    /// Initialize getters and setters
    let internal init () =
        initGetters ()
        initSetters ()