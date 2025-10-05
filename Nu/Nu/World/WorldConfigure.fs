// Nu Game Engine.
// Copyright (C) Bryan Edds.

namespace Nu
open System
open System.Configuration
open Prime
open Nu

/// Provides engine-specific configuration functionality.
[<RequireQualifiedAccess>]
module Configure =

    /// Override certain bindings with values from an App.config file of the given executable assembly. May be called
    /// only at the start of a Nu program.
    let fromAppConfig (exeFilePath : string) =
        try let configuration = ConfigurationManager.OpenExeConfiguration exeFilePath
            let settings = configuration.AppSettings.Settings
            for key in settings.AllKeys do
                let value = settings.[key].Value
                match key with
                | nameof Constants.Runtime.GcDebug -> Constants.Runtime.GcDebug <- scvalue value
                | nameof Constants.GameTime.DesiredFrameRate -> Constants.GameTime.DesiredFrameRate <- scvalue value
                | nameof Constants.OpenGL.HlDebug -> Constants.OpenGL.HlDebug <- scvalue value
                | nameof Constants.ImGui.FontSize -> Constants.ImGui.FontSize <- scvalue value
                | nameof Constants.Engine.Meter2d -> Constants.Engine.Meter2d <- scvalue value
                | nameof Constants.Engine.RunSynchronously -> Constants.Engine.RunSynchronously <- scvalue value
                | nameof Constants.Engine.TickDeltaAveraging -> Constants.Engine.TickDeltaAveraging <- scvalue value
                | nameof Constants.Engine.QuadnodeSize -> Constants.Engine.QuadnodeSize <- scvalue value
                | nameof Constants.Engine.QuadtreeDepth -> Constants.Engine.QuadtreeDepth <- scvalue value
                | nameof Constants.Engine.OctnodeSize -> Constants.Engine.OctnodeSize <- scvalue value
                | nameof Constants.Engine.OctreeDepth -> Constants.Engine.OctreeDepth <- scvalue value
                | nameof Constants.Engine.EventTracing -> Constants.Engine.EventTracing <- scvalue value
                | nameof Constants.Engine.EventFilter -> Constants.Engine.EventFilter <- scvalue value
                | nameof Constants.Render.Vsync -> Constants.Render.Vsync <- scvalue value
                | nameof Constants.Render.NearPlaneDistanceInterior -> Constants.Render.NearPlaneDistanceInterior <- scvalue value
                | nameof Constants.Render.FarPlaneDistanceInterior -> Constants.Render.FarPlaneDistanceInterior <- scvalue value
                | nameof Constants.Render.NearPlaneDistanceExterior -> Constants.Render.NearPlaneDistanceExterior <- scvalue value
                | nameof Constants.Render.FarPlaneDistanceExterior -> Constants.Render.FarPlaneDistanceExterior <- scvalue value
                | nameof Constants.Render.NearPlaneDistanceImposter -> Constants.Render.NearPlaneDistanceImposter <- scvalue value
                | nameof Constants.Render.FarPlaneDistanceImposter -> Constants.Render.FarPlaneDistanceImposter <- scvalue value
                | nameof Constants.Render.DisplayVirtualResolution -> Constants.Render.DisplayVirtualResolution <- scvalue value
                | nameof Constants.Render.SsaoResolutionDivisor -> Constants.Render.SsaoResolutionDivisor <- scvalue value
                | nameof Constants.Render.TextureAnisotropyMax -> Constants.Render.TextureAnisotropyMax <- scvalue value
                | nameof Constants.Render.TextureMinimalMipmapIndex -> Constants.Render.TextureMinimalMipmapIndex <- scvalue value
                | nameof Constants.Render.SpineSkeletonScalar -> Constants.Render.SpineSkeletonScalar <- scvalue value
                | nameof Constants.Render.ShadowVirtualResolution -> Constants.Render.ShadowVirtualResolution <- scvalue value
                | nameof Constants.Render.ShadowDisplayScalarMax -> Constants.Render.ShadowDisplayScalarMax <- scvalue value
                | nameof Constants.Render.ShadowDirectionalMarginRatioCull -> Constants.Render.ShadowDirectionalMarginRatioCull <- scvalue value
                | nameof Constants.Render.ShadowCascadeLimits -> Constants.Render.ShadowCascadeLimits <- scvalue value
                | nameof Constants.Render.ShadowCascadeMarginRatio -> Constants.Render.ShadowCascadeMarginRatio <- scvalue value
                | nameof Constants.Render.ShadowCascadeMarginRatioCull -> Constants.Render.ShadowCascadeMarginRatioCull <- scvalue value
                | nameof Constants.Physics.Collision3dBodiesMax -> Constants.Physics.Collision3dBodiesMax <- scvalue value
                | nameof Constants.Physics.Collision3dBodyPairsMax -> Constants.Physics.Collision3dBodyPairsMax <- scvalue value
                | nameof Constants.Physics.Collision3dContactConstraintsMax -> Constants.Physics.Collision3dContactConstraintsMax <- scvalue value
                | nameof Constants.Physics.Collision3dSteps -> Constants.Physics.Collision3dSteps <- scvalue value
                | nameof Constants.Physics.Collision3dThreads -> Constants.Physics.Collision3dThreads <- scvalue value
                | nameof Constants.Physics.Collision3dBarriersMax -> Constants.Physics.Collision3dBarriersMax <- scvalue value
                | nameof Constants.Physics.Collision3dJobsMax -> Constants.Physics.Collision3dJobsMax <- scvalue value
                | nameof Constants.Physics.GroundAngleMax -> Constants.Physics.GroundAngleMax <- scvalue value
                | nameof Constants.Gui.SliceMarginDefault -> Constants.Gui.SliceMarginDefault <- scvalue value
                | nameof Globals.Render.DisplayScalar -> Globals.Render.DisplayScalar <- scvalue value
                | nameof Globals.Render.ShadowScalar -> Globals.Render.ShadowScalar <- scvalue value
                | _ -> ()
            Constants.Render.NearPlaneDistanceOmnipresent <- Constants.Render.NearPlaneDistanceInterior
            Constants.Render.FarPlaneDistanceOmnipresent <- Constants.Render.FarPlaneDistanceImposter
        with
        | :? ConfigurationErrorsException ->
            Log.info ("Configuration value override failed due to: Could not find App.config file for " + exeFilePath + ".")
        | exn ->
            Log.info ("Configuration value override failed due to: " + scstring exn)