// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu.Constants
open System
open System.Configuration
open Prime
open Nu

[<RequireQualifiedAccess>]
module Dissolve =

    /// The default 'dissolving' transition behavior of screens.
    let Default =
        { IncomingTime = GameTime.ofSeconds 0.5f
          OutgoingTime = GameTime.ofSeconds 1.0f
          DissolveImage = Assets.Default.Black }

[<RequireQualifiedAccess>]
module Slide =

    /// The default 'slide shot' behavior of slide screens.
    let Default =
        { DissolveDescriptor = Dissolve.Default
          IdlingTime = GameTime.ofSeconds 1.0f
          SlideImageOpt = Some Assets.Default.NuSlide }

[<RequireQualifiedAccess>]
module Override =

    /// Override certain constants with values from an App.config file of the given executable assembly.
    let fromAppConfig (exeFilePath : string) =
        try let configuration = ConfigurationManager.OpenExeConfiguration exeFilePath
            let settings = configuration.AppSettings.Settings
            for key in settings.AllKeys do
                let value = settings.[key].Value
                match key with
                | nameof GameTime.DesiredFrameRate -> GameTime.DesiredFrameRate <- scvalue value
                | nameof OpenGL.HlAssert -> OpenGL.HlAssert <- scvalue value
                | nameof Engine.RunSynchronously -> Engine.RunSynchronously <- scvalue value
                | nameof Engine.EntityPerimeterCentered2dDefault -> Engine.EntityPerimeterCentered2dDefault <- scvalue value
                | nameof Engine.EntityPerimeterCenteredGuiDefault -> Engine.EntityPerimeterCenteredGuiDefault <- scvalue value
                | nameof Engine.QuadnodeSize -> Engine.QuadnodeSize <- scvalue value
                | nameof Engine.QuadtreeDepth -> Engine.QuadtreeDepth <- scvalue value
                | nameof Engine.OctnodeSize -> Engine.OctnodeSize <- scvalue value
                | nameof Engine.OctreeDepth -> Engine.OctreeDepth <- scvalue value
                | nameof Engine.EventTracing -> Engine.EventTracing <- scvalue value
                | nameof Engine.EventFilter -> Engine.EventFilter <- scvalue value
                | nameof Render.Vsync -> Render.Vsync <- scvalue value
                | nameof Render.NearPlaneDistanceInterior -> Render.NearPlaneDistanceInterior <- scvalue value
                | nameof Render.FarPlaneDistanceInterior -> Render.FarPlaneDistanceInterior <- scvalue value
                | nameof Render.NearPlaneDistanceExterior -> Render.NearPlaneDistanceExterior <- scvalue value
                | nameof Render.FarPlaneDistanceExterior -> Render.FarPlaneDistanceExterior <- scvalue value
                | nameof Render.NearPlaneDistanceImposter -> Render.NearPlaneDistanceImposter <- scvalue value
                | nameof Render.FarPlaneDistanceImposter -> Render.FarPlaneDistanceImposter <- scvalue value
                | nameof Render.SsaoResolutionDivisor -> Render.SsaoResolutionDivisor <- scvalue value
                | nameof Render.ShadowDetailedCount -> Render.ShadowDetailedResolutionScalar <- scvalue value
                | nameof Render.ShadowDetailedResolutionScalar -> Render.ShadowDetailedResolutionScalar <- scvalue value
                | nameof Render.ShadowsMax -> Render.ShadowsMax <- min (scvalue value) Constants.Render.ShadowsMaxShader
                | _ -> ()
        with
        | :? ConfigurationErrorsException ->
            Log.info ("Configuration value override failed due to: Could not find App.config file for " + exeFilePath + ".")
        | exn ->
            Log.info ("Configuration value override failed due to: " + scstring exn)