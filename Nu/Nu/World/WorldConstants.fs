// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu.Constants
open System.Configuration
open Prime
open Nu

[<RequireQualifiedAccess>]
module Dissolve =

    /// The default 'dissolving' transition behavior of screens.
    let Default =
        { IncomingTime =
            match Constants.Engine.DesiredFps with
            | LimitTo30 -> Frames 10L
            | LimitTo60 -> Frames 20L
            | Unlimited -> Milliseconds (1.0f / 3.0f)
          OutgoingTime =
            match Constants.Engine.DesiredFps with
            | LimitTo30 -> Frames 20L
            | LimitTo60 -> Frames 40L
            | Unlimited -> Milliseconds (2.0f / 3.0f)
          DissolveImage = Assets.Default.Image8 }

[<RequireQualifiedAccess>]
module Slide =

    /// The default 'slide shot' behavior of slide screens.
    let Default =
        { DissolveDescriptor = Dissolve.Default
          IdlingTime =
            match Constants.Engine.DesiredFps with
            | LimitTo30 -> Frames 30L
            | LimitTo60 -> Frames 60L
            | Unlimited -> Milliseconds 1.0f
          SlideImageOpt = Some Assets.Default.Image5 }

[<RequireQualifiedAccess>]
module Override =

    /// Override certain constants with values from an App.config file of the given executable assembly.
    let fromAppConfig (exeFilePath : string) =
        try let configuration = ConfigurationManager.OpenExeConfiguration exeFilePath
            let settings = configuration.AppSettings.Settings
            for key in settings.AllKeys do
                let value = settings.[key].Value
                match key with
                | nameof Engine.DesiredFps -> Engine.DesiredFps <- scvalue value
                | nameof Engine.EntityCentered2dDefault -> Engine.EntityCentered2dDefault <- scvalue value
                | nameof Engine.EntityCenteredGuiDefault -> Engine.EntityCenteredGuiDefault <- scvalue value
                | nameof Engine.EntityCentered3dDefault -> Engine.EntityCentered3dDefault <- scvalue value
                | nameof Engine.EventTracing -> Engine.EventTracing <- scvalue value
                | nameof Engine.EventFilter -> Engine.EventFilter <- scvalue value
                | _ -> ()
        with
        | :? ConfigurationErrorsException ->
            Log.info ("Configuration value override failed due to: Could not find App.config file for " + exeFilePath + ".")
        | exn ->
            Log.info ("Configuration value override failed due to: " + scstring exn)