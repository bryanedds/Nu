// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

#I __SOURCE_DIRECTORY__
#r "nuget: Aether.Physics2D, 1.7.0"
#r "nuget: Csv, 1.0.58"
#r "nuget: FParsec, 1.0.3"
#r "nuget: Twizzle.ImGuizmo.NET, 1.89.4.1"
#r "nuget: Magick.NET-Q8-x64, 7.5.0.1"
#r "nuget: Prime, 9.13.1"
#r "nuget: Prime.Scripting, 9.12.0"
#r "nuget: System.Configuration.ConfigurationManager, 7.0.0"
#r "../../../Nu/Nu.Dependencies/AssimpNet/netstandard1.3/AssimpNet.dll"
#r "../../../Nu/Nu.Dependencies/BulletSharpPInvoke/netstandard2.1/BulletSharp.dll"
#r "../../../Nu/Nu.Dependencies/OpenGL.NET/lib/netcoreapp2.2/OpenGL.Net.dll"
#r "../../../Nu/Nu.Dependencies/SDL2-CS/netstandard2.0/SDL2-CS.dll"
#r "../../../Nu/Nu.Dependencies/TiledSharp/lib/netstandard2.0/TiledSharp.dll"
#r "../../../Nu/Nu.Math/bin/Debug/netstandard2.0/Nu.Math.dll"
#r "../../../Nu/Nu/bin/Debug/net7.0/Nu.dll"

namespace Namespace
open System
open Prime
open Nu

[<AutoOpen>]
module TemplateDispatcher3d =

    type TemplateModel =
        { StaticModel : StaticModel AssetTag }

    type TemplateMessage =
        | Update
        interface Message

    type TemplateCommand =
        | Unregistering
        interface Command

    type TemplateDispatcher3d () =
        inherit EntityDispatcher3d<TemplateModel, TemplateMessage, TemplateCommand> (false, { StaticModel = Assets.Default.StaticModel })

        static member Facets =
            [typeof<StaticModelFacet>]

        override this.Initialize (template, entity) =
            [Entity.StaticModel := template.StaticModel
             Entity.UpdateEvent => Update
             Entity.UnregisteringEvent => Unregistering]

        override this.Message (template, message, entity, world) =
            match message with
            | Update ->
                // TODO: optionally add code to update your model every tick here.
                just template

        override this.Command (template, command, entity, world) =
            match command with
            | Unregistering ->
                // TODO: optionally add code to handle unregistering the entity here.
                just world

        override this.Content (model, entity) =
            [] // TODO: optionally add child entities here.