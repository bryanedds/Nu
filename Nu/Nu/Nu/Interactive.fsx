// Nu Game Engine.
// Copyright (C) Bryan Edds, 2012-2016.

#I __SOURCE_DIRECTORY__
#r "System.Configuration"
#r "../../../Prime/FParsec/FParsecCS.dll" // MUST be referenced BEFORE FParsec.dll!
#r "../../../Prime/FParsec/FParsec.dll"
#r "../../../Prime/xUnit/xunit.dll"
#r "../../../Nu/FSharpx.Core/FSharpx.Core.dll"
#r "../../../Nu/FSharpx.Collections/FSharpx.Collections.dll"
#r "../../../Nu/Prime/Release/Prime.exe"
#r "../../../Nu/Farseer/FarseerPhysics.dll"
#r "../../../Nu/Magick.NET/Magick.NET-AnyCPU.dll"
#r "../../../Nu/SDL2#/Release/SDL2#.dll"
#r "../../../Nu/TiledSharp/Release/TiledSharp.dll"
#r "../../../SDL2Addendum/SDL2Addendum/SDL2Addendum/bin/Debug/SDL2Addendum.dll"
#r "../../../Nu/Nu/Nu/bin/Debug/Nu.exe"

open System
open System.IO
open FSharpx
open FSharpx.Collections
open SDL2
open OpenTK
open TiledSharp
open Prime
open Prime.Observation
open Prime.Chain
open Nu

// set current directly to local for execution in VS F# interactive
Directory.SetCurrentDirectory ^ __SOURCE_DIRECTORY__ + "../bin/Debug"

// initialize Nu
Nu.init false