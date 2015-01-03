#nowarn "9"
#r "System.Configuration"

(* IMPORTANT NOTE: change these paths to make this script run with your Nu installation! *)
#r "C:/Development/FPWorks/Prime/xUnit/xunit.dll"
#r "C:/Development/FPWorks/Prime/FParsec/FParsecCS.dll" // MUST be referenced BEFORE FParsec.dll!
#r "C:/Development/FPWorks/Prime/FParsec/FParsec.dll"
#r "C:/Development/FPWorks/Prime/FSharpx.Core/FSharpx.Core.dll"
#r "C:/Development/FPWorks/Prime/Prime/Prime/bin/Debug/Prime.exe"
#r "C:/Development/FPWorks/Prime/FSharpx.Core/FSharpx.Core.dll"
#r "C:/Development/FPWorks/Prime/FSharpx.Collections/FSharpx.Collections.dll"
#r "C:/Development/FPWorks/Nu/xUnit/xunit.dll"
#r "C:/Development/FPWorks/Nu/Farseer/FarseerPhysics.dll"
#r "C:/Development/FPWorks/Nu/Magick.NET/Magick.NET-AnyCPU.dll"
#r "C:/Development/FPWorks/Nu/SDL2#/Debug/SDL2#.dll"
#r "C:/Development/FPWorks/Nu/TiledSharp/Debug/TiledSharp.dll"
#r "C:/Development/FPWorks/SDL2Addendum/SDL2Addendum/SDL2Addendum/bin/Debug/SDL2Addendum.dll"
System.IO.Directory.SetCurrentDirectory "C:/Development/FPWorks/Nu/Nu/Nu/bin/Debug"

#load "RQueue.fs"
#load "Address.fs"
#load "Core.fs"
#load "Constants.fs"
#load "Math.fs"
#load "Xtension.fs"
#load "Overlayer.fs"
#load "OverlayRouter.fs"
#load "Serialization.fs"
#load "Reflection.fs"
#load "Camera.fs"
#load "Assets.fs"
#load "Physics.fs"
#load "Render.fs"
#load "Audio.fs"
#load "Metadata.fs"
#load "Input.fs"
#load "Sdl.fs"
#load "Simulation.fs"
#load "Simulant.fs"
#load "WorldConstants.fs"
#load "WorldPrimitives.fs"
#load "Entity.fs"
#load "Group.fs"
#load "Screen.fs"
#load "Game.fs"
#load "Dispatchers.fs"
#load "World.fs"
#load "Observation.fs"
#load "Desync.fs"

open System
open FParsec
open SDL2
open OpenTK
open TiledSharp
open Prime
open Prime.Desync
open Nu
open Nu.Constants
open Nu.WorldConstants
open Nu.Observation
open Nu.Desync

