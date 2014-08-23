#r "System.Configuration"

(* IMPORTANT NOTE: change these paths to make this script run with your Nu installation! *)
#r "C:/Development/FPWorks/Prime/xUnit/xunit.dll"
#r "C:/Development/FPWorks/Prime/FSharpx.Core/FSharpx.Core.dll"
#r "C:/Development/FPWorks/Prime/Prime/Prime/bin/Debug/Prime.exe"
#r "C:/Development/FPWorks/Nu/xUnit/xunit.dll"
#r "C:/Development/FPWorks/Nu/FSharpx.Core/FSharpx.Core.dll"
#r "C:/Development/FPWorks/Nu/Farseer/FarseerPhysics.dll"
#r "C:/Development/FPWorks/Nu/SDL2#/Debug/SDL2#.dll"
#r "C:/Development/FPWorks/Nu/TiledSharp/Debug/TiledSharp.dll"
System.IO.Directory.SetCurrentDirectory "C:/Development/FPWorks/Nu/Nu/Nu/bin/Debug"

#load "RQueue.fs"
#load "NuCore.fs"
#load "NuConstants.fs"
#load "NuMath.fs"
#load "Camera.fs"
#load "Assets.fs"
#load "Physics.fs"
#load "Rendering.fs"
#load "Audio.fs"
#load "Metadata.fs"
#load "Input.fs"
#load "Sdl.fs"
#load "Sim.fs"
#load "Entity.fs"
#load "Group.fs"
#load "Screen.fs"
#load "Game.fs"
#load "Dispatchers.fs"
#load "World.fs"

open System
open SDL2
open OpenTK
open TiledSharp
open Nu
open NuConstants