// TODO: fix this script

// IMPORTANT!
// NOTE: this MUST be run in the 64-bit FSI (In VS, Tools -> Options... -> F# Tools -> 64-bit F# Interactive [set to true]).
// IMPORTANT!
#r "System.Configuration"

// IMPORTANT!
// NOTE: change these paths to make this script run with your Nu installation.
// IMPORTANT!
#r "C:/OmniBlade/Prime/xUnit/xunit.dll"
#r "C:/OmniBlade/Prime/FSharpx.Core/FSharpx.Core.dll"
#r "C:/OmniBlade/Prime/Prime/Prime/bin/Debug/Prime.dll"
#r "C:/OmniBlade/Nu/xUnit/xunit.dll"
#r "C:/OmniBlade/Nu/FSharpx.Core/FSharpx.Core.dll"
#r "C:/OmniBlade/Nu/Farseer/FarseerPhysics.dll"
#r "C:/OmniBlade/Nu/SDL2#/Debug/SDL2#.dll"
#r "C:/OmniBlade/Nu/TiledSharp/Debug/TiledSharp.dll"
System.IO.Directory.SetCurrentDirectory "C:/OmniBlade/Nu/Nu/Nu/bin/Release"

#load "Core.fs"
#load "RQueue.fs"
#load "Constants.fs"
#load "Math.fs"
#load "Voords.fs"
#load "Assets.fs"
#load "Physics.fs"
#load "Rendering.fs"
#load "Audio.fs"
#load "Metadata.fs"
#load "Input.fs"
#load "Camera.fs"
#load "DomainModel.fs"
#load "Sdl.fs"
#load "OmniTypes.fs"
#load "OmniData.fs"
#load "OmniState.fs"
#load "Simulation.fs"
#load "Entities.fs"
#load "Groups.fs"
#load "Screens.fs"
#load "Games.fs"
#load "World.fs"
#load "OmniBlade.fs"

open System
open SDL2
open OpenTK
open TiledSharp
open Nu