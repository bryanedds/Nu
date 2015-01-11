// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2014.

(* IMPORTANT NOTE: change these paths to make this script run with your Prime installation. *)
#r "C:/Development/FPWorks/Prime/FSharpx.Core/FSharpx.Core.dll"
#r "C:/Development/FPWorks/Prime/FSharpx.Collections/FSharpx.Collections.dll"
#r "C:/Development/FPWorks/Prime/FParsec/FParsecCS.dll" // MUST be referenced BEFORE FParsec.dll!
#r "C:/Development/FPWorks/Prime/FParsec/FParsec.dll"
#r "C:/Development/FPWorks/Prime/xUnit/xunit.dll"

#load "Log.fs"
#load "ReferenceEquality.fs"
#load "Miscellanea.fs"
#load "Sectioning.fs"
#load "Option.fs"
#load "Pair.fs"
#load "Triple.fs"
#load "Seq.fs"
#load "Array.fs"
#load "String.fs"
#load "List.fs"
#load "Set.fs"
#load "Map.fs"
#load "PersistentHashMap.fs"
#load "HashSet.fs"
#load "Dictionary.fs"
#load "Lens.fs"
#load "AlgebraicReader.fs"
#load "AlgebraicConverter.fs"
#load "AlgebraicString.fs"
#load "Either.fs"
#load "MapPlus.fs"
#load "RduTree.fs"
#load "Rand.fs"
#load "Desync.fs"

open System
open FSharpx
open FParsec
open Prime
open Prime.Desync

