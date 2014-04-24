// Aml - A Modular Language.
// Copyright (C) Bryan Edds, 2012-2013.

namespace Aml
open System
open System.IO
open System.Collections
open System.Collections.Generic
open Prime
open Aml.Ast
open Aml.AmlConstants
open Aml.Initial
open Aml.Primitives
open Aml.Evaluator
module Environment =

    /// Make empty debug info.
    let makeEmptyDebugInfo () =
        makeDebugInfo None [] []

    /// Make an empty environment.
    let makeEmptyEnv () =
        makeEnv
            (Dictionary<Lun, EnvEntry> ())
            []
            (Generic.List<CachedEntry ref> ())
            false
            (makeEmptyDebugInfo ())
            Set.empty
            []
            None
            None
            (Directory.GetCurrentDirectory ())
            0
            []

    /// Make an initial global environment.
    let makeInitialEnv () =
        let initialDirectory = Directory.GetCurrentDirectory ()
        let initialDeclarationFrame = List.toDictionary InitialEntriesList
        let debugInfo = makeEmptyDebugInfo ()
        let env =
            makeEnv
                initialDeclarationFrame
                []
                (Generic.List<CachedEntry ref> ())
                false
                debugInfo
                Set.empty
                []
                None
                None
                initialDirectory
                0
                []
        let newEnv =
            declareEquatable env
        let newEnv2 =
            List.fold
                (fun env initialType ->
                    let typeNameStr = (a__ initialType).LunStr
                    let rawTypeNameStr = typeNameStr.Substring TypePrefixStr.Length
                    let rawTypeName = Lun.make rawTypeNameStr
                    instantiateEquatable env rawTypeName)
                newEnv
                InitialTypes
        newEnv2