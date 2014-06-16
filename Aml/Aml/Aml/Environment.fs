// Aml - A Modular Language.
// Copyright (C) Bryan Edds, 2012-2014.

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
            (Dictionary<string, EnvEntry> ())
            []
            (List<CachedEntry ref> ())
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

        // make the core envrionment
        let env =
            makeEnv
                (List.toDictionary InitialEntriesList)
                []
                (List<CachedEntry ref> ())
                false
                (makeEmptyDebugInfo ())
                Set.empty
                []
                None
                None
                (Directory.GetCurrentDirectory ())
                0
                []

        // add equatable protocol declaration
        let env = declareEquatable env

        // add equatable protocol instantiations for primitives types
        let env =
            List.fold
                (fun env initialType ->
                    let typeName = a__ initialType : string
                    let rawTypeName = typeName.Substring TypePrefixStr.Length
                    instantiateEquatable rawTypeName env)
                env
                InitialTypes

        // present the full initial environment
        env