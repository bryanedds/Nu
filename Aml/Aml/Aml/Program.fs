// Aml - A Modular Language.
// Copyright (C) Bryan Edds, 2012-2015.

namespace Aml
open System
open System.Runtime
open Prime
open Aml
open Aml.Ast
open Aml.AmlConstants
open Aml.Initial
open Aml.Primitives
open Aml.Evaluator
open Aml.Repl
open Aml.Environment
module Program =

    // (let (c (ref 8000000)) (while! (i/= (get c) 0) (set! c (iDec (get c)))))
    // (let (c (ref 3800000)) (while! (/= (get c) 0) (set! c (dec (get c)))))

    // GCSettings.LatencyMode <- GCLatencyMode.SustainedLowLatency

    let [<Literal>] StdlibPath = "../../Stdlib/Stdlib.aml"

    let echoIntroduction () =
        Console.ForegroundColor <- NormalConsoleColor
        Console.WriteLine "Welcome to Aml (A Modular Language)."
        Console.WriteLine "Designed and implemented by Bryan Edds, 2012-2015."
        Console.WriteLine ()
        Console.WriteLine "NOTE: The best way to learn Aml currently is to study the *.aml files in"
        Console.WriteLine "the Stdlib directory, and to read AmlSpec.rtf in the Documentation"
        Console.WriteLine "directory."
        Console.WriteLine ()
        Console.WriteLine ("Current version is " + (let v = AmlVersion in v.ToString "F3") + ".")
        Console.WriteLine ()
        Console.WriteLine "Loading Stdlib..."

    let echoStdlibSuccess () =
        Console.WriteLine "Stdlib loaded!"
        Console.WriteLine ()

    let echoStdlibFailure (message : string) =
        Console.ForegroundColor <- ErrorConsoleColor
        Console.WriteLine "Stdlib failed to load due to:"
        Console.WriteLine message
        Console.WriteLine ()
        Console.ForegroundColor <- NormalConsoleColor

    let echoPrompt () =
        Console.WriteLine "Enter Aml expression(s) at the prompt or '?quit' to quit."

    let loadStdlib () =
        let stdlibEvalResult = evalUsingFile (makeUsingFileRecord StdlibPath false None) (makeInitialEnv ())
        match stdlibEvalResult.Value with
        | Violation violation -> echoStdlibFailure violation.VioMessage.SVValue
        | _ -> echoStdlibSuccess ()
        echoPrompt ()
        stdlibEvalResult

    let [<EntryPoint>] main _ =
        echoIntroduction ()
        let stdlibEvalResult = loadStdlib ()
        repl stdlibEvalResult.Env
        0