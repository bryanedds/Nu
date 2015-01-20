// Aml - A Modular Language.
// Copyright (C) Bryan Edds, 2012-2015.

namespace Aml
open System
open FParsec.CharParsers
open Prime
open Aml.Ast
open Aml.AmlConstants
open Aml.Primitives
open Aml.Reader
open Aml.Writer
open Aml.EvaluatorPrimitives
open Aml.Evaluator
module Repl =

    let [<Literal>] AmlPromptStr = "> "
    let [<Literal>] EmptyPromptStr = "  "
    let [<Literal>] QuitStr = "?quit"
    let [<Literal>] RedeclarationStr = "?redeclaration"
    let [<Literal>] OnStr = "on"
    let [<Literal>] OffStr = "off"
    let [<Literal>] NormalConsoleColor = ConsoleColor.Gray
    let [<Literal>] ErrorConsoleColor = ConsoleColor.Red
    let [<Literal>] CommandConsoleColor = ConsoleColor.Cyan
    let OpenChars = [OpenParenChar; OpenBracketChar; OpenCurlyChar]
    let CloseChars = [CloseParenChar; CloseBracketChar; CloseCurlyChar]
    let QuitCommand = [|QuitStr|]
    let RedeclarationOnCommand = [|RedeclarationStr; OnStr|]
    let RedeclarationOffCommand = [|RedeclarationStr; OffStr|]

    let isOpenChar =
        isAnyOf OpenChars

    let isCloseChar =
        isAnyOf CloseChars

    let scopesOpen chars =
        List.fold
            (fun openCount chr ->
                if isOpenChar chr then openCount + 1
                elif isCloseChar chr then openCount - 1
                else openCount)
            0
            chars

    let isInputClosed str =
        let chars = String.explode str
        scopesOpen chars <= 0

    let echoEvalReadSuccess expr env =
        let evalResult = try evalExprWithExplicitTracing expr env with exn -> makeEvalExceptionViolation exn env
        let exprStr = writeExpr evalResult.Value
        Console.ForegroundColor <- match evalResult.Value with Violation _ -> ErrorConsoleColor | _ -> NormalConsoleColor
        Console.WriteLine (AmlPromptStr + exprStr)
        Console.ForegroundColor <- NormalConsoleColor
        evalResult

    let echoReadFailure (message : string) env =
        Console.ForegroundColor <- ErrorConsoleColor
        Console.WriteLine (AmlPromptStr + message)
        Console.ForegroundColor <- NormalConsoleColor
        makeEvalUnit env

    let echoEvalReadResult readResult env =
        match readResult with
        | Failure (message, _, _) -> echoReadFailure message env
        | Success (exprs, _, _) ->
            List.fold
                (fun evalResult expr -> echoEvalReadSuccess expr evalResult.Env)
                (makeEvalUnit env)
                exprs

    let rec promptUser input (startingSpaces : string) =
        if isInputClosed input then input
        else
            Console.Write startingSpaces
            let nextLine = Console.ReadLine ()
            promptUser (input + nextLine) startingSpaces

    let getCommand (input : string) =
        let strs = input.Split [|' '|]
        let splits = Array.map (fun (str : string) -> str.Trim ()) strs
        Array.filter (fun (str : string) -> str.Length <> 0) splits

    let commandSuccess () =
        Console.ForegroundColor <- CommandConsoleColor
        Console.WriteLine (AmlPromptStr + "Repl command executed.\n")
        Console.ForegroundColor <- NormalConsoleColor
    
    let commandFailure () =
        Console.ForegroundColor <- ErrorConsoleColor
        Console.WriteLine (AmlPromptStr + "Invalid repl command or command format.\n")
        Console.ForegroundColor <- NormalConsoleColor

    let rec repl env =
        Console.Write AmlPromptStr
        let firstLine = Console.ReadLine ()
        let input = promptUser firstLine EmptyPromptStr
        let command = getCommand input
        if command.Length <> 0 && command.[0].Length <> 0 && command.[0].[0] = '?' then
            if command = QuitCommand then commandSuccess ()
            elif command = RedeclarationOnCommand then
                commandSuccess ()
                let cachedEntries = env.EnvCachedDeclarationEntries
                for cachedEntry in cachedEntries do cachedEntry := CEUncached
                cachedEntries.Clear ()
                repl { env with EnvAllowRedeclaration = true }
            elif command = RedeclarationOffCommand then
                commandSuccess ()
                repl { env with EnvAllowRedeclaration = false }
            else
                commandFailure ()
                repl env
        else
            let env = { env with EnvDebugInfo = { env.EnvDebugInfo with DIOptFirstReplLine = Some firstLine }}
            let readResult = run readExprsTillEnd input
            let evalResult = echoEvalReadResult readResult env
            let evalResult = configureEvalResultFirstReplLine evalResult None
            Console.WriteLine ()
            repl evalResult.Env