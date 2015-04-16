// Aml - A Modular Language.
// Copyright (C) Bryan Edds, 2012-2015.

#r "../../../Prime/FParsec/FParsecCS.dll" // MUST be referenced BEFORE FParsec.dll!
#r "../../../Prime/FParsec/FParsec.dll"
#r "../../../Aml/Fspp4.0/FSharp.PowerPack.Compatibility.dll"
#r "../../../Aml/xUnit/xunit.dll"
#r "../../../Prime/Prime/Prime/bin/Debug/Prime.exe"
#r "../../../Aml/Aml/Aml/bin/Debug/Aml.exe"

open System
open FParsec.Primitives
open FParsec.CharParsers
open Aml
open Aml.Ast
open Aml.AmlConstants
open Aml.Primitives
open Aml.Initial
open Aml.Writer
open Aml.Conversions
open Aml.Reader

/// Test a parser interactively.
let testParser (parser : Parser<string, unit>) str =
    let result = run parser str
    match result with
    | Failure (message, _, _) -> Console.WriteLine message
    | Success (desugared, _, _) -> Console.WriteLine desugared
    
/// Test a parser on a file interactively.
let testParserOnFile (parser : Parser<string, unit>) path =
    let result = runParserOnFile parser () path System.Text.Encoding.Default
    match result with
    | Failure (message, _, _) -> Console.WriteLine message
    | Success (desugared, _, _) -> Console.WriteLine desugared