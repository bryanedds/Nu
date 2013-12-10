// Aml - A Modular Language.
// Copyright (C) Bryan Edds, 2012-2013.

// NOTE: change these paths to make this script run with your Aml installation.
#r "C:/OmniBlade/Aml/Fspp4.0/FSharp.PowerPack.Compatibility.dll"
#r "C:/OmniBlade/Aml/Aml/Aml/bin/Debug/FParsecCS.dll"
#r "C:/OmniBlade/Aml/Aml/Aml/bin/Debug/FParsec.dll"
#r "C:/OmniBlade/Aml/xUnit/xunit.dll"
#r "C:/OmniBlade/Prime/Prime/Prime/bin/Debug/Prime.dll"

#load "Ast.fs"
#load "Constants.fs"
#load "Primitives.fs"
#load "Initial.fs"
#load "Writer.fs"
#load "Conversions.fs"
#load "Reader.fs"

open System
open FParsec.Primitives
open FParsec.CharParsers
open Aml.Ast
open Aml.Constants
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