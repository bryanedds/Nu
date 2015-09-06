// Aml - A Meta-Language.
// Copyright (C) Bryan Edds, 2012-2015.

namespace Aml
open System
open System.Collections.Generic
open Prime
open Aml
open Aml.Ast
open Aml.AmlConstants
open Aml.Primitives
module Initial =

    /// Make an initial variable entry value.
    let makeInitialVariableEntry (name, value, doc) =
        (name, makeVariableEntry value doc)

    /// Make several initial variable entry values.
    let makeInitialVariableEntries variables =
        List.map makeInitialVariableEntry variables

    /// Make an initial operator entry.
    let makeInitialOperatorEntry (name, doc) =
        (name, makeOperatorEntry name doc)

    /// Make several initial operator entry values.
    let makeInitialOperatorEntries operators =
        List.map makeInitialOperatorEntry operators

    /// Make an initial type entry value.
    let makeInitialTypeEntry (typeName, typeValue, doc) =
        (typeName, TypeEntry (typeName, typeValue, doc))

    /// Make several initial type entry values.
    let makeInitialTypeEntries typeDesciptions =
        List.map makeInitialTypeEntry typeDesciptions

    /// The initial variables.
    let InitialVariables =
        [("amlVersion", Float (makeFloatRecord AmlVersion None), makeDoc "Aml's current version.")
         ("-bool-", Boolean (makeBooleanRecord false None), makeDoc "A type indicator for a 'bool' type.")
         ("-char-", Character (makeCharacterRecord '\u0000' None), makeDoc "A type indicator for a 'char' type.")
         ("-string-", String (makeStringRecord (makeLiteralStringValue EmptyStr) None), makeDoc "A type indicator for a 'string' type.")
         ("-int-", Int (makeIntRecord 0 None), makeDoc "A type indicator for an 'int' type.")
         ("-long-", Long (makeLongRecord 0L None), makeDoc "A type indicator for a 'long' type.")
         ("-float-", Float (makeFloatRecord 0.0f None), makeDoc "A type indicator for a 'float' type.")
         ("-double-", Double (makeDoubleRecord 0.0 None), makeDoc "A type indicator for a 'double' type.")
         ("-keyword-", Keyword (makeKeywordRecord String.Empty None), makeDoc "A type indicator for a 'keyword' type.")
         ("-ref-", Ref (makeRefRecord true UnitValue None), makeDoc "A type indicator for a 'ref' type.")
         ("-list-", List (makeListRecord true [] None), makeDoc "A type indicator for a 'list' type.")
         ("-array-", Array (makeArrayRecord true [||] None), makeDoc "A type indicator for an 'array' type.")
         ("-lambda-", Lambda (makeLambdaRecord true String.Empty [] 0 UnitValue tautology UnitValue UnitValue true None None), makeDoc "A type indicator for a 'lambda' type.")]

    /// The initial built-in operators.
    /// TODO: document these with documentation comments.
    let InitialBuiltins =
        [(FloatFloorStr, None)
         (FloatCeilingStr, None)
         (FloatTruncateStr, None)
         (FloatRoundStr, None)
         (FloatExpStr, None)
         (FloatLogStr, None)
         (FloatSqrtStr, None)
         (FloatSinStr, None)
         (FloatCosStr, None)
         (FloatTanStr, None)
         (FloatAsinStr, None)
         (FloatAcosStr, None)
         (FloatAtanStr, None)
         (DoubleFloorStr, None)
         (DoubleCeilingStr, None)
         (DoubleTruncateStr, None)
         (DoubleRoundStr, None)
         (DoubleExpStr, None)
         (DoubleLogStr, None)
         (DoubleSqrtStr, None)
         (DoubleSinStr, None)
         (DoubleCosStr, None)
         (DoubleTanStr, None)
         (DoubleAsinStr, None)
         (DoubleAcosStr, None)
         (DoubleAtanStr, None)
         (IntPlusStr, None)
         (IntMinusStr, None)
         (IntMultiplyStr, None)
         (IntDivideStr, None)
         (IntPowStr, None)
         (IntRemStr, None)
         (IntIncStr, None)
         (IntDecStr, None)
         (LongPlusStr, None)
         (LongMinusStr, None)
         (LongMultiplyStr, None)
         (LongDivideStr, None)
         (LongPowStr, None)
         (LongRemStr, None)
         (LongIncStr, None)
         (LongDecStr, None)
         (FloatPlusStr, None)
         (FloatMinusStr, None)
         (FloatMultiplyStr, None)
         (FloatDivideStr, None)
         (FloatPowStr, None)
         (FloatRemStr, None)
         (FloatLogNStr, None)
         (FloatRootStr, None)
         (DoublePlusStr, None)
         (DoubleMinusStr, None)
         (DoubleMultiplyStr, None)
         (DoubleDivideStr, None)
         (DoublePowStr, None)
         (DoubleRemStr, None)
         (DoubleLogNStr, None)
         (DoubleRootStr, None)
         (CharEqualStr, None)
         (CharInequalStr, None)
         (CharLessThanStr, None)
         (CharGreaterThanStr, None)
         (CharLessThanOrEqualStr, None)
         (CharGreaterThanOrEqualStr, None)
         (IntEqualStr, None)
         (IntInequalStr, None)
         (IntLessThanStr, None)
         (IntGreaterThanStr, None)
         (IntLessThanOrEqualStr, None)
         (IntGreaterThanOrEqualStr, None)
         (LongEqualStr, None)
         (LongInequalStr, None)
         (LongLessThanStr, None)
         (LongGreaterThanStr, None)
         (LongLessThanOrEqualStr, None)
         (LongGreaterThanOrEqualStr, None)
         (FloatEqualStr, None)
         (FloatInequalStr, None)
         (FloatLessThanStr, None)
         (FloatGreaterThanStr, None)
         (FloatLessThanOrEqualStr, None)
         (FloatGreaterThanOrEqualStr, None)
         (DoubleEqualStr, None)
         (DoubleInequalStr, None)
         (DoubleLessThanStr, None)
         (DoubleGreaterThanStr, None)
         (DoubleLessThanOrEqualStr, None)
         (DoubleGreaterThanOrEqualStr, None)
         (AndStr, None)
         (OrStr, None)
         (DocStr, None)
         (IfStr, None)
         (ApplyStr, None)
         (TypeStr, None)
         (TypeOfStr, None)
         (EqualStr, None)
         (InequalStr, None)
         (RefEqualityStr, None)
         (RefInequalityStr, None)
         (ConsStr, None)
         (HeadStr, None)
         (TailStr, None)
         (StringLengthStr, None)
         (StringAppendStr, None)
         (ListLengthStr, None)
         (ListAppendStr, None)
         (ArrayLengthStr, None)
         (ArrayAppendStr, None)
         (StepsStr, None)
         (WhileStr, None)
         (IsUnitStr, None)
         (IsBooleanStr, None)
         (IsIntStr, None)
         (IsLongStr, None)
         (IsFloatStr, None)
         (IsDoubleStr, None)
         (IsCharacterStr, None)
         (IsStringStr, None)
         (IsKeywordStr, None)
         (IsPackageStr, None)
         (IsLambdaStr, None)
         (IsListStr, None)
         (IsArrayStr, None)
         (IsCompositeStr, None)
         (HasTypeStr, None)
         (HasProtocolStr, None)
         (CharToIntStr, None)
         (IntToCharStr, None)
         (IntToLongStr, None)
         (LongToIntStr, None)
         (FloatToDoubleStr, None)
         (DoubleToFloatStr, None)
         (IntToFloatStr, None)
         (FloatToIntStr, None)
         (LongToDoubleStr, None)
         (DoubleToLongStr, None)
         (StringToArrayStr, None)
         (ArrayToStringStr, None)
         (ListToArrayStr, None)
         (ArrayToListStr, None)]

    /// The initial built-in operator names.
    let InitialBuiltinNames =
        let names = List.map fst InitialBuiltins
        List.toHashSet names

    /// The initial types.
    let InitialTypes =
        [(ViolationTypeStr, ViolationType, makeDoc "Describes an unhandled evaluation error.")
         (BooleanTypeStr, BooleanType, makeDoc "A value that is either true or false.")
         (CharacterTypeStr, CharacterType, makeDoc "A alphabetic, numeric, or symbolic character.")
         (StringTypeStr, StringType, makeDoc "A series of characters, often used to represent text.")
         (IntTypeStr, IntType, makeDoc "A 32-bit integer number.")
         (LongTypeStr, LongType, makeDoc "A 64-bit integer number.")
         (FloatTypeStr, FloatType, makeDoc "A 32-bit, floating point, fractional number.")
         (DoubleTypeStr, DoubleType, makeDoc "A 64-bit, floating point, fractional number (may be 80-bit on certain platforms).")
         (KeywordTypeStr, KeywordType, makeDoc "A purely symbolic value.")
         (PackageTypeStr, PackageType, makeDoc "Couples an optional parameter with a value.")
         (SpecialValueTypeStr, SpecialValueType, makeDoc "A value that describes a corresponding value in a language plugin.")
         (LambdaTypeStr, LambdaType, makeDoc "A lamda in terms of the lambda calculus.")
         (UnitTypeStr, UnitType, makeDoc "A unit type (a type with no values).")
         (RefTypeStr, RefType, makeDoc "A reference type for mutable values.")
         (ListTypeStr, ListType, makeDoc "A persistent list.")
         (ArrayTypeStr, ArrayType, makeDoc "A collection of values.")
         (CompositeTypeStr, CompositeType, makeDoc "An anonymous collection of values.")]

    /// The list of entries in the initial global environment.
    /// NOTE: this has some duplication with Evaluator.Prims.isBuiltin.
    let InitialEntriesList =
        List.concat
            [makeInitialVariableEntries InitialVariables
             makeInitialOperatorEntries InitialBuiltins
             makeInitialTypeEntries InitialTypes]

    /// Query that the operation with the given name uses division.
    let usesDivisionOperation name =
        name = IntDivideStr ||
        name = IntRemStr ||
        name = LongDivideStr ||
        name = LongRemStr