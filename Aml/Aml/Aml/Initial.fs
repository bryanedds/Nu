// Aml - A Modular Language.
// Copyright (C) Bryan Edds, 2012-2013.

namespace Aml
open System
open System.Collections.Generic
open Prime
open Aml.Ast
open Aml.AstModule
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
        [(Lun.make "amlVersion", Float (makeFloatRecord AmlVersion None), makeDoc "Aml's current version.")
         (Lun.make "-bool-", Boolean (makeBooleanRecord false None), makeDoc "A type indicator for a 'bool' type.")
         (Lun.make "-char-", Character (makeCharacterRecord '\u0000' None), makeDoc "A type indicator for a 'char' type.")
         (Lun.make "-string-", String (makeStringRecord (makeLiteralStringValue EmptyStr) None), makeDoc "A type indicator for a 'string' type.")
         (Lun.make "-int-", Int (makeIntRecord 0 None), makeDoc "A type indicator for an 'int' type.")
         (Lun.make "-long-", Long (makeLongRecord 0L None), makeDoc "A type indicator for a 'long' type.")
         (Lun.make "-float-", Float (makeFloatRecord 0.0f None), makeDoc "A type indicator for a 'float' type.")
         (Lun.make "-double-", Double (makeDoubleRecord 0.0 None), makeDoc "A type indicator for a 'double' type.")
         (Lun.make "-keyword-", Keyword (makeKeywordRecord Lun.empty None), makeDoc "A type indicator for a 'keyword' type.")
         (Lun.make "-ref-", Ref (makeRefRecord true UnitValue None), makeDoc "A type indicator for a 'ref' type.")
         (Lun.make "-list-", List (makeListRecord true [] None), makeDoc "A type indicator for a 'list' type.")
         (Lun.make "-array-", Array (makeArrayRecord true [||] None), makeDoc "A type indicator for an 'array' type.")
         (Lun.make "-lambda-", Lambda (makeLambdaRecord true Lun.empty [] 0 UnitValue tautology UnitValue UnitValue true None None), makeDoc "A type indicator for a 'lambda' type.")]

    /// The initial built-in operators.
    /// TODO: document these with documentation comments.
    let InitialBuiltins =
        [(Lun.make FloatFloorStr, None)
         (Lun.make FloatCeilingStr, None)
         (Lun.make FloatTruncateStr, None)
         (Lun.make FloatRoundStr, None)
         (Lun.make FloatExpStr, None)
         (Lun.make FloatLogStr, None)
         (Lun.make FloatSqrtStr, None)
         (Lun.make FloatSinStr, None)
         (Lun.make FloatCosStr, None)
         (Lun.make FloatTanStr, None)
         (Lun.make FloatAsinStr, None)
         (Lun.make FloatAcosStr, None)
         (Lun.make FloatAtanStr, None)
         (Lun.make DoubleFloorStr, None)
         (Lun.make DoubleCeilingStr, None)
         (Lun.make DoubleTruncateStr, None)
         (Lun.make DoubleRoundStr, None)
         (Lun.make DoubleExpStr, None)
         (Lun.make DoubleLogStr, None)
         (Lun.make DoubleSqrtStr, None)
         (Lun.make DoubleSinStr, None)
         (Lun.make DoubleCosStr, None)
         (Lun.make DoubleTanStr, None)
         (Lun.make DoubleAsinStr, None)
         (Lun.make DoubleAcosStr, None)
         (Lun.make DoubleAtanStr, None)
         (Lun.make IntPlusStr, None)
         (Lun.make IntMinusStr, None)
         (Lun.make IntMultiplyStr, None)
         (Lun.make IntDivideStr, None)
         (Lun.make IntPowStr, None)
         (Lun.make IntRemStr, None)
         (Lun.make IntIncStr, None)
         (Lun.make IntDecStr, None)
         (Lun.make LongPlusStr, None)
         (Lun.make LongMinusStr, None)
         (Lun.make LongMultiplyStr, None)
         (Lun.make LongDivideStr, None)
         (Lun.make LongPowStr, None)
         (Lun.make LongRemStr, None)
         (Lun.make LongIncStr, None)
         (Lun.make LongDecStr, None)
         (Lun.make FloatPlusStr, None)
         (Lun.make FloatMinusStr, None)
         (Lun.make FloatMultiplyStr, None)
         (Lun.make FloatDivideStr, None)
         (Lun.make FloatPowStr, None)
         (Lun.make FloatRemStr, None)
         (Lun.make FloatLogNStr, None)
         (Lun.make FloatRootStr, None)
         (Lun.make DoublePlusStr, None)
         (Lun.make DoubleMinusStr, None)
         (Lun.make DoubleMultiplyStr, None)
         (Lun.make DoubleDivideStr, None)
         (Lun.make DoublePowStr, None)
         (Lun.make DoubleRemStr, None)
         (Lun.make DoubleLogNStr, None)
         (Lun.make DoubleRootStr, None)
         (Lun.make CharEqualStr, None)
         (Lun.make CharInequalStr, None)
         (Lun.make CharLessThanStr, None)
         (Lun.make CharGreaterThanStr, None)
         (Lun.make CharLessThanOrEqualStr, None)
         (Lun.make CharGreaterThanOrEqualStr, None)
         (Lun.make IntEqualStr, None)
         (Lun.make IntInequalStr, None)
         (Lun.make IntLessThanStr, None)
         (Lun.make IntGreaterThanStr, None)
         (Lun.make IntLessThanOrEqualStr, None)
         (Lun.make IntGreaterThanOrEqualStr, None)
         (Lun.make LongEqualStr, None)
         (Lun.make LongInequalStr, None)
         (Lun.make LongLessThanStr, None)
         (Lun.make LongGreaterThanStr, None)
         (Lun.make LongLessThanOrEqualStr, None)
         (Lun.make LongGreaterThanOrEqualStr, None)
         (Lun.make FloatEqualStr, None)
         (Lun.make FloatInequalStr, None)
         (Lun.make FloatLessThanStr, None)
         (Lun.make FloatGreaterThanStr, None)
         (Lun.make FloatLessThanOrEqualStr, None)
         (Lun.make FloatGreaterThanOrEqualStr, None)
         (Lun.make DoubleEqualStr, None)
         (Lun.make DoubleInequalStr, None)
         (Lun.make DoubleLessThanStr, None)
         (Lun.make DoubleGreaterThanStr, None)
         (Lun.make DoubleLessThanOrEqualStr, None)
         (Lun.make DoubleGreaterThanOrEqualStr, None)
         (Lun.make AndStr, None)
         (Lun.make OrStr, None)
         (Lun.make DocStr, None)
         (Lun.make IfStr, None)
         (Lun.make ApplyStr, None)
         (Lun.make TypeStr, None)
         (Lun.make TypeOfStr, None)
         (Lun.make EqualStr, None)
         (Lun.make InequalStr, None)
         (Lun.make RefEqualityStr, None)
         (Lun.make RefInequalityStr, None)
         (Lun.make ConsStr, None)
         (Lun.make HeadStr, None)
         (Lun.make TailStr, None)
         (Lun.make StringLengthStr, None)
         (Lun.make StringAppendStr, None)
         (Lun.make ListLengthStr, None)
         (Lun.make ListAppendStr, None)
         (Lun.make ArrayLengthStr, None)
         (Lun.make ArrayAppendStr, None)
         (Lun.make StepsStr, None)
         (Lun.make WhileStr, None)
         (Lun.make IsUnitStr, None)
         (Lun.make IsBooleanStr, None)
         (Lun.make IsIntStr, None)
         (Lun.make IsLongStr, None)
         (Lun.make IsFloatStr, None)
         (Lun.make IsDoubleStr, None)
         (Lun.make IsCharacterStr, None)
         (Lun.make IsStringStr, None)
         (Lun.make IsKeywordStr, None)
         (Lun.make IsPackageStr, None)
         (Lun.make IsLambdaStr, None)
         (Lun.make IsListStr, None)
         (Lun.make IsArrayStr, None)
         (Lun.make IsCompositeStr, None)
         (Lun.make HasTypeStr, None)
         (Lun.make HasProtocolStr, None)
         (Lun.make CharToIntStr, None)
         (Lun.make IntToCharStr, None)
         (Lun.make IntToLongStr, None)
         (Lun.make LongToIntStr, None)
         (Lun.make FloatToDoubleStr, None)
         (Lun.make DoubleToFloatStr, None)
         (Lun.make IntToFloatStr, None)
         (Lun.make FloatToIntStr, None)
         (Lun.make LongToDoubleStr, None)
         (Lun.make DoubleToLongStr, None)
         (Lun.make StringToArrayStr, None)
         (Lun.make ArrayToStringStr, None)
         (Lun.make ListToArrayStr, None)
         (Lun.make ArrayToListStr, None)]

    /// The initial built-in operator names.
    let InitialBuiltinNames =
        let names = List.map fst InitialBuiltins
        List.toHashSet names

    /// The initial types.
    let InitialTypes =
        [(Lun.make ViolationTypeStr, ViolationType, makeDoc "Describes an unhandled evaluation error.")
         (Lun.make BooleanTypeStr, BooleanType, makeDoc "A value that is either true or false.")
         (Lun.make CharacterTypeStr, CharacterType, makeDoc "A alphabetic, numeric, or symbolic character.")
         (Lun.make StringTypeStr, StringType, makeDoc "A series of characters, often used to represent text.")
         (Lun.make IntTypeStr, IntType, makeDoc "A 32-bit integer number.")
         (Lun.make LongTypeStr, LongType, makeDoc "A 64-bit integer number.")
         (Lun.make FloatTypeStr, FloatType, makeDoc "A 32-bit, floating point, fractional number.")
         (Lun.make DoubleTypeStr, DoubleType, makeDoc "A 64-bit, floating point, fractional number (may be 80-bit on certain platforms).")
         (Lun.make KeywordTypeStr, KeywordType, makeDoc "A purely symbolic value.")
         (Lun.make PackageTypeStr, PackageType, makeDoc "Couples an optional parameter with a value.")
         (Lun.make SpecialValueTypeStr, SpecialValueType, makeDoc "A value that describes a corresponding value in a language module.")
         (Lun.make LambdaTypeStr, LambdaType, makeDoc "A lamda in terms of the lambda calculus.")
         (Lun.make UnitTypeStr, UnitType, makeDoc "A unit type (a type with no values).")
         (Lun.make RefTypeStr, RefType, makeDoc "A reference type for mutable values.")
         (Lun.make ListTypeStr, ListType, makeDoc "A persistent list.")
         (Lun.make ArrayTypeStr, ArrayType, makeDoc "A collection of values.")
         (Lun.make CompositeTypeStr, CompositeType, makeDoc "An anonymous collection of values.")]

    /// The list of entries in the initial global environment.
    /// NOTE: this has some duplication with EvalPrims.isBuiltin.
    let InitialEntriesList =
        List.concat
            [makeInitialVariableEntries InitialVariables
             makeInitialOperatorEntries InitialBuiltins
             makeInitialTypeEntries InitialTypes]

    /// Query that the operation with the given name uses division.
    let usesDivisionOperation name =
        name = IntDivideLun ||
        name = IntRemLun ||
        name = LongDivideLun ||
        name = LongRemLun