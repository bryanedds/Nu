// Aml - A Modular Language.
// Copyright (C) Bryan Edds, 2012-2013.

module Aml.Reader
open System
open FParsec
open FParsec.Primitives
open FParsec.CharParsers
open Aml.Ast
open Aml.Constants
open Aml.Primitives
open Aml.Conversions

/// Parse one more than many1.
let many2 parser = parser .>>. many1 parser |>> fun (x, xs) -> x :: xs

/// Query that a character is a special name character.
let isSpecialNameChar = isAnyOf SpecialNameChars

/// Read a reserved character.
let reservedChar = anyOf ReservedChars

/// Read a special name character.
let specialNameChar = anyOf SpecialNameChars

/// Read a prefix character.
let prefixChar = anyOf PrefixChars

/// Read the first character of a name.
let firstNameChar = lower <|> specialNameChar

/// Read a name character.
let nameChar = digit <|> letter <|> specialNameChar

/// Read an escaped literal character.
let escapedChar =
    skipChar '\\' >>.
    anyOf "0\"\\abfnrtv" |>>
    function
    | '0' -> '\u0000'
    | '\"' -> '\"'
    | '\\' -> '\\'
    | 'a' -> '\a'
    | 'b' -> '\b'
    | 'f' -> '\u000c'
    | 'n' -> '\n'
    | 'r' -> '\r'
    | 't' -> '\t'
    | 'v' -> '\v'
    | c -> c

/// Read a literal character.
let literalChar = escapedChar <|> noneOf VerbatimChars

/// Read a verbatim character.
let verbatimChar = noneOf DoubleQuoteStr

/// Read the remaining characters of a name.
let nameChars = many nameChar

/// Read the remaining characters of a string with reserved characters.
let remainingReservedChars = many (nameChar <|> reservedChar)

/// Declare the next char to be not a dot.
let notFollowedByDot =
    nextCharSatisfiesNot (fun chr -> chr = DotChar)

/// Declare the next char to be not a letter, digit, or name character.
let notFollowedByLetterOrDigitOrNameChar =
    nextCharSatisfiesNot (fun chr -> isLetter chr || isDigit chr || isSpecialNameChar chr)
    
/// Skip a whitespace character.
let skipSpace = skipAnyOf WhitespaceChars

/// Skip a line comment.
let skipLineComment = skipChar LineCommentChar >>. skipRestOfLine true

/// Skip a multiline comment.
/// TODO: make multiline comments nest.
let skipMultilineComment =
    between
        (skipString OpenMultilineCommentStr)
        (skipString CloseMultilineCommentStr)
        (skipCharsTillString CloseMultilineCommentStr false System.Int32.MaxValue)

/// Skip whitespace text.
let skipWhitespace = skipLineComment <|> skipMultilineComment <|> skipSpace

/// Skip any white space characters.
let skipWhitespaces = skipMany skipWhitespace

/// Skip at least one white space character.
let skipWhitespaces1 = skipMany1 skipWhitespace

/// Skip a double quote character.
let skipDoubleQuote = skipChar DoubleQuoteChar

/// Skip a hash.
let skipHash = skipChar HashChar

/// Skip a backslash.
let skipBackslash = skipChar BackslashChar

/// Skip a colon.
let skipColon = skipChar ColonChar

/// Skip a character form.
let charForm character =
    skipChar character >>.
    skipWhitespaces

/// Skip a colon form.
let colonForm = charForm ColonChar

/// Skip an open bracket form.
let openBracketForm = charForm OpenBracketChar

/// Skip a close bracket form.
let closeBracketForm = charForm CloseBracketChar

/// Skip between brackets.
let betweenBracketForms parser = between openBracketForm closeBracketForm parser

/// Skip an open paren form.
let openParenForm = charForm OpenParenChar

/// Skip a close paren form.
let closeParenForm = charForm CloseParenChar

/// Skip between parenthises.
let betweenParenForms parser = between openParenForm closeParenForm parser

/// Skip an open curly form.
let openCurlyForm = charForm OpenCurlyChar

/// Skip a close curly form.
let closeCurlyForm = charForm CloseCurlyChar

/// Skip between curlies.
let betweenCurlyForms parser = between openCurlyForm closeCurlyForm parser

/// Skip a form with a series of chars.
let charsForm str =
    skipString str >>.
    skipWhitespaces

/// Skip an open triangle form.
let openTriangleForm = charsForm OpenTriangleStr

/// Skip a close triangle form.
let closeTriangleForm = charsForm CloseTriangleStr

/// Skip between triangles.
let betweenTriangleForms parser = between openTriangleForm closeTriangleForm parser

/// Skip a string form.
let strForm str closeChar =
    skipString str >>.
    (skipWhitespaces1 <|> followedBy (pchar closeChar)) >>.
    skipWhitespaces

/// Skip a violation form.
let violationForm closeChar = strForm ViolationStr closeChar

/// Skip a lambda form.
let lambdaForm closeChar = strForm LambdaStr closeChar

/// Skip a let form.
let letForm closeChar = strForm LetStr closeChar

/// Skip an attempt form.
let attemptForm closeChar = strForm AttemptStr closeChar

/// Skip an extend form.
let extendForm closeChar = strForm ExtendStr closeChar

/// Skip a case form.
let caseForm closeChar = strForm CaseStr closeChar

/// Skip a condition form.
let conditionForm closeChar = strForm ConditionStr closeChar

/// Skip a intervene form.
let interveneForm closeChar = strForm InterveneStr closeChar

/// Skip a ref form.
let refForm closeChar = strForm RefStr closeChar

/// Skip a get form.
let getForm closeChar = strForm GetStr closeChar

/// Skip a set form.
let setForm closeChar = strForm SetStr closeChar

/// Skip a list form.
let listForm closeChar = strForm ListStr closeChar

/// Skip an array form.
let arrayForm closeChar = strForm ArrayStr closeChar

/// Skip a composite form.
let compositeForm closeChar = strForm CompositeStr closeChar

/// Skip a constraint form.
let constraintForm closeChar = strForm WhereStr closeChar

/// Skip a selector form.
let selectorForm closeChar = strForm SelectorStr closeChar

/// Skip a definition form.
let definitionForm closeChar = strForm DefinitionStr closeChar

/// Skip a structure form.
let structureForm closeChar = strForm StructureStr closeChar

/// Skip a special value form.
let specialValueForm closeChar = strForm SpecialValueStr closeChar

/// Skip a signature form.
let signatureForm closeChar = strForm SignatureStr closeChar

/// Skip a protocol form.
let protocolForm closeChar = strForm ProtocolStr closeChar

/// Skip an instance form.
let instanceForm closeChar = strForm InstanceStr closeChar

/// Skip an affirmation form.
let affirmationForm closeChar = strForm AffirmationStr closeChar

/// Skip a using file form.
let usingFileForm closeChar = strForm UsingFileStr closeChar

/// Skip a using language form.
let usingLanguageForm closeChar = strForm UsingLanguageStr closeChar

/// Read a name string that starts with an uppercase letter.
let readCapitalizedNameStr =
    upper .>>.
    nameChars |>>
    fun (nameHead, nameTail) -> Lun.make (String.implode (nameHead :: nameTail))

/// Read a name string with reserved characters in it.
let readReservedNameStr =
    nameChars .>>.
    reservedChar .>>.
    remainingReservedChars |>>
    fun ((nonReservedPart, firstReservedChar), remainingPart) -> Lun.make (String.implode (nonReservedPart @ firstReservedChar :: remainingPart))

/// Read a keyword string with trailing whitespace.
let readKeywordValueWithWhitespace =
    skipColon >>.
    firstNameChar .>>.
    nameChars .>>
    skipWhitespaces |>>
    fun (first, rest) -> Lun.make (String.implode (ColonChar :: first :: rest))

/// Read an F# string.
let readStr str =
    pstring str .>>
    skipWhitespaces

/// Read the content of a character.
let readCharacterContent = literalChar

/// Read the content of a literal string.
let readLiteralStringContent = many literalChar |>> String.implode

/// Read the content of a verbatim string.
let readVerbatimStringContent = many verbatimChar |>> String.implode

/// Read a name.
let readName =
    firstNameChar .>>.
    nameChars .>>
    skipWhitespaces |>>
    fun (head, tail) -> Lun.make (String.implode (head :: tail))
    
/// Read multiple names.
let readNames formOpen formClose = between formOpen formClose (many readName)

/// Read an abstracting arg.
/// HACK: readArg is arbitrarily relying on receiving expression to discriminate on what type of
/// arg was parsed, so we hack by using a Keyword here to follow along...
let readArgAsAbstracting =
    betweenTriangleForms <| readName .>>
    skipWhitespaces |>>
    fun name -> Violation (makeViolationRecord true name (makeLiteralStringValue EmptyStr) UnitValue None)

/// Read an concrete arg as a string.
let readConcreteArgAsString =
    readName |>>
    fun name -> String (makeStringRecord (makeStringValue name.LunStr LiteralString) None)

/// Read an arg as a string.
let readArgAsString =
    readName .>>.
    opt (pstring EllipsisStr) |>>
    fun (name, optEllipsis) ->
        let suffix = match optEllipsis with None -> EmptyStr | Some ellipsis -> ellipsis
        String (makeStringRecord (makeStringValue (name.LunStr + suffix) LiteralString) None)

/// Forward-reference the expr reader.
let (readExpr : Parser<Expr, unit>, readExprRef : Parser<Expr, unit> ref) = createParserForwardedToRef ()

/// Forward-reference the procedural expr reader.
let (readProceduralExpr : Parser<Expr, unit>, readProceduralExprRef : Parser<Expr, unit> ref) = createParserForwardedToRef ()

/// Forward-reference the declarative expr reader.
let (readDeclarativeExpr : Parser<Expr, unit>, readDeclarativeExprRef : Parser<Expr, unit> ref) = createParserForwardedToRef ()

/// Read an optional procedural expressions.
let readOptProceduralExpr = opt readProceduralExpr

/// Read multiple expressions.
let readExprs = many readExpr

/// Read multiple procedural expressions.
let readProceduralExprs = many readProceduralExpr

/// Read at least two declarative expressions.
let readDeclarativeExprs2 = many2 readDeclarativeExpr

/// Read expressions until the end of the input.
let readExprsTillEnd = skipWhitespaces >>. readExprs .>> eof

/// Read the contents of a package.
let readPackageContents =
    readName .>>
    colonForm .>>.
    readProceduralExpr .>>
    skipWhitespaces

/// Read a package.
let readPackage =
    parse {
        let! start = getPosition
        let! (name, value) = readPackageContents
        let! stop = getPosition
        let optPositions = Some (makeParserPositions start stop)
        return Package (makePackageRecord name value optPositions) }

/// Read a package argument.
let readPackageArg = readPackage

/// Read a package value.
let readPackageValue = readPackage

/// Read multiple packages or expressions.
let readPackagesOrExprs = many (attempt readPackageValue <|> readExpr)

/// Read multiple procedural packages or expressions.
let readPackagesOrProceduralExprs = many (attempt readPackageValue <|> readProceduralExpr)

/// Read an argument.
let readArg =
    (attempt readArgAsAbstracting <|> attempt readPackageArg <|> readArgAsString) |>>
    function
    | String string -> makeArgFromName (Lun.make string.SRValue.SVValue)
    | Package package -> makeArgFromPackage package
    | Violation violation -> makeArgFromViolation violation
    | _ -> failwith "Unexpected argument read error in 'Aml.Reader.readArg'."

/// Read multiple arguments.
let readArgs formOpen formClose = between formOpen formClose (many readArg)

/// Read at least one argument.
let readArgs1 formOpen formClose = between formOpen formClose (many1 readArg)

/// Read a concrete argument.
let readConcreteArg =
    readConcreteArgAsString |>>
    function
    | String string -> makeArgFromName (Lun.make string.SRValue.SVValue)
    | _ -> failwith "Unexpected argument read error in 'Aml.Reader.readArg'."

/// Read multiple concrete arguments.
let readConcreteArgs formOpen formClose = between formOpen formClose (many readConcreteArg)

/// Read at least one concrete argument.
let readConcreteArgs1 formOpen formClose = between formOpen formClose (many1 readConcreteArg)

/// Read at one argument name.
let readArgName formOpen formClose = between formOpen formClose readName

/// Read at least one argument name.
let readArgNames1 formOpen formClose = between formOpen formClose (many1 readName)

/// Read at least one argument name.
let readArgNamesFlat1 = many1 readName

/// Read the contents of a specifically-tagged package.
let readContractContents tag =
    skipString tag >>.
    colonForm >>.
    readProceduralExpr

/// Read a contract.
let readContract contractTag =
    opt (attempt (readContractContents contractTag)) |>>
    fun optPre -> match optPre with None -> UnitValue | Some pre -> pre

/// Read a composite member.
let readMember =
    openParenForm >>.
    readName .>>.
    readProceduralExpr .>>
    closeParenForm |>>
    fun (name, expr) -> makeMember name expr

/// Read multiple members.
let readMembers = many readMember

/// Read at least one member.
let readMembers1 = many1 readMember

/// Read a test branch.
let readTestBranch =
    openParenForm >>.
    readProceduralExpr .>>.
    readProceduralExpr .>>
    closeParenForm |>>
    fun (test, body) -> makeTestBranch test body

/// Read at least one test branch.
let readTestBranches1 = many1 readTestBranch

/// Read a type constraint.
let readConstraint =
    parse {
        do! openBracketForm
        let! typeName = readName
        let! args = readArgNamesFlat1
        do! closeBracketForm
        return makeConstraint typeName args }

// Read type constraints.
let readConstraints formOpen formClose =
    parse {
        do! skipString WhereStr
        do! colonForm
        do! formOpen
        let! constraints = many readConstraint
        do! formClose
        return constraints }

/// Optionally read constraints.
let readOptConstraints formOpen formClose =
    opt (attempt (readConstraints formOpen formClose))

/// Read a reserved name.
let readReservedName =
    (attempt readCapitalizedNameStr <|> readReservedNameStr) .>>
    skipWhitespaces |>>
    fun reservedName -> makeViolationWithoutBreakpoint ":v/reader/reservedChars" ("Reserved character(s) in '" + reservedName.LunStr + "'.")

/// Read a boolean.
let readBoolean =
    parse {
        let! start = getPosition
        let! value = readStr TrueStr <|> readStr FalseStr
        do! skipWhitespaces
        let! stop = getPosition
        let optPositions = Some (makeParserPositions start stop)
        return Boolean (makeBooleanRecord (value = TrueStr) optPositions) }

/// Read a character.
let readCharacter =
    parse {
        let! start = getPosition
        do! skipBackslash
        let! chr = between skipDoubleQuote skipDoubleQuote readCharacterContent
        do! skipWhitespaces
        let! stop = getPosition
        let optPositions = Some (makeParserPositions start stop)
        return Character (makeCharacterRecord chr optPositions) }

/// Read a literal string.
let readLiteralString =
    parse {
        let! start = getPosition
        let! str = between skipDoubleQuote skipDoubleQuote readLiteralStringContent
        do! skipWhitespaces
        let! stop = getPosition
        let optPositions = Some (makeParserPositions start stop)
        return String (makeStringRecord (makeStringValue str LiteralString) optPositions) }

/// Read a verbatim string.
let readVerbatimString =
    parse {
        let! start = getPosition
        do! skipHash
        let! str = between skipDoubleQuote skipDoubleQuote readVerbatimStringContent
        do! skipWhitespaces
        let! stop = getPosition
        let optPositions = Some (makeParserPositions start stop)
        return String (makeStringRecord (makeStringValue str VerbatimString) optPositions) }

/// Read a literal or verbatim string.
let readString = (attempt readLiteralString) <|> readVerbatimString

/// Read an int.
let readInt =
    parse {
        let! start = getPosition
        let! value = pint32
        let! _ = opt (skipString IntSuffixStr)
        do! notFollowedByLetterOrDigitOrNameChar
        do! notFollowedByDot
        do! skipWhitespaces
        let! stop = getPosition
        let optPositions = Some (makeParserPositions start stop)
        return Int (makeIntRecord value optPositions) }

/// Read a small long.
let readSmallLong =
    (pint32 |>> int64) .>>
    skipString LongSuffixStr

/// Read a large long.
let readLargeLong =
    pint64 .>>
    opt (skipString LongSuffixStr)

/// Read an long.
let readLong =
    parse {
        let! start = getPosition
        let! value = attempt readSmallLong <|> readLargeLong
        do! notFollowedByLetterOrDigitOrNameChar
        do! notFollowedByDot
        do! skipWhitespaces
        let! stop = getPosition
        let optPositions = Some (makeParserPositions start stop)
        return Long (makeLongRecord value optPositions) }

/// Read a float or double.
/// TODO: add parser for single precision as to avoid the hackiness here.
let readFloatOrDouble =
    parse {
        let! start = getPosition
        let! value = pfloat
        let! optDoubleness =
            (opt
                ((skipString FloatSuffixStr |>> fun _ -> false) <|>
                 (skipString DoubleSuffixStr |>> fun _ -> true)))
        do! notFollowedByLetterOrDigitOrNameChar
        do! skipWhitespaces
        let! stop = getPosition
        let optPositions = Some (makeParserPositions start stop)
        return
            match optDoubleness with
            | Some doubleness when doubleness -> Double (makeDoubleRecord value optPositions)
            | _ -> Float (makeFloatRecord (single value) optPositions) }

/// Read a keyword.
let readKeyword =
    parse {
        let! start = getPosition
        let! value = readKeywordValueWithWhitespace
        do! skipWhitespaces
        let! stop = getPosition
        let optPositions = Some (makeParserPositions start stop)
        return Keyword (makeKeywordRecord value optPositions) }

/// Read a tagged flag.
let readFlag tag =
    skipString tag >>.
    colonForm >>.
    readBoolean

/// Optionally read a tagged flag.
let readOptFlag tag =
    opt (attempt (readFlag tag))

/// Read a symbol.
let readSymbol =
    parse {
        let! start = getPosition
        let! name = readName
        do! skipWhitespaces
        let! stop = getPosition
        let optPositions = Some (makeParserPositions start stop)
        return Symbol (makeSymbolRecord name (ref CEUncached) optPositions) }

/// Read a prefixed expression.
let readPrefixed readPrefixedExpr =
    parse {
        let! start = getPosition
        let! prefixType =
            prefixChar |>>
                function
                | DollarChar -> DollarPrefix
                | PercentChar -> PercentPrefix
                | AmpersandChar -> AmpersandPrefix
                | AtChar -> AtPrefix
                | _ -> failwith "Unexpected match failure in 'Aml.Reader.readPrefixed'."
        do! notFollowedBy skipWhitespace
        let! value = readPrefixedExpr
        let! stop = getPosition
        let optPositions = Some (makeParserPositions start stop)
        let specialId = getNextSpecialId ()
        return Prefixed (makePrefixedRecord prefixType value specialId optPositions) }

/// Read a prefixed procedural expression.
let readPrefixedProceduralExpr = readPrefixed readProceduralExpr

/// Read a prefixed expression.
let readPrefixedExpr = readPrefixed readExpr

/// Read a violation.
let readViolation =
    parse {
        let! start = getPosition
        do! openParenForm
        do! violationForm CloseParenChar
        let! category = readKeywordValueWithWhitespace
        let! message = readString |>> fun optString -> (exprToOptStringValue optString).Value
        let! data = readProceduralExpr
        do! closeParenForm
        let! stop = getPosition
        let optPositions = Some (makeParserPositions start stop)
        return Violation (makeViolationRecord false category message data optPositions) }

/// Read a documentation comment.
let readDoc =
    opt (
        attempt (
            skipString DocStr >>.
            colonForm >>.
            readString .>>
            skipWhitespaces |>>
            fun string -> (exprToOptStringValue string).Value))

/// Read a lambda.
let readLambda =
    parse {
        let! start = getPosition
        do! openParenForm
        do! lambdaForm CloseParenChar
        let! args = readArgs openParenForm closeParenForm
        let argCount = args.Length
        let emptyUnification = areSimpleArgs args
        let! pre = readContract PreconditionStr
        let! post = readContract PostconditionStr
        let! expr = readProceduralExpr
        do! closeParenForm
        let! stop = getPosition
        let optPositions = Some (makeParserPositions start stop)
        return Lambda (makeLambdaRecord false Lun.empty args argCount expr tautology pre post emptyUnification optPositions None) }

/// Read an attempt branch.
let readAttemptBranch =
    parse {
        let! start = getPosition
        do! openParenForm
        let! category = readKeywordValueWithWhitespace
        let! body = readProceduralExpr
        do! closeParenForm
        let! stop = getPosition
        let optPositions = Some (makeParserPositions start stop)
        return makeAttemptBranch category body }

/// Read at least one attempt branch.
let readAttemptBranches1 = many1 readAttemptBranch

/// Read an intervention branch.
let readInterventionBranch =
    parse {
        let! start = getPosition
        do! openParenForm
        let! category = readKeywordValueWithWhitespace
        let! body = readProceduralExpr
        let! optHide = readOptFlag HideStr
        let hide = match optHide with Some (Boolean hide) -> hide.BRValue | _ -> false
        do! closeParenForm
        let! stop = getPosition
        let optPositions = Some (makeParserPositions start stop)
        return makeInterventionBranch None category body hide }

/// Read at least one intervention branch.
let readInterventionBranches1 = many1 readInterventionBranch

/// Read a let expression.
let readAttempt =
    parse {
        let! start = getPosition
        do! openParenForm
        do! attemptForm SpaceChar
        let! body = readProceduralExpr
        let! branches = readAttemptBranches1
        do! closeParenForm
        let! stop = getPosition
        let optPositions = Some (makeParserPositions start stop)
        return Attempt (makeAttemptRecord body branches optPositions) }

/// Read a variable let binding.
let readLetVariable =
    parse {
        let! start = getPosition
        do! openParenForm
        let! name = readName
        let! expr = readProceduralExpr
        do! closeParenForm
        let! stop = getPosition
        let optPositions = Some (makeParserPositions start stop)
        return LetVariable (name, expr) }

/// Read a function let binding.
let readLetFunction =
    parse {
        let! start = getPosition
        do! openParenForm
        let! name = readName
        let! args = readArgs openParenForm closeParenForm
        let argCount = args.Length
        let emptyUnification = areSimpleArgs args
        let! optConstraints = readOptConstraints openBracketForm closeBracketForm
        let! pre = readContract PreconditionStr
        let! post = readContract PostconditionStr
        let! body = readProceduralExpr
        do! closeParenForm
        let! stop = getPosition
        let optPositions = Some (makeParserPositions start stop)
        return LetFunction (name, args, argCount, body, optConstraints, pre, post, emptyUnification) }

/// Read a let binding.
let readLetBinding = attempt readLetFunction <|> attempt readLetVariable

/// Read at least one let binding.
/// This is a wonky implementation because the last clause in a let can be parsed as a binding but
/// is always instead an expression.
let readLetBindings1 = many (notFollowedBy (readProceduralExpr >>. closeParenForm) >>. readLetBinding)

/// Read a let expression.
let readLet =
    parse {
        let! start = getPosition
        do! openParenForm
        do! letForm CloseParenChar
        let! bindings = readLetBindings1
        let bindingCount = bindings.Length
        let! body = readProceduralExpr
        do! closeParenForm
        let! stop = getPosition
        let optPositions = Some (makeParserPositions start stop)
        return Let (makeLetRecord bindings bindingCount body optPositions) }

/// Read an extend expression.
let readExtend =
    parse {
        let! start = getPosition
        do! openParenForm
        do! extendForm CloseParenChar
        let! target = readProceduralExpr
        let! memberList = readMembers1
        let members = List.toDictionaryBy (fun mem -> (mem.MemName, mem)) memberList
        do! closeParenForm
        let! stop = getPosition
        let optPositions = Some (makeParserPositions start stop)
        return Extend (makeExtendRecord target members optPositions) }

/// Read a case expression.
let readCase =
    parse {
        let! start = getPosition
        do! openParenForm
        do! caseForm CloseParenChar
        let! target = readProceduralExpr
        let! branches = readTestBranches1
        do! closeParenForm
        let! stop = getPosition
        let optPositions = Some (makeParserPositions start stop)
        return Case (makeCaseRecord target branches optPositions) }

/// Read a condition expression.
let readCondition =
    parse {
        let! start = getPosition
        do! openParenForm
        do! conditionForm CloseParenChar
        let! branches = readTestBranches1
        do! closeParenForm
        let! stop = getPosition
        let optPositions = Some (makeParserPositions start stop)
        return Condition (makeConditionRecord branches optPositions) }

/// Read an intervene expression.
let readIntervene =
    parse {
        let! start = getPosition
        do! openParenForm
        do! interveneForm SpaceChar
        let! body = readProceduralExpr
        let! branches = readInterventionBranches1
        do! closeParenForm
        let! stop = getPosition
        let optPositions = Some (makeParserPositions start stop)
        return Intervene (makeInterveneRecord body branches optPositions) }

/// Read a ref expression.
let readRef =
    parse {
        let! start = getPosition
        do! openParenForm
        do! refForm CloseParenChar
        let! expr = readExpr
        do! closeParenForm
        let! stop = getPosition
        let optPositions = Some (makeParserPositions start stop)
        return Ref (makeRefRecord false expr optPositions) }

/// Read a get expression.
let readGet =
    parse {
        let! start = getPosition
        do! openParenForm
        do! getForm CloseParenChar
        let! target = readExpr
        do! closeParenForm
        let! stop = getPosition
        let optPositions = Some (makeParserPositions start stop)
        return Get (makeGetRecord target optPositions) }

/// Read a set! expression.
let readSet =
    parse {
        let! start = getPosition
        do! openParenForm
        do! setForm CloseParenChar
        let! target = readExpr
        let! injection = readExpr
        do! closeParenForm
        let! stop = getPosition
        let optPositions = Some (makeParserPositions start stop)
        return Set (makeSetRecord target injection optPositions) }

/// Read a list value.
let readList =
    parse {
        let! start = getPosition
        do! openParenForm
        do! listForm CloseParenChar
        let! elementsList = readProceduralExprs
        do! closeParenForm
        let! stop = getPosition
        let optPositions = Some (makeParserPositions start stop)
        return List (makeListRecord false elementsList optPositions) }

/// Read an array value.
let readArray =
    parse {
        let! start = getPosition
        do! openParenForm
        do! arrayForm CloseParenChar
        let! elementsList = readProceduralExprs
        do! closeParenForm
        let! stop = getPosition
        let optPositions = Some (makeParserPositions start stop)
        return Array (makeArrayRecord false (List.toArray elementsList) optPositions) }

/// Read a composite value.
let readComposite =
    parse {
        let! start = getPosition
        do! openParenForm
        do! compositeForm CloseParenChar
        let! memberList = readMembers
        let members = List.toDictionaryBy (fun mem -> (mem.MemName, mem)) memberList
        do! closeParenForm
        let! stop = getPosition
        let optPositions = Some (makeParserPositions start stop)
        return Composite (makeCompositeRecord false Lun.empty members CompositeType null null optPositions) }

/// Read a functional selector.
let readFunctionalSelector =
    parse {
        let! start = getPosition
        do! openParenForm
        do! selectorForm CloseParenChar
        let! key = readProceduralExpr
        let! target = readProceduralExpr
        do! closeParenForm
        let! stop = getPosition
        let optPositions = Some (makeParserPositions start stop)
        return Selector (makeSelectorRecord key target FunctionalSelector optPositions) }

/// Read a special value
let readSpecialValue =
    parse {
        let! start = getPosition
        do! openParenForm
        do! specialValueForm CloseParenChar
        let! languageName = readKeywordValueWithWhitespace
        let! value = readProceduralExpr
        do! closeParenForm
        let! stop = getPosition
        let optPositions = Some (makeParserPositions start stop)
        return SpecialValue (makeSpecialValueRecord false languageName value optPositions) }

/// Read a variable.
let readVariable =
    parse {
        let! start = getPosition
        do! openBracketForm
        do! definitionForm CloseBracketChar
        let! name = readName
        let! doc = readDoc
        let! expr = readProceduralExpr
        do! closeBracketForm
        let! stop = getPosition
        let optPositions = Some (makeParserPositions start stop)
        return Variable (makeVariableRecord name expr doc optPositions) }

/// Read a function.
let readFunction =
    parse {
        let! start = getPosition
        do! openBracketForm
        do! definitionForm CloseBracketChar
        let! name = readName
        let! args = readArgs openBracketForm closeBracketForm
        let argCount = args.Length
        let emptyUnification = areSimpleArgs args
        let! optConstraints = readOptConstraints openBracketForm closeBracketForm
        let! doc = readDoc
        let! pre = readContract PreconditionStr
        let! post = readContract PostconditionStr
        let! body = readProceduralExpr
        do! closeBracketForm
        let! stop = getPosition
        let optPositions = Some (makeParserPositions start stop)
        return Function (makeFunctionRecord name args argCount body optConstraints doc pre post emptyUnification optPositions) }

/// Read either a variable or a function.
let readVariableOrFunction = (attempt readVariable) <|> readFunction

/// Read an unconstrained function.
/// TODO: find a way to factor out commonality among this and readFunction.
let readUnconstrainedFunction =
    parse {
        let! start = getPosition
        do! openBracketForm
        do! definitionForm CloseBracketChar
        let! name = readName
        let! args = readArgs openBracketForm closeBracketForm
        let argCount = args.Length
        let emptyUnification = areSimpleArgs args
        let! doc = readDoc
        let! pre = readContract PreconditionStr
        let! post = readContract PostconditionStr
        let! body = readProceduralExpr
        do! closeBracketForm
        let! stop = getPosition
        let optPositions = Some (makeParserPositions start stop)
        return Function (makeFunctionRecord name args argCount body None doc pre post emptyUnification optPositions) }

/// Read at least one variable or unconstrained function.
let readVariablesAndUnconstrainedFunctions1 = many1 (attempt readVariable <|> readUnconstrainedFunction)

/// Read a series.
let readSeries =
    parse {
        let! start = getPosition
        do! openParenForm
        let! optOpExpr = readOptProceduralExpr
        let! argExprs = readPackagesOrProceduralExprs
        do! closeParenForm
        let! stop = getPosition
        let optPositions = Some (makeParserPositions start stop)
        return
            match optOpExpr with
            | None -> UnitValue
            | Some opExpr -> Series (makeSeriesRecord (opExpr :: argExprs) (argExprs.Length + 1) optPositions) }

/// Read a structure.
let readStructure =
    parse {
        let! start = getPosition
        do! openBracketForm
        do! structureForm CloseBracketChar
        let! name = readName
        let! memberNames = readNames openBracketForm closeBracketForm
        let! optConstraints = readOptConstraints openBracketForm closeBracketForm
        let! doc = readDoc
        let! req = readContract RequirementStr
        do! closeBracketForm
        let! stop = getPosition
        let optPositions = Some (makeParserPositions start stop)
        return Structure (makeStructureRecord name memberNames optConstraints doc req optPositions) }

/// Read a function signature.
let readSignature =
    parse {
        do! openBracketForm
        do! signatureForm CloseBracketChar
        let! name = readName
        let! args = readConcreteArgs1 openBracketForm closeBracketForm
        let! doc = readDoc
        do! closeBracketForm
        return makeSignature name args doc }

// Read multiple function signatures.
let readSignatures = many1 readSignature

/// Read a protocol.
let readProtocol =
    parse {
        let! start = getPosition
        do! openBracketForm
        do! protocolForm CloseBracketChar
        let! name = readName
        let! arg = readArgName openBracketForm closeBracketForm
        let! optConstraints = readOptConstraints openBracketForm closeBracketForm
        let! doc = readDoc
        let! signatures = readSignatures
        do! closeBracketForm
        let! stop = getPosition
        let optPositions = Some (makeParserPositions start stop)
        return Protocol (makeProtocolRecord name arg optConstraints doc signatures optPositions) }

/// Read an instance.
let readInstance =
    parse {
        let! start = getPosition
        do! openBracketForm
        do! instanceForm CloseBracketChar
        let! protocolName = readName |>> fun name -> Lun.make ProtocolPrefixStr ++ name
        let! args = readArgNames1 openBracketForm closeBracketForm
        let! constraints = readConstraints openBracketForm closeBracketForm
        let! doc = readDoc
        let! sigImpls = readVariablesAndUnconstrainedFunctions1
        do! closeBracketForm
        let! stop = getPosition
        let optPositions = Some (makeParserPositions start stop)
        return Instance (makeInstanceRecord protocolName args constraints sigImpls optPositions) }

/// Read an affirmation.
let readAffirmation =
    parse {
        let! start = getPosition
        do! openBracketForm
        do! affirmationForm CloseBracketChar
        let! name = readName
        let! doc = readDoc
        let! expr = readProceduralExpr
        do! closeBracketForm
        let! stop = getPosition
        let optPositions = Some (makeParserPositions start stop)
        return Affirmation (makeAffirmationRecord name doc expr optPositions) }

/// Read a using file declaration.
let readUsingFile =
    parse {
        let! start = getPosition
        do! openBracketForm
        do! usingFileForm CloseBracketChar
        let! pathString = readString
        let! optReload = readOptFlag ReloadStr
        let reload = match optReload with Some (Boolean reload) -> reload.BRValue | _ -> false
        do! closeBracketForm
        let! stop = getPosition
        let optPositions = Some (makeParserPositions start stop)
        match pathString with
        | String string -> return UsingFile (makeUsingFileRecord string.SRValue.SVValue reload optPositions)
        | _ -> failwith "Unexpected match failure in 'Aml.Reader.readUsingFile'." }

/// Read a using language declaration.
let readUsingLanguage =
    parse {
        let! start = getPosition
        do! openBracketForm
        do! usingLanguageForm CloseBracketChar
        let! pathString = readString
        let! typeString = readString
        do! closeBracketForm
        let! stop = getPosition
        let optPositions = Some (makeParserPositions start stop)
        match (pathString, typeString) with
        | (String path, String aType) -> return UsingLanguage (makeUsingLanguageRecord path.SRValue.SVValue aType.SRValue.SVValue optPositions)
        | _ -> failwith "Unexpected match failure in 'Aml.Reader.readUsingLanguage'." }

/// Read a special declarative series.
let readSpecialDeclarativeSeries =
    parse {
        let! start = getPosition
        do! openBracketForm
        let! exprs = readPackagesOrExprs
        let exprCount = exprs.Length
        do! closeBracketForm
        let! stop = getPosition
        let optPositions = Some (makeParserPositions start stop)
        let specialId = getNextSpecialId ()
        return SpecialSeries (makeSpecialSeriesRecord DeclarativeExpr exprs exprCount specialId optPositions) }

/// Read a special meta series.
let readSpecialMetaSeries =
    parse {
        let! start = getPosition
        do! skipChar OpenCurlyChar
        do! skipWhitespaces
        let! exprs = readPackagesOrProceduralExprs
        let exprCount = exprs.Length
        do! closeCurlyForm
        let! stop = getPosition
        let optPositions = Some (makeParserPositions start stop)
        let specialId = getNextSpecialId ()
        return SpecialSeries (makeSpecialSeriesRecord MetaExpr exprs exprCount specialId optPositions) }

let skipBy pred (terminatorChars : string) (stream : _ CharStream) (start : _ CharStreamState) =
    let middle = stream.State
    let result = pred () && isAnyOf terminatorChars (stream.ReadCharOrNewline ())
    if result then
        do stream.BacktrackTo start
        result
    else
        do stream.BacktrackTo middle
        result

let skip (str : string) (terminatorChars : string) (stream : _ CharStream) (start : _ CharStreamState) =
    skipBy (fun () -> stream.Skip str) terminatorChars stream start

let skipNone (stream : 'a CharStream) (start : 'a CharStreamState)=
    do stream.BacktrackTo start
    true

/// Read a bi-recursive procedural expression (may recurse either left or right).
let readBirecursiveProceduralExpr =
    choice
        [attempt readReservedName
         attempt readBoolean
         attempt readCharacter
         attempt readString
         attempt readInt
         attempt readLong
         attempt readFloatOrDouble
         attempt readKeyword
         attempt readSymbol // must come after all stand-alone reads
         attempt readPrefixedProceduralExpr
         fun (stream : _ CharStream) ->
            match stream.Peek () with
            | '(' ->
                let start = stream.State    
                ignore (skipWhitespaces stream)
                ignore (stream.ReadCharOrNewline ())
                if skip ViolationStr WhitespaceCharsAndParens stream start then readViolation stream
                elif skip LambdaStr WhitespaceCharsAndParens stream start then readLambda stream
                elif skip AttemptStr WhitespaceCharsAndParens stream start then readAttempt stream
                elif skip LetStr WhitespaceCharsAndParens stream start then readLet stream
                elif skip ExtendStr WhitespaceCharsAndParens stream start then readExtend stream
                elif skip CaseStr WhitespaceCharsAndParens stream start then readCase stream
                elif skip ConditionStr WhitespaceCharsAndParens stream start then readCondition stream
                elif skip InterveneStr WhitespaceCharsAndParens stream start then readIntervene stream
                elif skip RefStr WhitespaceCharsAndParens stream start then readRef stream
                elif skip GetStr WhitespaceCharsAndParens stream start then readGet stream
                elif skip SetStr WhitespaceCharsAndParens stream start then readSet stream
                elif skip ListStr WhitespaceCharsAndParens stream start then readList stream
                elif skip ArrayStr WhitespaceCharsAndParens stream start then readArray stream
                elif skip CompositeStr WhitespaceCharsAndParens stream start then readComposite stream
                elif skip SelectorStr WhitespaceCharsAndParens stream start then readFunctionalSelector stream
                elif skip SpecialValueStr WhitespaceCharsAndParens stream start then readSpecialValue stream
                elif skipNone stream start then readSeries stream
                else failwith "Could not read operation expression."
            | '{' -> readSpecialMetaSeries stream
            | _ -> pzero stream]

/// Read a dot selector.
let readDotSelector =
    parse {
        let! start = getPosition
        do! skipChar DotChar
        let! stop = getPosition
        do! skipWhitespaces
        let optPositions = Some (makeParserPositions start stop)
        return fun target key ->
            match key with
            | Int _ | Float _ | Symbol _ | Selector _ -> Selector (makeSelectorRecord key target DotSelector optPositions)
            | _ -> makeViolationWithoutBreakpoint ":v/reader/invalidSelectorKeyType" "Selector key must be a number, symbol, or colon selector." }
         
/// Read a double colon selector.
let readDoubleColonSelector =
    parse {
        let! start = getPosition
        do! skipString DoubleColonStr
        let! stop = getPosition
        do! skipWhitespaces
        let optPositions = Some (makeParserPositions start stop)
        return fun target key ->
            match key with
            | Int _ | Float _ | Symbol _ -> Selector (makeSelectorRecord key target DoubleColonSelector optPositions)
            | _ -> makeViolationWithoutBreakpoint ":v/reader/invalidSelectorKeyType" "Selector key must be a number or symbol." }

/// Read a double colon selector bi-recursively.
do readProceduralExprRef :=
    chainl1
        (chainr1 readBirecursiveProceduralExpr readDoubleColonSelector)
        readDotSelector

/// Read a declarative expression.
do readDeclarativeExprRef :=
    choice
        [attempt readReservedName
         attempt readPrefixedExpr
         fun (stream : _ CharStream) ->
            match stream.Peek () with
            | '[' ->
                let start = stream.State
                ignore (stream.ReadCharOrNewline ())  
                ignore (skipWhitespaces stream)
                if skip DefinitionStr WhitespaceCharsAndBrackets stream start then readVariableOrFunction stream
                elif skip StructureStr WhitespaceCharsAndBrackets stream start then readStructure stream
                elif skip ProtocolStr WhitespaceCharsAndBrackets stream start then readProtocol stream
                elif skip InstanceStr WhitespaceCharsAndBrackets stream start then readInstance stream
                elif skip AffirmationStr WhitespaceCharsAndBrackets stream start then readAffirmation stream
                elif skip UsingFileStr WhitespaceCharsAndBrackets stream start then readUsingFile stream
                elif skip UsingLanguageStr WhitespaceCharsAndBrackets stream start then readUsingLanguage stream
                elif skipNone stream start then readSpecialDeclarativeSeries stream
                else failwith "Could not read declaration expression."
            | _ -> pzero stream]

/// Read an expression.
do readExprRef :=
    choice
        [attempt readProceduralExpr
         attempt readDeclarativeExpr]