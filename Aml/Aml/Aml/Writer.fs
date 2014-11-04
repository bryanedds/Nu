// Aml - A Modular Language.
// Copyright (C) Bryan Edds, 2012-2014.

namespace Aml
open System
open System.IO
open System.Collections.Generic
open Prime
open Aml.Ast
open Aml.AstModule // fixes issues when loading this file in fsi
open Aml.AmlConstants
open Aml.Primitives
module Writer =

    /// TODO: Consider using lazy lists or difference lists for faster string appending (would Seqs also work?)

    /// Insert a space between strings.
    let private (<+>) x y = x + SpaceStr + y

    /// Wrap a string in brackets.
    let bracketize str = OpenBracketStr + str + CloseBracketStr

    /// Wrap a string list in brackets.
    let bracketizeNames (names : string list) =
        OpenParenStr + String.Join (SpaceStr, names) + CloseParenStr
        
    /// Wrap a string in parentheses.
    let parenthesize str = OpenParenStr + str + CloseParenStr

    /// Wrap a string list in parentheses.
    let parenthesizeNames (names : string list) =
        OpenParenStr + String.Join (SpaceStr, names) + CloseParenStr
        
    /// Wrap a string in curlies.
    let curlize str = OpenCurlyStr + str + CloseCurlyStr

    /// Wrap a string list in curlies.
    let curlizeNames (names : string list) =
        OpenCurlyStr + String.Join (SpaceStr, names) + CloseCurlyStr
        
    /// Wrap a string in triangles.
    let triangulate str = OpenTriangleStr + str + CloseTriangleStr

    /// Wrap a string in special surroundings.
    let specialize specialSeriesType str =
        let (opener, closer) =
            match specialSeriesType with
            | DeclarativeExpr -> (OpenBracketStr, CloseBracketStr)
            | MetaExpr -> (OpenCurlyStr, CloseCurlyStr)
        opener + str + closer

    /// Unite the words of exprs.
    let rec unwordsExprs exprs = (List.map (writeExpr) >> String.concat SpaceStr) exprs

    /// Bracketize exprs.
    and bracketizeExprs exprs = bracketize (unwordsExprs exprs)

    /// Parenthesize exprs.
    and parenthesizeExprs exprs = parenthesize (unwordsExprs exprs)

    /// Curlize exprs.
    and curlizeExprs exprs = curlize (unwordsExprs exprs)

    /// Write a boolean literal.
    and writeBoolean boolean = if boolean.BRValue then TrueStr else FalseStr

    /// Write a character literal.
    and writeCharacter character = BackslashStr + DoubleQuoteStr + character.CRValue.ToString () + DoubleQuoteStr

    /// Write a literal string.
    and writeLiteralString (str : string) =
        // NOTE: doing escape character substitution in-place with a linked-list may prevent speed issues
        let literal =
            str
                .Replace("\\", "\\\\") // NOTE: this line must come first
                .Replace("\u0000", "\\0")
                .Replace("\"", "\\\"")
                .Replace("\a", "\\a")
                .Replace("\b", "\\b")
                .Replace("\f", "\\f")
                .Replace("\n", "\\n")
                .Replace("\r", "\\r")
                .Replace("\t", "\\t")
                .Replace("\v", "\\v")
        DoubleQuoteStr + literal + DoubleQuoteStr

    /// Write a verbatim string.
    and writeVerbatimString (str : string) =
        let verbatim = str.Replace("\"", "\\\"")
        HashStr + DoubleQuoteStr + verbatim + DoubleQuoteStr

    /// Write a string value.
    and writeStringValue stringValue =
        match stringValue.SVType with
        | LiteralString -> writeLiteralString stringValue.SVValue
        | VerbatimString -> writeVerbatimString stringValue.SVValue

    /// Write a string.
    and writeString string =
        let value = string.SRValue
        match value.SVType with
        | LiteralString -> writeLiteralString value.SVValue
        | VerbatimString -> writeVerbatimString value.SVValue

    /// Write multiple values.
    and writeValues writeValue values =
        let valueStrs = List.map (fun value -> writeValue value) values
        List.join SpaceStr valueStrs

    /// Write an option string.
    and writeOptionStr (optStr : string option) =
        match optStr with
        | Some str -> str
        | None -> EmptyStr

    /// Write out an int.
    and writeInt int =
        let value = int.IRValue
        value.ToString ()

    /// Write out a long.
    and writeLong long =
        let value = long.GRValue
        let valueStr = value.ToString ()
        let suffixStr =
            if value > int64 System.Int32.MaxValue ||
               value < int64 System.Int32.MinValue
            then EmptyStr
            else LongSuffixStr
        valueStr + suffixStr

    /// Write out a float.
    and writeFloat float =
        let value = float.FRValue
        if single (int value) = value then value.ToString "F1" else value.ToString ()

    /// Write out a double.
    and writeDouble double =
        let value = double.DRValue
        let valueStr =
            if float (int64 value) = value // NOTE: would this fail on x86's 80-bit doubles?
            then value.ToString "F1"
            else value.ToString ()
        let suffixStr =
            if value > float System.Single.MaxValue ||
               value < float System.Single.MinValue
            then EmptyStr
            else DoubleSuffixStr
        valueStr + suffixStr

    /// Write out a keyword.
    and writeKeyword keyword = keyword.KRValue

    /// Write multiple argument names without surrounding.
    and writeArgNamesFlat (names : string list) =
        String.Join (SpaceStr, names)
        
    /// Write multiple argument names.
    and writeArgNames surroundBy names = surroundBy (writeArgNamesFlat names)

    /// Write an argument.
    and writeArg arg =
        match arg.ArgType with
        | Concrete -> arg.ArgName
        | Abstracting -> triangulate arg.ArgName
        | Labeled -> arg.ArgName + ColonStr <+> writeExpr arg.ArgExpr
        | Variadic -> arg.ArgName + EllipsisStr

    /// Write multiple arguments.
    and writeArgs args surroundBy = surroundBy (writeValues writeArg args)

    /// Write a prefix type.
    and writePrefixType =
        function
        | DollarPrefix -> DollarStr
        | PercentPrefix -> PercentStr
        | AmpersandPrefix -> AmpersandStr
        | AtPrefix -> AtStr

    /// Write a list of items with a space at front when list is non-empty.
    and writeListWithSpace writer (list : 'a list) = if list.IsEmpty then EmptyStr else SpaceStr + writer list

    /// Write multiple elemenets with a space at front if applicable.
    and writeElementsWithSpace (elements : Expr list) = writeListWithSpace (writeExprs) elements

    /// Write a contract with a space at the front when not unit.
    and writeContractWithSpace tag body = if body <> UnitValue then SpaceStr + tag + ColonStr <+> writeExpr body else EmptyStr

    /// Write a composite member.
    and writeMember mem = parenthesize (mem.MemName <+> writeExpr mem.MemExpr)

    /// Write composite members with a space at front if applicable.
    and writeMembersWithSpace (members : MemberDictionary) =
        let memberList = List.ofSeq members.Values
        writeListWithSpace (writeValues writeMember) memberList

    /// Write a test branch.
    and writeTestBranch branch = parenthesize (writeExpr branch.TBTest <+> writeExpr branch.TBBody)

    /// Write test branches with a space at front if applicable.
    and writeTestBranchesWithSpace branches =
        writeListWithSpace (writeValues writeTestBranch) branches

    and writeSelector selector =
        match selector.SelType with
        | FunctionalSelector -> parenthesize (SelectorStr <+> writeExpr selector.SelKey <+> writeExpr selector.SelTarget)
        | DotSelector -> writeExpr selector.SelTarget + DotStr + writeExpr selector.SelKey
        | DoubleColonSelector -> writeExpr selector.SelTarget + DoubleColonStr + writeExpr selector.SelKey

    /// Write a signature.
    and writeSignature signature = bracketize (signature.SigName <+> writeArgs signature.SigArgs bracketize)

    /// Write signatures.
    and writeSignatures signatures = writeValues writeSignature signatures

    /// Write an instance constraint.
    and writeConstraint surroundBy constr = surroundBy (constr.ConstrName <+> writeArgNamesFlat constr.ConstrArgs)

    /// Write type constraints.
    and writeConstraintsWithSpace surroundBy constraints =
        if List.isEmpty constraints then String.Empty
        else SpaceStr + WhereStr + ColonStr <+> surroundBy (writeValues (writeConstraint surroundBy) constraints)

    /// Write opt type constraints.
    and writeOptConstraintsWithSpace surroundBy optConstraints =
        match optConstraints with
        | Some constraints -> writeConstraintsWithSpace surroundBy constraints
        | None -> EmptyStr

    /// Write an attempt branch.
    and writeAttemptBranch branch =
        branch.ABCategory <+> writeExpr branch.ABBody

    /// Write multiple attempt branches.
    and writeAttemptBranches branches =
        writeValues writeAttemptBranch branches

    /// Write an intervention branch.
    and writeInterventionBranch branch =
        branch.IBCategory <+> writeExpr branch.IBBody + writeHideWithSpace branch.IBHide

    /// Write multiple intervention branches.
    and writeInterventionBranches branches =
        writeValues writeInterventionBranch branches

    /// Write a let binding.
    and writeLetBinding binding =
        match binding with
        | LetVariable (name, body) -> parenthesize (name <+> writeExpr body)
        | LetFunction (name, args, _, body, optConstraints, pre, post, _) -> parenthesize (name <+> writeArgs args parenthesize + writeOptConstraintsWithSpace parenthesize optConstraints + writeContractWithSpace PreconditionStr pre + writeContractWithSpace PostconditionStr post <+> writeExpr body)

    /// Write multiple let bindings.
    and writeLetBindings bindings =
        writeValues writeLetBinding bindings

    /// Write an Aml file's reload value.
    and writeHideWithSpace hide =
        if hide then SpaceStr + TrueStr else EmptyStr

    /// Write an Aml file's reload value.
    and writeReloadWithSpace reload =
        if reload then SpaceStr + TrueStr else EmptyStr

    /// Write an expression as a string.
    and writeExpr exprToWrite =
        match exprToWrite with
        | Violation violation -> parenthesize (ViolationStr <+> violation.VioCategory <+> writeStringValue violation.VioMessage <+> writeExpr violation.VioData)
        | Boolean boolean -> writeBoolean boolean
        | Character character -> writeCharacter character
        | String string -> writeString string
        | Int int -> writeInt int
        | Long long -> writeLong long
        | Float float -> writeFloat float
        | Double double -> writeDouble double
        | Keyword keyword -> writeKeyword keyword
        | Package package -> package.PkgName + ColonStr <+> writeExpr package.PkgExpr
        | Prefixed prefixed -> writePrefixType prefixed.PxdType + writeExpr prefixed.PxdExpr
        | Symbol symbol -> symbol.SymName
        | Dispatch dispatch -> OpenMultilineCommentStr + DispatchStr <+> dispatch.DispName + CloseMultilineCommentStr
        | SpecialValue specialValue -> parenthesize (SpecialValueStr <+> specialValue.SVLanguageName <+> writeExpr specialValue.SVExpr)
        | SpecialObject specialObject -> writeExpr (SpecialValue (makeSpecialValueRecord true (xstring specialObject.SOLanguageGuid) (specialObject.SOContent.ToValue ()) specialObject.SOOptPositions))
        | Series series -> parenthesizeExprs series.SerExprs
        | Lambda lambda -> parenthesize (FunStr <+> writeArgs lambda.LamArgs parenthesize + writeContractWithSpace PreconditionStr lambda.LamPre + writeContractWithSpace PostconditionStr lambda.LamPost <+> writeExpr lambda.LamBody)
        | Attempt attemptRecord -> parenthesize (AttemptStr <+> writeExpr attemptRecord.AttemptBody <+> writeAttemptBranches attemptRecord.AttemptBranches)
        | Let letRecord -> parenthesize (LetStr <+> writeLetBindings letRecord.LetBindings <+> writeExpr letRecord.LetBody)
        | Extend extend -> parenthesize (ExtendStr <+> writeExpr extend.ExtTarget + writeMembersWithSpace extend.ExtMembers)
        | Case case -> parenthesize (CaseStr <+> writeExpr case.CaseTarget + writeTestBranchesWithSpace case.CaseBranches)
        | Condition condition -> parenthesize (ConditionStr + writeTestBranchesWithSpace condition.CondBranches)
        | Intervene intervene -> parenthesize (InterveneStr <+> writeExpr intervene.ItvBody <+> writeInterventionBranches intervene.ItvBranches)
        | Ref reference -> parenthesize (RefStr <+> writeExpr reference.RefExpr)
        | Get get -> parenthesize (GetStr <+> writeExpr get.GetTarget)
        | Set set -> parenthesize (SetStr <+> writeExpr set.SetTarget <+> writeExpr set.SetInjection)
        | List list -> parenthesize (ListStr + writeElementsWithSpace list.ListElements)
        | Array array -> parenthesize (ArrayStr + writeElementsWithSpace (List.ofArray array.ArrElements))
        | Composite composite -> parenthesize (CompositeStr + writeMembersWithSpace composite.CompMembers)
        | Selector selector -> writeSelector selector
        | Variable variable -> bracketize (DefinitionStr <+> variable.VarName <+> writeExpr variable.VarBody)
        | Function fn -> bracketize (DefinitionStr <+> fn.FnName <+> writeArgs fn.FnArgs bracketize + writeOptConstraintsWithSpace bracketize fn.FnOptConstraints + writeContractWithSpace PreconditionStr fn.FnPre + writeContractWithSpace PostconditionStr fn.FnPost <+> writeExpr fn.FnBody)
        | Structure structure -> bracketize (StructureStr <+> structure.StructName <+> bracketizeNames structure.StructMemberNames + writeOptConstraintsWithSpace bracketize structure.StructOptConstraints + writeContractWithSpace RequirementStr structure.StructReq)
        | Protocol protocol -> bracketize (ProtocolStr <+> protocol.ProtoName <+> bracketize (protocol.ProtoArg) + writeOptConstraintsWithSpace bracketize protocol.ProtoOptConstraints <+> writeSignatures protocol.ProtoSignatures)
        | Instance instance -> bracketize (InstanceStr <+> instance.InstProtocolName <+> writeArgNames bracketize instance.InstArgs + writeConstraintsWithSpace bracketize instance.InstConstraints <+> writeExprs instance.InstFunctions)
        | Affirmation affirmation -> bracketize (AffirmationStr <+> affirmation.AffName <+> writeExpr affirmation.AffBody)
        | UsingFile usingFile -> bracketize (UsingFileStr <+> writeLiteralString usingFile.UFPath + writeReloadWithSpace usingFile.UFReload)
        | UsingLanguage usingLanguage -> bracketize (UsingLanguageStr <+> writeLiteralString usingLanguage.ULPath <+> usingLanguage.ULType)
        | SpecialSeries specialSeries -> specialize specialSeries.SSType (writeExprs specialSeries.SSExprs)

    /// Write multiple expressions as a string.
    and writeExprs exprs = writeValues writeExpr exprs

    /// Try to write an expression from parser positions.
    let tryWriteExprFromPositions optPositions trimStartOfFirstLine =
        match optPositions with
        | Some positions ->
            let start = positions.ParStart
            let usesFile = start.StreamName <> null && start.StreamName.Length <> 0 // NOTE: it seems like it's a bug for FParsec to allow StreamName to be null...
            if usesFile then
                let fileLines = File.ReadAllLines start.StreamName
                let firstFileLine = fileLines.[int start.Line - 1]
                let firstFileLineTrimmed = if trimStartOfFirstLine then firstFileLine.Trim () else firstFileLine
                let start = positions.ParStart
                let startFile = if start.StreamName.Length <> 0 then start.StreamName else "[N/A]"
                let startStr = "[Ln: " + xstring start.Line + ", Col: " + string start.Column + ", In: " + startFile + "]"
                let result = firstFileLineTrimmed + "\n  " + startStr
                Some result
            else None
        | None -> None
    
    /// Write a stack frame.
    let writeStackFrame expr =
        let positions = getOptPositions expr
        let optResult = tryWriteExprFromPositions positions true
        match optResult with
        | Some result -> result
        | None -> writeExpr expr + "\n  [In: Ast]"
    
    /// Write a stack trace.
    let writeStackTrace env =
        let results = List.map writeStackFrame env.EnvDebugInfo.DIStackTrace
        let result = String.Join ("\n", results)
        result

    /// Make a violation with an error message using an environment's parser positions.
    let makeViolationWithPositions category message env =
        let optExpr = match env.EnvDebugInfo.DIExprTrace with [] -> None | exprTrace -> Some exprTrace.Head
        let optPositions = match optExpr with Some expr -> getOptPositions expr | None -> None
        let positionDescription =
            // TODO: remove the code duplication with 'Primitives.tryWriteExprFromPositions'.
            match optPositions with
            | Some positions ->
                let start = positions.ParStart
                let stop = positions.ParStop
                let usesFile = start.StreamName <> null && start.StreamName.Length <> 0
                let exprStr =
                    if usesFile then
                        let fileLines = System.IO.File.ReadAllLines start.StreamName
                        let firstFileLine = fileLines.[int start.Line - 1]
                        firstFileLine
                    else
                        match env.EnvDebugInfo.DIOptFirstReplLine with
                        | Some firstReplLine -> firstReplLine
                        | None -> "[Could not write expression...]"
                let underline = (String.replicate (int start.Column - 1) " ") + "^"
                let fileStr = if usesFile then start.StreamName else "Ast"
                let stackTrace = writeStackTrace env
                "\nError encountered in the following expression:\n\n" +
                    exprStr +
                    "\n" +
                    underline +
                    "\n  Start Pos: [Ln: " +
                    xstring start.Line +
                    ", Col: " +
                    xstring start.Column +
                    "]\n  Stop Pos:  [Ln: " +
                    xstring stop.Line +
                    ", Col: " +
                    xstring stop.Column +
                    "]\n  In:        " +
                    fileStr +
                    "\n\nStack trace:\n\n" +
                    stackTrace
            | None -> EmptyStr
        let messagePositioned = message + positionDescription
        makeViolation category messagePositioned