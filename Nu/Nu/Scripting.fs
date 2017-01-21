// Nu Game Engine.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Nu
open System
open System.Collections.Generic
open System.ComponentModel
open OpenTK
open Prime
open Nu

/// A scripting language for Nu that is hoped to eventually be a cross between Elm and Unreal Blueprints.
/// TODO: down the line, I'd like to separate the lispy core of the scripting language from the Nu-specific vocabulary,
/// placing the core language into Prime and extending the Nu-specific requirements with a language plug-in of sorts.
module Scripting =

    /// Commands to the engine, EG - applyLinearImpulse, playSound, etc.
    type [<NoComparison>] Command =
        { CommandName : string
          CommandArgs : Expr list }

    and [<CompilationRepresentation (CompilationRepresentationFlags.UseNullAsTrueValue); NoComparison>] CachedBinding =
        | UncachedBinding
        | DeclarationBinding of Expr
        | ProceduralBinding of int * int

    and [<NoComparison>] LetBinding =
        | LetVariable of string * Expr
        | LetFunction of string * string list * Expr

    and [<NoComparison>] Stream =
        // constructed as [variableStream v]
        | VariableStream of string
        // constructed as [eventStream X/Y/Z]
        | EventStream of Expr
        // constructed as [propertyStream P] or [propertyStream P ././.]
        | PropertyStream of string * Expr
        // constructed as [propertyStream P ././@ EntityDispatcher] or [propertyStream P ././@ EntityDispatcher Vanilla]
        // does not allow for wildcards in the relation
        | PropertyStreamMany of string * Expr * Classification
        // not constructable by user. Weakly-typed to simplify type declarations
        | ComputedStream of obj // actual type is Prime.Stream<'p, 'w when 'p :> Participant and 'w :> EventWorld<'w>>

    and [<Syntax    ("pow root sqr sqrt " +
                     "floor ceiling truncate round exp log " +
                     "sin cos tan asin acos atan " +
                     "length normal " +
                     "cross dot " +
                     "violation bool int int64 single double string " +
                     "nil nix " + // the empty keyword / keyphrase
                     "v2 xOf yOf xAs yAs " + // vector operations
                     "pair tuple unit fst snd thd fth fif nth " +
                     "some none isNone isSome contains map " +
                     // TODO: "either isLeft isRight left right " +
                     "list head tail cons empty isEmpty notEmpty filter fold reduce " +
                     "ring add remove " +
                     "table tryFind find " +
                     "let fun if cond try break get set run do " +
                     "variableStream eventStream propertyStream " +
                     "define variable equate handle " +
                     "tickRate tickTime updateCount",
                     "");
          TypeConverter (typeof<ExprConverter>);
          CustomEquality;
          CustomComparison>]
        Expr =

        (* Primitive Value Types *)
        | Violation of Name list * string * SymbolOrigin option
        | Unit of SymbolOrigin option // constructed as []
        | Bool of bool * SymbolOrigin option
        | Int of int * SymbolOrigin option
        | Int64 of int64 * SymbolOrigin option
        | Single of single * SymbolOrigin option
        | Double of double * SymbolOrigin option
        | Vector2 of Vector2 * SymbolOrigin option
        | String of string * SymbolOrigin option
        | Keyword of string * SymbolOrigin option

        (* Primitive Data Structures *)
        | Tuple of Map<int, Expr> * SymbolOrigin option
        | Keyphrase of Expr * Map<int, Expr> * SymbolOrigin option
        | Option of Expr option * SymbolOrigin option
        | List of Expr list * SymbolOrigin option
        | Ring of Set<Expr> * SymbolOrigin option
        | Table of Map<Expr, Expr> * SymbolOrigin option
        | Stream of Stream * SymbolOrigin option

        (* Special Forms *)
        | Binding of string * CachedBinding ref * SymbolOrigin option
        | Apply of Expr list * SymbolOrigin option
        | Let of LetBinding * Expr * SymbolOrigin option
        | LetMany of LetBinding list * Expr * SymbolOrigin option
        | Fun of string list * int * Expr * bool * obj option * SymbolOrigin option
        | If of Expr * Expr * Expr * SymbolOrigin option
        | Match of Expr * (Expr * Expr) list * SymbolOrigin option
        | Select of (Expr * Expr) list * SymbolOrigin option
        | Try of Expr * (Name list * Expr) list * SymbolOrigin option
        | Do of Expr list * SymbolOrigin option
        | Run of Command * SymbolOrigin option
        | Break of Expr * SymbolOrigin option
        | Get of string * SymbolOrigin option
        | GetFrom of string * Expr * SymbolOrigin option
        | Set of string * Expr * SymbolOrigin option
        | SetTo of string * Expr * Expr * SymbolOrigin option
        | Quote of string * SymbolOrigin option

        (* Special Declarations - only work at the top level, and always return unit. *)
        // accessible anywhere
        // constructed as [define c 0]
        | Define of string * Expr * SymbolOrigin option
        // only accessible by variables and equations
        // constructed as [variable v stream]
        | Variable of string * Stream * SymbolOrigin option
        // constructed as [equate Density stream] or [equate Density ././Player stream]
        // does not allow for relations to parents or siblings, or for a wildcard in the relation
        | Equate of string * obj Relation * Stream * Guid * SymbolOrigin option
        // constructed as [equate Density ././@ BoxDispatcher stream] or [equate Density ././@ BoxDispatcher Vanilla stream]
        // does not allow for relations to parents or siblings
        | EquateMany of string * obj Relation * Classification * Stream * Guid * SymbolOrigin option
        // constructed as [handle stream]
        | Handle of Stream * Guid * SymbolOrigin option

        static member getOriginOpt term =
            match term with
            | Violation (_, _, originOpt)
            | Unit originOpt
            | Bool (_, originOpt)
            | Int (_, originOpt)
            | Int64 (_, originOpt)
            | Single (_, originOpt)
            | Double (_, originOpt)
            | Vector2 (_, originOpt)
            | String (_, originOpt)
            | Keyword (_, originOpt)
            | Tuple (_, originOpt)
            | Keyphrase (_, _, originOpt)
            | Option (_, originOpt)
            | List (_, originOpt)
            | Ring (_, originOpt)
            | Table (_, originOpt)
            | Stream (_, originOpt)
            | Binding (_, _, originOpt)
            | Apply (_, originOpt)
            | Let (_, _, originOpt)
            | LetMany (_, _, originOpt)
            | Fun (_, _, _, _, _, originOpt)
            | If (_, _, _, originOpt)
            | Match (_, _, originOpt)
            | Select (_, originOpt)
            | Try (_, _, originOpt)
            | Do (_, originOpt)
            | Run (_, originOpt)
            | Break (_, originOpt)
            | Get (_, originOpt)
            | GetFrom (_, _, originOpt)
            | Set (_, _, originOpt)
            | SetTo (_, _, _, originOpt)
            | Quote (_, originOpt)
            | Define (_, _, originOpt)
            | Variable (_, _, originOpt)
            | Equate (_, _, _, _, originOpt)
            | EquateMany (_, _, _, _, _, originOpt)
            | Handle (_, _, originOpt) -> originOpt

        static member equals left right =
            match (left, right) with
            | (Violation (leftNames, leftError, _), Violation (rightNames, rightError, _)) -> (leftNames, leftError) = (rightNames, rightError)
            | (Unit _, Unit _) -> true
            | (Bool (left, _), Bool (right, _)) -> left = right
            | (Int (left, _), Int (right, _)) -> left = right
            | (Int64 (left, _), Int64 (right, _)) -> left = right
            | (Single (left, _), Single (right, _)) -> left = right
            | (Double (left, _), Double (right, _)) -> left = right
            | (Vector2 (left, _), Vector2 (right, _)) -> left = right
            | (String (left, _), String (right, _)) -> left = right
            | (Keyword (left, _), Keyword (right, _)) -> left = right
            | (Tuple (left, _), Tuple (right, _)) -> left = right
            | (Keyphrase (leftKeyword, leftExprs, _), Keyphrase (rightKeyword, rightExprs, _)) -> (leftKeyword, leftExprs) = (rightKeyword, rightExprs)
            | (Option (left, _), Option (right, _)) -> left = right
            | (List (left, _), List (right, _)) -> left = right
            | (Ring (left, _), Ring (right, _)) -> left = right
            | (Table (left, _), Table (right, _)) -> left = right
            | (_, _) -> false

        static member compare left right =
            match (left, right) with
            | (Violation (leftNames, leftError, _), Violation (rightNames, rightError, _)) -> compare (leftNames, leftError) (rightNames, rightError)
            | (Unit _, Unit _) -> 0
            | (Bool (left, _), Bool (right, _)) -> compare left right
            | (Int (left, _), Int (right, _)) -> compare left right
            | (Int64 (left, _), Int64 (right, _)) -> compare left right
            | (Single (left, _), Single (right, _)) -> compare left right
            | (Double (left, _), Double (right, _)) -> compare left right
            | (Vector2 (left, _), Vector2 (right, _)) -> compare (left.X, left.Y) (right.X, right.Y) // TODO: comparison for OpenTK.Vector2!
            | (String (left, _), String (right, _)) -> compare left right
            | (Keyword (left, _), Keyword (right, _)) -> compare left right
            | (Tuple (left, _), Tuple (right, _)) -> compare left right
            | (Keyphrase (leftKeyword, leftExprs, _), Keyphrase (rightKeyword, rightExprs, _)) -> compare (leftKeyword, leftExprs) (rightKeyword, rightExprs)
            | (Option (left, _), Option (right, _)) -> compare left right
            | (List (left, _), List (right, _)) -> compare left right
            | (Ring (left, _), Ring (right, _)) -> compare left right
            | (Table (left, _), Table (right, _)) -> compare left right
            | (_, _) -> -1

        // TODO: check if we can trust the hash function to be efficient on value types...
        override this.GetHashCode () =
            match this with
            | Violation (names, error, _) -> hash names ^^^ hash error
            | Unit _ -> 0
            | Bool (value, _) -> hash value
            | Int (value, _) -> hash value
            | Int64 (value, _) -> hash value
            | Single (value, _) -> hash value
            | Double (value, _) -> hash value
            | Vector2 (value, _) -> hash value
            | String (value, _) -> hash value
            | Keyword (value, _) -> hash value
            | Tuple (value, _) -> hash value
            | Keyphrase (valueKeyword, valueExprs, _) -> hash (valueKeyword, valueExprs)
            | Option (value, _) -> hash value
            | List (value, _) -> hash value
            | Ring (value, _) -> hash value
            | Table (value, _) -> hash value
            | _ -> -1

        override this.Equals that =
            match that with
            | :? Expr as that -> Expr.equals this that
            | _ -> failwithumf ()

        interface IComparable<Expr> with
            member this.CompareTo that =
                Expr.compare this that

        interface IComparable with
            member this.CompareTo that =
                match that with
                | :? Expr as that -> (this :> IComparable<Expr>).CompareTo that
                | _ -> failwithumf ()

    /// Converts Expr types.
    and ExprConverter () =
        inherit TypeConverter ()

        member this.SymbolToExpr symbol =
            this.ConvertFrom symbol :?> Expr

        member this.SymbolsToLetBindingOpt bindingSymbols =
            match bindingSymbols with
            | [Atom (bindingName, _); bindingBody] ->
                let binding = LetVariable (bindingName, this.SymbolToExpr bindingBody)
                Some binding
            | [Atom (bindingName, _); Symbols (bindingArgs, _); bindingBody] ->
                let (bindingArgs, bindingErrors) = List.split (function Atom _ -> true | _ -> false) bindingArgs
                if List.isEmpty bindingErrors then
                    let bindingArgs = List.map (function Atom (arg, _) -> arg | _ -> failwithumf ()) bindingArgs
                    let binding = LetFunction (bindingName, bindingArgs, this.SymbolToExpr bindingBody)
                    Some binding
                else None
            | _ -> None

        override this.CanConvertTo (_, destType) =
            destType = typeof<Symbol> ||
            destType = typeof<Expr>

        override this.ConvertTo (_, _, source, destType) =
            if destType = typeof<Symbol> then Symbols ([], None) :> obj // TODO: implement
            elif destType = typeof<Expr> then source
            else failconv "Invalid ExprConverter conversion to source." None

        override this.CanConvertFrom (_, sourceType) =
            sourceType = typeof<Symbol> ||
            sourceType = typeof<Expr>

        override this.ConvertFrom (_, _, source) =
            match source with
            | :? Symbol as symbol ->
                match symbol with
                | Prime.Atom (str, originOpt) ->
                    match str with
                    | "true" -> Bool (true, originOpt) :> obj
                    | "false" -> Bool (false, originOpt) :> obj
                    | "none" -> Option (None, originOpt) :> obj
                    | "empty" -> List (List.empty, originOpt) :> obj
                    | _ ->
                        let firstChar = str.[0]
                        if firstChar = '.' || Char.IsUpper firstChar
                        then Keyword (str, originOpt) :> obj
                        else Binding (str, ref UncachedBinding, originOpt) :> obj
                | Prime.Number (str, originOpt) ->
                    match Int32.TryParse str with
                    | (false, _) ->
                        match Int64.TryParse str with
                        | (false, _) ->
                            if str.EndsWith "f" || str.EndsWith "F" then
                                let str = str.Substring(0, str.Length - 1)
                                match Single.TryParse str with
                                | (true, single) -> Single (single, originOpt) :> obj
                                | (false, _) -> Violation ([!!"InvalidNumberForm"], "Unexpected number parse failure.", originOpt) :> obj
                            else
                                let str = if str.EndsWith "d" || str.EndsWith "D" then str.Substring(0, str.Length - 1) else str
                                match Double.TryParse (str, Globalization.NumberStyles.Float, Globalization.CultureInfo.CurrentCulture) with
                                | (true, double) -> Double (double, originOpt) :> obj
                                | (false, _) -> Violation ([!!"InvalidNumberForm"], "Unexpected number parse failure.", originOpt) :> obj
                        | (true, int64) -> Int64 (int64, originOpt) :> obj
                    | (true, int) -> Int (int, originOpt) :> obj
                | Prime.String (str, originOpt) -> String (str, originOpt) :> obj
                | Prime.Quote (str, originOpt) -> Quote (str, originOpt) :> obj
                | Prime.Symbols (symbols, originOpt) ->
                    match symbols with
                    | Atom (name, _) :: tail ->
                        match name with
                        | "violation" ->
                            match tail with
                            | [Prime.Atom (tagStr, _)]
                            | [Prime.String (tagStr, _)] ->
                                try let tagName = !!tagStr in Violation (Name.split [|'/'|] tagName, "User-defined error.", originOpt) :> obj
                                with exn -> Violation ([!!"InvalidViolationForm"], "Invalid violation form. Violation tag must be composed of 1 or more valid names.", originOpt) :> obj
                            | [Prime.Atom (tagStr, _); Prime.String (errorMsg, _)]
                            | [Prime.String (tagStr, _); Prime.String (errorMsg, _)] ->
                                try let tagName = !!tagStr in Violation (Name.split [|'/'|] tagName, errorMsg, originOpt) :> obj
                                with exn -> Violation ([!!"InvalidViolationForm"], "Invalid violation form. Violation tag must be composed of 1 or more valid names.", originOpt) :> obj
                            | _ -> Violation ([!!"InvalidViolationForm"], "Invalid violation form. Requires 1 tag.", originOpt) :> obj
                        | "let" ->
                            match tail with
                            | [] -> Violation ([!!"InvalidLetForm"], "Invalid let form. TODO: more info.", originOpt) :> obj
                            | [_] -> Violation ([!!"InvalidLetForm"], "Invalid let form. TODO: more info.", originOpt) :> obj
                            | [binding; body] ->
                                match binding with
                                | Symbols (bindingSymbols, originOpt) ->
                                    match this.SymbolsToLetBindingOpt bindingSymbols with
                                    | Some binding -> Let (binding, this.SymbolToExpr body, originOpt) :> obj
                                    | None -> Violation ([!!"InvalidLetForm"], "Invalid let form. TODO: more info.", originOpt) :> obj
                                | _ -> Violation ([!!"InvalidLetForm"], "Invalid let form. TODO: more info.", originOpt) :> obj
                            | bindingsAndBody ->
                                let (bindings, body) = (List.allButLast bindingsAndBody, List.last bindingsAndBody)
                                let (bindings, bindingErrors) = List.split (function Symbols ([_; _], _) -> true | _ -> false) bindings
                                if List.isEmpty bindingErrors then
                                    let bindings = List.map (function Symbols ([_; _] as binding, _) -> binding | _ -> failwithumf ()) bindings
                                    let bindingOpts = List.map this.SymbolsToLetBindingOpt bindings
                                    let (bindingOpts, bindingErrors) = List.split Option.isSome bindingOpts
                                    if List.isEmpty bindingErrors then
                                        let bindings = List.definitize bindingOpts
                                        LetMany (bindings, this.SymbolToExpr body, originOpt) :> obj
                                    else Violation ([!!"InvalidLetForm"], "Invalid let form. TODO: more info.", originOpt) :> obj
                                else Violation ([!!"InvalidLetForm"], "Invalid let form. TODO: more info.", originOpt) :> obj
                        | "fun" ->
                            match tail with
                            | [args; body] ->
                                match args with
                                | Symbols (args, originOpt) ->
                                    if List.forall (function Atom _ -> true | _ -> false) args then
                                        let args = List.map (function Atom (arg, _) -> arg | _ -> failwithumf ()) args
                                        Fun (args, List.length args, this.SymbolToExpr body, false, None, originOpt) :> obj
                                    else Violation ([!!"InvalidFunForm"], "Invalid fun form. TODO: more info.", originOpt) :> obj
                                | _ -> Violation ([!!"InvalidFunForm"], "Invalid fun form. TODO: more info.", originOpt) :> obj
                            | _ -> Violation ([!!"InvalidFunForm"], "Invalid fun form. TODO: more info.", originOpt) :> obj
                        | "if" ->
                            match tail with
                            | [condition; consequent; alternative] -> If (this.SymbolToExpr condition, this.SymbolToExpr consequent, this.SymbolToExpr alternative, originOpt) :> obj
                            | _ -> Violation ([!!"InvalidIfForm"], "Invalid if form. Requires 3 arguments.", originOpt) :> obj
                        | "match" ->
                            match tail with
                            | input :: cases ->
                                let input = this.SymbolToExpr input
                                if List.forall (function Symbols ([_; _], _) -> true | _ -> false) cases then
                                    let cases = List.map (function Symbols ([condition; consequent], _) -> (condition, consequent) | _ -> failwithumf ()) cases
                                    let cases = List.map (fun (condition, consequent) -> (this.SymbolToExpr condition, this.SymbolToExpr consequent)) cases
                                    Match (input, cases, originOpt) :> obj
                                else Violation ([!!"InvalidMatchForm"], "Invalid match form. Requires 1 or more cases.", originOpt) :> obj
                            | _ -> Violation ([!!"InvalidMatchForm"], "Invalid match form. Requires 1 input and 1 or more cases.", originOpt) :> obj
                        | "select" ->
                            let cases = tail
                            if List.forall (function Symbols ([_; _], _) -> true | _ -> false) cases then
                                let cases = List.map (function Symbols ([condition; consequent], _) -> (condition, consequent) | _ -> failwithumf ()) cases
                                let cases = List.map (fun (condition, consequent) -> (this.SymbolToExpr condition, this.SymbolToExpr consequent)) cases
                                Select (cases, originOpt) :> obj
                            else Violation ([!!"InvalidSelectForm"], "Invalid select form. Requires 1 or more cases.", originOpt) :> obj
                        | "try" ->
                            match tail with
                            | [body; Prime.Symbols (handlers, _)] ->
                                let handlerEirs =
                                    List.mapi
                                        (fun i handler ->
                                            match handler with
                                            | Prime.Symbols ([Prime.Atom (categoriesStr, _); handlerBody], _) ->
                                                Right (Name.split [|'/'|] !!categoriesStr, handlerBody)
                                            | _ ->
                                                Left ("Invalid try handler form for handler #" + scstring (inc i) + ". Requires 1 path and 1 body."))
                                        handlers
                                let (errors, handlers) = Either.split handlerEirs
                                match errors with
                                | [] -> Try (this.SymbolToExpr body, List.map (mapSnd this.SymbolToExpr) handlers, originOpt) :> obj
                                | error :: _ -> Violation ([!!"InvalidTryForm"], error, originOpt) :> obj
                            | _ -> Violation ([!!"InvalidTryForm"], "Invalid try form. Requires 1 body and a handler list.", originOpt) :> obj
                        | "do" ->
                            match tail with
                            | [] -> Violation ([!!"InvalidDoForm"], "Invalid do form. Requires 1 or more sub-expressions.", originOpt) :> obj
                            | symbols ->
                                let exprs = List.map this.SymbolToExpr symbols
                                Do (exprs, originOpt) :> obj
                        | "break" ->
                            let content = this.SymbolToExpr (Symbols (tail, originOpt))
                            Break (content, originOpt) :> obj
                        | "get" ->
                            match tail with
                            | Prime.Atom (nameStr, originOpt) :: tail2
                            | Prime.String (nameStr, originOpt) :: tail2 ->
                                match tail2 with
                                | [] -> Get (nameStr, originOpt) :> obj
                                | [relation] -> GetFrom (nameStr, this.SymbolToExpr relation, originOpt) :> obj
                                | _ -> Violation ([!!"InvalidGetForm"], "Invalid get form. Requires a name and an optional relation expression.", originOpt) :> obj
                            | _ -> Violation ([!!"InvalidGetForm"], "Invalid get form. Requires a name and an optional relation expression.", originOpt) :> obj
                        | "set" ->
                            match tail with
                            | Prime.Atom (nameStr, originOpt) :: value :: tail2
                            | Prime.String (nameStr, originOpt) :: value :: tail2 ->
                                match tail2 with
                                | [] -> Set (nameStr, this.SymbolToExpr value, originOpt) :> obj
                                | [relation] -> SetTo (nameStr, this.SymbolToExpr value, this.SymbolToExpr relation, originOpt) :> obj
                                | _ -> Violation ([!!"InvalidSetForm"], "Invalid set form. Requires a name, a value expression, and an optional relation expression.", originOpt) :> obj
                            | _ -> Violation ([!!"InvalidSetForm"], "Invalid set form. Requires a name, a value expression, and an optional relation expression.", originOpt) :> obj
                        | "variableStream" ->
                            match tail with
                            | [Prime.Atom (nameStr, originOpt)]
                            | [Prime.String (nameStr, originOpt)] -> Stream (VariableStream nameStr, originOpt) :> obj
                            | _ -> Violation ([!!"InvalidVariableStreamForm"], "Invalid variable stream form. Requires a name.", originOpt) :> obj
                        | "eventStream" ->
                            match tail with
                            | [relation] -> Stream (EventStream (this.SymbolToExpr relation), originOpt) :> obj
                            | _ -> Violation ([!!"InvalidEventStreamForm"], "Invalid event stream form. Requires a relation expression.", originOpt) :> obj
                        | _ -> Apply (List.map this.SymbolToExpr symbols, originOpt) :> obj
                    | _ -> Apply (List.map this.SymbolToExpr symbols, originOpt) :> obj
            | :? Expr -> source
            | _ -> failconv "Invalid ExprConverter conversion from source." None

    let UnitValue = Unit None
    let TrueValue = Bool (true, None)
    let FalseValue = Bool (false, None)
    let EmptyStringValue = String (String.Empty, None)
    let ZeroIntValue = Int (0, None)

[<AutoOpen>]
module EnvModule =

    type Frame = (string * Scripting.Expr) array

    /// The manner in which bindings are added to a frame.
    type AddType =
        | AddToNewFrame of Size : int
        | AddToHeadFrame of Offset : int

    /// The execution environment for scripts.
    /// TODO: better encapsulation for Env operations.
    type [<NoEquality; NoComparison>] Env<'p, 'g, 'w when 'p :> Participant and 'g :> Participant and 'w :> EventWorld<'g, 'w>> =
        private
            { Rebinding : bool // rebinding should be enabled in Terminal or perhaps when reloading existing scripts.
              TopLevel : Dictionary<string, Scripting.Expr>
              Frames : Frame list
              Streams : Map<obj Address, Prime.Stream<obj, 'g, 'w> * ('w -> 'w)>
              Context : 'p // TODO: get rid of this and use EventContext instead.
              World : 'w }

    [<RequireQualifiedAccess>]
    module Env =

        let private BottomBinding =
            (String.Empty, Scripting.Violation ([!!"BottomAccess"], "Accessed a bottom value.", None))

        let private makeFrame size =
            Array.create size BottomBinding

        let private addFrame frame (env : Env<'p, 'g, 'w>) =
            { env with Frames = frame :: env.Frames }

        let tryGetDeclarationBinding name (env : Env<'p, 'g, 'w>) =
            match env.TopLevel.TryGetValue name with
            | (true, binding) -> Some binding
            | (false, _) -> None

        let tryGetProceduralBinding name env =
            let refOffset = ref -1
            let refOptIndex = ref None
            let optBinding =
                List.tryFindPlus
                    (fun (frame : Frame) ->
                        refOffset := !refOffset + 1
                        refOptIndex := Array.tryFindIndexRev (fun (bindingName, _) -> name.Equals bindingName) frame // OPTIMIZATION: faster than (=) here
                        match !refOptIndex with
                        | Some index -> Some frame.[index]
                        | None -> None)
                    env.Frames
            match optBinding with
            | Some (_, binding) -> Some (binding, !refOffset, (!refOptIndex).Value)
            | None -> None

        let tryGetBinding name cachedBinding env =
            match !cachedBinding with
            | Scripting.UncachedBinding ->
                match tryGetProceduralBinding name env with
                | None ->
                    match tryGetDeclarationBinding name env with
                    | Some binding ->
                        if not env.Rebinding then
                            let newCachedBinding = Scripting.DeclarationBinding binding
                            cachedBinding := newCachedBinding
                        Some binding
                    | None -> None
                | Some (binding, offset, index) ->
                    let newCachedBinding = Scripting.ProceduralBinding (offset, index)
                    cachedBinding := newCachedBinding
                    Some binding
            | Scripting.DeclarationBinding binding ->
                Some binding
            | Scripting.ProceduralBinding (offset, index) ->
                let frame = (List.skip offset env.Frames).Head
                let (_, binding) = frame.[index]
                Some binding

        let tryAddDeclarationBinding name value env =
            if (env.Rebinding || not ^ env.TopLevel.ContainsKey name) then
                env.TopLevel.Add (name, value)
                Some env
            else None

        let tryAddDeclarationBindings bindings env =
            if not env.Rebinding then 
                let existences = Seq.map (fun (key, _) -> env.TopLevel.ContainsKey key) bindings
                if Seq.notExists id existences then
                    for binding in bindings do env.TopLevel.Add binding
                    Some env
                else None
            else
                for (name, value) in bindings do env.TopLevel.ForceAdd (name, value) |> ignore
                Some env

        let tryGetStream streamAddress (env : Env<'p, 'g, 'w>) =
            Map.tryFind streamAddress env.Streams

        let addProceduralBinding appendType name value env =
            match appendType with
            | AddToNewFrame size ->
                let frame = makeFrame size
                frame.[0] <- (name, value)
                addFrame frame env
            | AddToHeadFrame offset ->
                match env.Frames with
                | frame :: _ ->
                    frame.[offset] <- (name, value)
                    env
                | [] -> failwithumf ()

        let addProceduralBindings appendType bindings env =
            match appendType with
            | AddToNewFrame size ->
                let frame = makeFrame size
                let mutable index = 0
                for binding in bindings do
                    frame.[index] <- binding
                    index <- index + 1
                addFrame frame env
            | AddToHeadFrame start ->
                match env.Frames with
                | frame :: _ ->
                    let mutable index = start
                    for binding in bindings do
                        frame.[index] <- binding
                        index <- index + 1
                    env
                | [] -> failwithumf ()

        let addStream streamAddress address (env : Env<'p, 'g, 'w>) =
            { env with Streams = Map.add streamAddress address env.Streams }

        let getContext (env : Env<'p, 'g, 'w>) =
            env.Context

        let getWorld (env : Env<'p, 'g, 'w>) =
            env.World

        let setWorld chooseWorld world (env : Env<'p, 'g, 'w>) =
            { env with World = chooseWorld world }

        let updateWorld chooseWorld by (env : Env<'p, 'g, 'w>) =
            setWorld chooseWorld (by (getWorld env)) env

        let make chooseWorld rebinding topLevel (context : 'p) (world : 'w) =
            { Rebinding = rebinding
              TopLevel = topLevel
              Frames = []
              Streams = Map.empty
              Context = context
              World = chooseWorld world }

[<AutoOpen>]
module ScriptModule =

    /// Dynamically drives behavior for simulants.
    type [<NoComparison>] Script =
        { Constants : (Name * Scripting.Expr) list
          Streams : (Name * Guid * Scripting.Stream * Scripting.Expr) list
          Equalities : (Name * Guid * Scripting.Stream) list }
    
        static member empty =
            { Constants = []
              Streams = []
              Equalities = [] }

/// The execution environment for scripts.
type Env<'p, 'g, 'w when 'p :> Participant and 'g :> Participant and 'w :> EventWorld<'g, 'w>> =
    EnvModule.Env<'p, 'g, 'w>

/// Dynamically drives behavior for simulants.
type Script = ScriptModule.Script