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
                     "nil " + // the empty keyword
                     "v2 xOf yOf xAs yAs " + // vector operations
                     "some none isNone isSome map " +
                     // TODO: "either isLeft isRight left right " +
                     "tuple unit fst snd thd fth fif nth " +
                     "list head tail cons empty isEmpty notEmpty fold filter product sum contains " + // [list] is empty list
                     // TODO: "ring add remove " +
                     // TODO: "table tryFind find " +
                     "nix " + // the empty keyphrase
                     "let fun if cond try break get set do " +
                     "variableStream eventStream propertyStream " +
                     "define variable equate handle " +
                     // TODO?: "onInit onRegister onUnregister onUpdate onPostUpdate onActualize" +
                     "tickRate tickTime updateCount",
                     "");
          TypeConverter (typeof<ExprConverter>);
          NoComparison>]
        Expr =

        (* Primitive Value Types *)
        | Violation of Name list * string * Origin option
        | Unit of Origin option // constructed as []
        | Bool of bool * Origin option
        | Int of int * Origin option
        | Int64 of int64 * Origin option
        | Single of single * Origin option
        | Double of double * Origin option
        | Vector2 of Vector2 * Origin option
        | String of string * Origin option
        | Keyword of string * Origin option

        (* Primitive Data Structures *)
        | Option of Expr option * Origin option
        | Tuple of Map<int, Expr> * Origin option
        | List of Expr list * Origin option
        | Keyphrase of Map<int, Expr> * Origin option
        | Stream of Stream * Origin option

        (* Special Forms *)
        | Binding of string * CachedBinding ref * Origin option
        | Apply of Expr list * Origin option
        | Let of LetBinding * Expr * Origin option
        | LetMany of LetBinding list * Expr * Origin option
        | Fun of string list * int * Expr * bool * obj option * Origin option
        | If of Expr * Expr * Expr * Origin option
        | Match of Expr * (Expr * Expr) list * Origin option
        | Select of (Expr * Expr) list * Origin option
        | Try of Expr * (Name list * Expr) list * Origin option
        | Do of Expr list * Origin option
        | Break of Expr * Origin option
        | Get of string * Origin option
        | GetFrom of string * Expr * Origin option
        | Set of string * Expr * Origin option
        | SetTo of string * Expr * Expr * Origin option
        | Run of Command * Origin option
        | Quote of string * Origin option

        (* Special Declarations - only work at the top level, and always return unit. *)
        // accessible anywhere
        // constructed as [define c 0]
        | Define of string * Expr * Origin option
        // only accessible by variables and equations
        // constructed as [variable v stream]
        | Variable of string * Stream * Origin option
        // constructed as [equate Density stream] or [equate Density ././Player stream]
        // does not allow for relations to parents or siblings, or for a wildcard in the relation
        | Equate of string * obj Relation * Stream * Guid * Origin option
        // constructed as [equate Density ././@ BoxDispatcher stream] or [equate Density ././@ BoxDispatcher Vanilla stream]
        // does not allow for relations to parents or siblings
        | EquateMany of string * obj Relation * Classification * Stream * Guid * Origin option
        // constructed as [handle stream]
        | Handle of Stream * Guid * Origin option

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
            | Option (_, originOpt)
            | Tuple (_, originOpt)
            | List (_, originOpt)
            | Keyphrase (_, originOpt)
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
            | Break (_, originOpt)
            | Get (_, originOpt)
            | GetFrom (_, _, originOpt)
            | Set (_, _, originOpt)
            | SetTo (_, _, _, originOpt)
            | Run (_, originOpt)
            | Quote (_, originOpt)
            | Define (_, _, originOpt)
            | Variable (_, _, originOpt)
            | Equate (_, _, _, _, originOpt)
            | EquateMany (_, _, _, _, _, originOpt)
            | Handle (_, _, originOpt) -> originOpt

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
                        | "some" -> failwithumf ()
                        | "list" -> failwithumf ()
                        | "tuple" -> failwithumf ()
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
                                if List.forall (function Symbols (symbols, _) when List.hasExactly 2 symbols -> true | _ -> false) cases then
                                    let cases = List.map (function Symbols ([condition; consequent], _) -> (condition, consequent) | _ -> failwithumf ()) cases
                                    let cases = List.map (fun (condition, consequent) -> (this.SymbolToExpr condition, this.SymbolToExpr consequent)) cases
                                    Match (input, cases, originOpt) :> obj
                                else Violation ([!!"InvalidMatchForm"], "Invalid match form. Requires 1 or more cases.", originOpt) :> obj
                            | _ -> Violation ([!!"InvalidMatchForm"], "Invalid match form. Requires 1 input and 1 or more cases.", originOpt) :> obj
                        | "select" ->
                            let cases = tail
                            if List.forall (function Symbols (symbols, _) when List.hasExactly 2 symbols -> true | _ -> false) cases then
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

/// Dynamically drives behavior for simulants.
type [<NoComparison>] Script =
    { Constants : (Name * Scripting.Expr) list
      Streams : (Name * Guid * Scripting.Stream * Scripting.Expr) list
      Equalities : (Name * Guid * Scripting.Stream) list
      OnInit : Scripting.Expr }

    static member empty =
        { Constants = []
          Streams = []
          Equalities = []
          OnInit = Scripting.Unit (None) }

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
    
        /// A bottom value for an binding in a procedural frame.
        /// Should ever exist ONLY in an uninitialized procedural frame!
        let BottomValue = Scripting.Violation ([!!"BottomAccess"], "Accessed a bottom value.", None)

        /// A bottom binding for a procedural frame.
        /// Should ever exist ONLY in an uninitialized procedural frame!
        let BottomBinding = (String.Empty, BottomValue)

        let make chooseWorld rebinding topLevel (context : 'p) (world : 'w) =
            { Rebinding = rebinding
              TopLevel = topLevel
              Frames = []
              Streams = Map.empty
              Context = context
              World = chooseWorld world }

        let getRebinding (env : Env<'p, 'g, 'w>) =
            env.Rebinding

        let tryGetStream streamAddress (env : Env<'p, 'g, 'w>) =
            Map.tryFind streamAddress env.Streams

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

        let makeFrame size =
            Array.create size BottomBinding

        let addFrame frame (env : Env<'p, 'g, 'w>) =
            { env with Frames = frame :: env.Frames }

        let tryAddBinding isTopLevel name evaled (env : Env<'p, 'g, 'w>) =
            if isTopLevel && (env.Rebinding || not ^ env.TopLevel.ContainsKey name) then
                env.TopLevel.Add (name, evaled)
                Some env
            else None

        let tryAddDeclarationBinding name binding env =
            tryAddBinding true name binding env

        let addProceduralBinding appendType name binding env =
            match appendType with
            | AddToNewFrame size ->
                let frame = makeFrame size
                frame.[0] <- (name, binding)
                addFrame frame env
            | AddToHeadFrame offset ->
                match env.Frames with
                | frame :: _ ->
                    frame.[offset] <- (name, binding)
                    env
                | [] -> failwithumf ()

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

/// The execution environment for scripts.
type Env<'p, 'g, 'w when 'p :> Participant and 'g :> Participant and 'w :> EventWorld<'g, 'w>> =
    EnvModule.Env<'p, 'g, 'w>