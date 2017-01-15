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
                     "nix " + // the empty phrase
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
        | Keyword of string * Origin option // constructed as any identifier starting with an uppercase letter

        (* Primitive Data Structures *)
        | Option of Expr option * Origin option
        | Tuple of Map<int, Expr> * Origin option
        | List of Expr list * Origin option
        | Phrase of Map<int, Expr> * Origin option // constructed as [CapitalizedIdentifier ...]
        | Stream of Stream * Origin option

        (* Special Forms *)
        | Binding of string * Origin option
        | Apply of Expr list * Origin option
        | Quote of string * Origin option
        | Let of string * Expr * Origin option
        | LetMany of (string * Expr) list * Origin option
        | Fun of string list * Expr * int * Origin option
        | If of Expr * Expr * Expr * Origin option
        | Cond of (Expr * Expr) list * Origin option
        | Try of Expr * (Name list * Expr) list * Origin option
        | Break of Expr * Origin option
        | Get of string * Origin option
        | GetFrom of string * Expr * Origin option
        | Set of string * Expr * Origin option
        | SetTo of string * Expr * Expr * Origin option
        | Do of Command * Origin option
        | DoMany of Command list * Origin option

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
            | Phrase (_, originOpt)
            | Stream (_, originOpt)
            | Binding (_, originOpt)
            | Apply (_, originOpt)
            | Quote (_, originOpt)
            | Let (_, _, originOpt)
            | LetMany (_, originOpt)
            | Fun (_, _, _, originOpt)
            | If (_, _, _, originOpt)
            | Cond (_, originOpt)
            | Try (_, _, originOpt)
            | Break (_, originOpt)
            | Get (_, originOpt)
            | GetFrom (_, _, originOpt)
            | Set (_, _, originOpt)
            | SetTo (_, _, _, originOpt)
            | Do (_, originOpt)
            | DoMany (_, originOpt)
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
                    | "none" -> Option (None, originOpt) :> obj
                    | "empty" -> List (List.empty, originOpt) :> obj
                    | _ ->
                        let firstChar = str.[0]
                        if firstChar = '.' || Char.IsUpper firstChar
                        then Keyword (str, originOpt) :> obj
                        else Binding (str, originOpt) :> obj
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
                            | [Prime.Atom (name, _); body] -> Let (name, this.SymbolToExpr body, originOpt) :> obj
                            | _ -> Violation ([!!"InvalidLetForm"], "Invalid let form. Requires 1 name and 1 body.", originOpt) :> obj
                        | "if" ->
                            match tail with
                            | [condition; consequent; alternative] -> If (this.SymbolToExpr condition, this.SymbolToExpr consequent, this.SymbolToExpr alternative, originOpt) :> obj
                            | _ -> Violation ([!!"InvalidIfForm"], "Invalid if form. Requires 3 arguments.", originOpt) :> obj
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

    /// The execution environment for scripts.
    type [<NoEquality; NoComparison>] Env<'p, 'g, 'w when 'p :> Participant and 'g :> Participant and 'w :> EventWorld<'g, 'w>> =
        private
            { Rebinding : bool // rebinding should be enabled in Terminal or perhaps when reloading existing scripts.
              TopLevel : Dictionary<string, Scripting.Expr>
              Streams : Map<obj Address, Prime.Stream<obj, 'g, 'w> * ('w -> 'w)>
              Context : 'p // TODO: get rid of this and use EventContext instead.
              World : 'w }

    [<RequireQualifiedAccess>]
    module Env =

        let make chooseWorld rebinding topLevel (context : 'p) (world : 'w) =
            { Rebinding = rebinding
              TopLevel = topLevel
              Streams = Map.empty
              Context = context
              World = chooseWorld world }

        let tryGetBinding name (env : Env<'p, 'g, 'w>) =
            match env.TopLevel.TryGetValue name with
            | (true, binding) -> Some binding
            | (false, _) -> None

        let tryAddBinding isTopLevel name evaled (env : Env<'p, 'g, 'w>) =
            if isTopLevel && (env.Rebinding || not ^ env.TopLevel.ContainsKey name) then
                env.TopLevel.Add (name, evaled)
                Some env
            else None

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

/// The execution environment for scripts.
type Env<'p, 'g, 'w when 'p :> Participant and 'g :> Participant and 'w :> EventWorld<'g, 'w>> =
    EnvModule.Env<'p, 'g, 'w>