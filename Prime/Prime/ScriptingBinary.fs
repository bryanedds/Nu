// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2018.

namespace Prime
open System
open Prime
open Prime.Scripting
module ScriptingBinary =

    type [<Struct; NoEquality; NoComparison>] BinaryFns =
        { Bool : bool -> bool -> SymbolOrigin option -> Expr
          Int : int -> int -> SymbolOrigin option -> Expr
          Int64 : int64 -> int64 -> SymbolOrigin option -> Expr
          Single : single -> single -> SymbolOrigin option -> Expr
          Double : double -> double -> SymbolOrigin option -> Expr
          String : string -> string -> SymbolOrigin option -> Expr
          Keyword : string -> string -> SymbolOrigin option -> Expr
          Tuple : Expr array -> Expr array -> SymbolOrigin option -> Expr
          Union : string -> Expr array -> string -> Expr array -> SymbolOrigin option -> Expr
          Codata : Codata -> Codata -> SymbolOrigin option -> Expr
          List : Expr list -> Expr list -> SymbolOrigin option -> Expr
          Ring : Expr Set -> Expr Set -> SymbolOrigin option -> Expr
          Table : Map<Expr, Expr> -> Map<Expr, Expr> -> SymbolOrigin option -> Expr
          Record : string -> Map<string, int> -> Expr array -> string -> Map<string, int> -> Expr array -> SymbolOrigin option -> Expr }

    let EqFns =
        { Bool = fun left right _ -> Bool (left = right)
          Int = fun left right _ -> Bool (left = right)
          Int64 = fun left right _ -> Bool (left = right)
          Single = fun left right _ -> Bool (left = right)
          Double = fun left right _ -> Bool (left = right)
          String = fun left right _ -> Bool (left = right)
          Keyword = fun left right _ -> Bool (left = right)
          Tuple = fun left right _ -> Bool (left = right)
          Union = fun keywordLeft fieldsLeft keywordRight fieldsRight _ -> Bool ((keywordLeft, fieldsLeft) = (keywordRight, fieldsRight))
          Codata = fun _ _ originOpt -> Violation (["NotImplemented"; "Eq"], "Equality not implemented for Codata.", originOpt)
          List = fun left right _ -> Bool (left = right)
          Ring = fun left right _ -> Bool (left = right)
          Table = fun left right _ -> Bool (left = right)
          Record = fun keywordLeft mapLeft fieldsLeft keywordRight mapRight fieldsRight _ -> Bool ((keywordLeft, mapLeft, fieldsLeft) = (keywordRight, mapRight, fieldsRight)) }

    let NotEqFns =
        { Bool = fun left right _ -> Bool (left <> right)
          Int = fun left right _ -> Bool (left <> right)
          Int64 = fun left right _ -> Bool (left <> right)
          Single = fun left right _ -> Bool (left <> right)
          Double = fun left right _ -> Bool (left <> right)
          String = fun left right _ -> Bool (left <> right)
          Keyword = fun left right _ -> Bool (left <> right)
          Tuple = fun left right _ -> Bool (left <> right)
          Union = fun keywordLeft fieldsLeft keywordRight fieldsRight _ -> Bool ((keywordLeft, fieldsLeft) <> (keywordRight, fieldsRight))
          Codata = fun _ _ originOpt -> Violation (["NotImplemented"; "NotEq"], "Equality not implemented for Codata.", originOpt)
          List = fun left right _ -> Bool (left <> right)
          Ring = fun left right _ -> Bool (left <> right)
          Table = fun left right _ -> Bool (left <> right)
          Record = fun keywordLeft mapLeft fieldsLeft keywordRight mapRight fieldsRight _ -> Bool ((keywordLeft, mapLeft, fieldsLeft) <> (keywordRight, mapRight, fieldsRight)) }

    let LtFns =
        { Bool = fun left right _ -> Bool (left < right)
          Int = fun left right _ -> Bool (left < right)
          Int64 = fun left right _ -> Bool (left < right)
          Single = fun left right _ -> Bool (left < right)
          Double = fun left right _ -> Bool (left < right)
          String = fun left right _ -> Bool (left < right)
          Keyword = fun left right _ -> Bool (left < right)
          Tuple = fun left right _ -> Bool (left < right)
          Union = fun keywordLeft fieldsLeft keywordRight fieldsRight _ -> Bool ((keywordLeft, fieldsLeft) < (keywordRight, fieldsRight))
          Codata = fun _ _ originOpt -> Violation (["NotImplemented"; "Lt"], "Comparison not implemented for Codata.", originOpt)
          List = fun left right _ -> Bool (left < right)
          Ring = fun left right _ -> Bool (left < right)
          Table = fun left right _ -> Bool (left < right)
          Record = fun keywordLeft mapLeft fieldsLeft keywordRight mapRight fieldsRight _ -> Bool ((keywordLeft, mapLeft, fieldsLeft) < (keywordRight, mapRight, fieldsRight)) }

    let GtFns =
        { Bool = fun left right _ -> Bool (left > right)
          Int = fun left right _ -> Bool (left > right)
          Int64 = fun left right _ -> Bool (left > right)
          Single = fun left right _ -> Bool (left > right)
          Double = fun left right _ -> Bool (left > right)
          String = fun left right _ -> Bool (left > right)
          Keyword = fun left right _ -> Bool (left > right)
          Tuple = fun left right _ -> Bool (left > right)
          Union = fun keywordLeft fieldsLeft keywordRight fieldsRight _ -> Bool ((keywordLeft, fieldsLeft) > (keywordRight, fieldsRight))
          Codata = fun _ _ originOpt -> Violation (["NotImplemented"; "Gt"], "Comparison not implemented for Codata.", originOpt)
          List = fun left right _ -> Bool (left > right)
          Ring = fun left right _ -> Bool (left > right)
          Table = fun left right _ -> Bool (left > right)
          Record = fun keywordLeft mapLeft fieldsLeft keywordRight mapRight fieldsRight _ -> Bool ((keywordLeft, mapLeft, fieldsLeft) > (keywordRight, mapRight, fieldsRight)) }

    let LtEqFns =
        { Bool = fun left right _ -> Bool (left <= right)
          Int = fun left right _ -> Bool (left <= right)
          Int64 = fun left right _ -> Bool (left <= right)
          Single = fun left right _ -> Bool (left <= right)
          Double = fun left right _ -> Bool (left <= right)
          String = fun left right _ -> Bool (left <= right)
          Keyword = fun left right _ -> Bool (left <= right)
          Tuple = fun left right _ -> Bool (left <= right)
          Union = fun keywordLeft fieldsLeft keywordRight fieldsRight _ -> Bool ((keywordLeft, fieldsLeft) <= (keywordRight, fieldsRight))
          Codata = fun _ _ originOpt -> Violation (["NotImplemented"; "LtEq"], "Comparison not implemented for Codata.", originOpt)
          List = fun left right _ -> Bool (left <= right)
          Ring = fun left right _ -> Bool (left <= right)
          Table = fun left right _ -> Bool (left <= right)
          Record = fun keywordLeft mapLeft fieldsLeft keywordRight mapRight fieldsRight _ -> Bool ((keywordLeft, mapLeft, fieldsLeft) <= (keywordRight, mapRight, fieldsRight)) }

    let GtEqFns =
        { Bool = fun left right _ -> Bool (left >= right)
          Int = fun left right _ -> Bool (left >= right)
          Int64 = fun left right _ -> Bool (left >= right)
          Single = fun left right _ -> Bool (left >= right)
          Double = fun left right _ -> Bool (left >= right)
          String = fun left right _ -> Bool (left >= right)
          Keyword = fun left right _ -> Bool (left >= right)
          Tuple = fun left right _ -> Bool (left >= right)
          Union = fun keywordLeft fieldsLeft keywordRight fieldsRight _ -> Bool ((keywordLeft, fieldsLeft) >= (keywordRight, fieldsRight))
          Codata = fun _ _ originOpt -> Violation (["NotImplemented"; "GtEq"], "Comparison not implemented for Codata.", originOpt)
          List = fun left right _ -> Bool (left >= right)
          Ring = fun left right _ -> Bool (left >= right)
          Table = fun left right _ -> Bool (left >= right)
          Record = fun keywordLeft mapLeft fieldsLeft keywordRight mapRight fieldsRight _ -> Bool ((keywordLeft, mapLeft, fieldsLeft) >= (keywordRight, mapRight, fieldsRight)) }

    let AddFns =
        { Bool = fun left right _ -> Bool (if left && right then false elif left then true elif right then true else false)
          Int = fun left right _ -> Int (left + right)
          Int64 = fun left right _ -> Int64 (left + right)
          Single = fun left right _ -> Single (left + right)
          Double = fun left right _ -> Double (left + right)
          String = fun left right _ -> String (left + right)
          Keyword = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Add"], "Cannot add Keywords.", originOpt)
          Tuple = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Add"], "Cannot add Tuples.", originOpt)
          Union = fun _ _ _ _ originOpt -> Violation (["InvalidArgumentType"; "Add"], "Cannot add Unions.", originOpt)
          Codata = fun left right _ -> Codata (Add (left, right))
          List = fun left right _ -> List (left @ right)
          Ring = fun left right _ -> Ring (Set.union left right)
          Table = fun left right _ -> Table (left @@ right)
          Record = fun _ _ _ _ _ _ originOpt -> Violation (["InvalidArgumentType"; "Add"], "Cannot add Records.", originOpt) }

    let SubFns =
        { Bool = fun left right _ -> Bool (if left && right then false elif left then true elif right then true else false)
          Int = fun left right _ -> Int (left - right)
          Int64 = fun left right _ -> Int64 (left - right)
          Single = fun left right _ -> Single (left - right)
          Double = fun left right _ -> Double (left - right)
          String = fun left right _ -> String (left.Replace (right, String.Empty))
          Keyword = fun left right _ -> String (left.Replace (right, String.Empty))
          Tuple = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Sub"], "Cannot subtract Tuples.", originOpt)
          Union = fun _ _ _ _ originOpt -> Violation (["InvalidArgumentType"; "Sub"], "Cannot subtract Unions.", originOpt)
          Codata = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Sub"], "Cannot subtract Codata.", originOpt)
          List = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Sub"], "Cannot subtract Lists.", originOpt)
          Ring = fun left right _ -> Ring (Set.difference left right)
          Table = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Sub"], "Cannot subtract Tables.", originOpt)
          Record = fun _ _ _ _ _ _ originOpt -> Violation (["InvalidArgumentType"; "Sub"], "Cannot subtract Records.", originOpt) }

    let MulFns =
        { Bool = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Mul"], "Cannot multiply Bools.", originOpt)
          Int = fun left right _ -> Int (left * right)
          Int64 = fun left right _ -> Int64 (left * right)
          Single = fun left right _ -> Single (left * right)
          Double = fun left right _ -> Double (left * right)
          String = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Mul"], "Cannot multiply Strings.", originOpt)
          Keyword = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Mul"], "Cannot multiply Keyword.", originOpt)
          Tuple = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Mul"], "Cannot multiply Tuples.", originOpt)
          Union = fun _ _ _ _ originOpt -> Violation (["InvalidArgumentType"; "Mul"], "Cannot multiply Unions.", originOpt)
          Codata = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Mul"], "Cannot multiply Codata.", originOpt)
          List = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Mul"], "Cannot multiply Lists.", originOpt)
          Ring = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Mul"], "Cannot multiply Rings.", originOpt)
          Table = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Mul"], "Cannot multiply Tables.", originOpt)
          Record = fun _ _ _ _ _ _ originOpt -> Violation (["InvalidArgumentType"; "Mul"], "Cannot multiply Records.", originOpt) }

    let DivFns =
        { Bool = fun left right originOpt -> if right = false then Violation (["OutOfRangeArgument"; "Div"], "Cannot divide by a false Bool.", originOpt) else Bool (if left && right then true else false)
          Int = fun left right originOpt -> if right = 0 then Violation (["OutOfRangeArgument"; "Div"], "Cannot divide by a zero Int.", originOpt) else Int (left / right)
          Int64 = fun left right originOpt -> if right = 0L then Violation (["OutOfRangeArgument"; "Div"], "Cannot divide by a zero Int64.", originOpt) else Int64 (left / right)
          Single = fun left right _ -> Single (left / right)
          Double = fun left right _ -> Double (left / right)
          String = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Div"], "Cannot divide Strings.", originOpt)
          Keyword = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Div"], "Cannot divide Keywords.", originOpt)
          Tuple = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Div"], "Cannot divide Tuples.", originOpt)
          Union = fun _ _ _ _ originOpt -> Violation (["InvalidArgumentType"; "Div"], "Cannot divide Unions.", originOpt)
          Codata = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Div"], "Cannot divide Codata.", originOpt)
          List = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Div"], "Cannot divide Lists.", originOpt)
          Ring = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Div"], "Cannot divide Rings.", originOpt)
          Table = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Div"], "Cannot divide Tables.", originOpt)
          Record = fun _ _ _ _ _ _ originOpt -> Violation (["InvalidArgumentType"; "Div"], "Cannot divide Records.", originOpt) }

    let ModFns =
        { Bool = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Mod"], "Cannot modulate Bools.", originOpt)
          Int = fun left right originOpt -> if right = 0 then Violation (["OutOfRangeArgument"; "Mod"], "Cannot modulate by a zero Int.", originOpt) else Int (left % right)
          Int64 = fun left right originOpt -> if right = 0L then Violation (["OutOfRangeArgument"; "Mod"], "Cannot divide by a zero Int64.", originOpt) else Int64 (left % right)
          Single = fun left right _ -> Single (left % right)
          Double = fun left right _ -> Double (left % right)
          String = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Mod"], "Cannot modulate Strings.", originOpt)
          Keyword = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Mod"], "Cannot modulate Keywords.", originOpt)
          Tuple = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Mod"], "Cannot modulate Tuples.", originOpt)
          Union = fun _ _ _ _ originOpt -> Violation (["InvalidArgumentType"; "Mod"], "Cannot modulate Unions.", originOpt)
          Codata = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Mod"], "Cannot modulate Codata.", originOpt)
          List = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Mod"], "Cannot modulate Lists.", originOpt)
          Ring = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Mod"], "Cannot modulate Rings.", originOpt)
          Table = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Mod"], "Cannot modulate Tables.", originOpt)
          Record = fun _ _ _ _ _ _ originOpt -> Violation (["InvalidArgumentType"; "Mod"], "Cannot modulate Records.", originOpt) }

    let PowFns =
        { Bool = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Pow"], "Cannot power Bools.", originOpt)
          Int = fun left right _ -> Int (int ^ Math.Pow (double left, double right))
          Int64 = fun left right _ -> Int64 (int64 ^ Math.Pow (double left, double right))
          Single = fun left right _ -> Single (single ^ Math.Pow (double left, double right))
          Double = fun left right _ -> Double (Math.Pow (double left, double right))
          String = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Pow"], "Cannot power Strings.", originOpt)
          Keyword = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Pow"], "Cannot power Keywords.", originOpt)
          Tuple = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Pow"], "Cannot power Tuples.", originOpt)
          Union = fun _ _ _ _ originOpt -> Violation (["InvalidArgumentType"; "Pow"], "Cannot power Unions.", originOpt)
          Codata = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Pow"], "Cannot power Codata.", originOpt)
          List = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Pow"], "Cannot power Lists.", originOpt)
          Ring = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Pow"], "Cannot power Rings.", originOpt)
          Table = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Pow"], "Cannot power Tables.", originOpt)
          Record = fun _ _ _ _ _ _ originOpt -> Violation (["InvalidArgumentType"; "Pow"], "Cannot power Records.", originOpt) }

    let RootFns =
        { Bool = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Root"], "Cannot root Bools.", originOpt)
          Int = fun left right _ -> Int (int ^ Math.Pow (double left, 1.0 / double right))
          Int64 = fun left right _ -> Int64 (int64 ^ Math.Pow (double left, 1.0 / double right))
          Single = fun left right _ -> Single (single ^ Math.Pow (double left, 1.0 / double right))
          Double = fun left right _ -> Double (Math.Pow (double left, 1.0 / double right))
          String = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Root"], "Cannot root Strings.", originOpt)
          Keyword = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Root"], "Cannot root Keywords.", originOpt)
          Tuple = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Root"], "Cannot root Tuples.", originOpt)
          Union = fun _ _ _ _ originOpt -> Violation (["InvalidArgumentType"; "Root"], "Cannot root Unions.", originOpt)
          Codata = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Root"], "Cannot root Codata.", originOpt)
          List = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Root"], "Cannot root Lists.", originOpt)
          Ring = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Root"], "Cannot root Rings.", originOpt)
          Table = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Root"], "Cannot root Tables.", originOpt)
          Record = fun _ _ _ _ _ _ originOpt -> Violation (["InvalidArgumentType"; "Root"], "Cannot root Records.", originOpt) }

    let CrossFns =
        { Bool = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Cross"], "Cannot cross multiply Bools.", originOpt)
          Int = fun left right _ -> Int (left * right)
          Int64 = fun left right _ -> Int64 (left * right)
          Single = fun left right _ -> Single (left * right)
          Double = fun left right _ -> Double (left * right)
          String = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Cross"], "Cannot cross multiply Strings.", originOpt)
          Keyword = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Cross"], "Cannot cross multiply Keywords.", originOpt)
          Tuple = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Cross"], "Cannot cross multiply Tuples.", originOpt)
          Union = fun _ _ _ _ originOpt -> Violation (["InvalidArgumentType"; "Cross"], "Cannot cross multiply Unions.", originOpt)
          Codata = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Cross"], "Cannot cross multiply Codata.", originOpt)
          List = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Cross"], "Cannot cross multiply Lists.", originOpt)
          Ring = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Cross"], "Cannot cross multiply Rings.", originOpt)
          Table = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Cross"], "Cannot cross multiply Tables.", originOpt)
          Record = fun _ _ _ _ _ _ originOpt -> Violation (["InvalidArgumentType"; "Cross"], "Cannot cross multiply Records.", originOpt) }

    let DotFns =
        { Bool = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Dot"], "Cannot dot multiply Bools.", originOpt)
          Int = fun left right _ -> Int (left * right)
          Int64 = fun left right _ -> Int64 (left * right)
          Single = fun left right _ -> Single (left * right)
          Double = fun left right _ -> Double (left * right)
          String = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Dot"], "Cannot dot multiply Strings.", originOpt)
          Keyword = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Dot"], "Cannot dot multiply Keywords.", originOpt)
          Tuple = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Dot"], "Cannot dot multiply Tuples.", originOpt)
          Union = fun _ _ _ _ originOpt -> Violation (["InvalidArgumentType"; "Dot"], "Cannot dot multiply Unions.", originOpt)
          Codata = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Dot"], "Cannot dot multiply Codata.", originOpt)
          List = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Dot"], "Cannot dot multiply Lists.", originOpt)
          Ring = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Dot"], "Cannot dot multiply Rings.", originOpt)
          Table = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Dot"], "Cannot dot multiply Tables.", originOpt)
          Record = fun _ _ _ _ _ _ originOpt -> Violation (["InvalidArgumentType"; "Dot"], "Cannot dot multiply Records.", originOpt) }

    let evalBinaryInner (fns : BinaryFns) fnName evaledLeft evaledRight originOpt (world : 'w) =
        match (evaledLeft, evaledRight) with
        | (Bool boolLeft, Bool boolRight) -> struct (fns.Bool boolLeft boolRight originOpt, world)
        | (Int intLeft, Int intRight) -> struct (fns.Int intLeft intRight originOpt, world)
        | (Int64 int64Left, Int64 int64Right) -> struct (fns.Int64 int64Left int64Right originOpt, world)
        | (Single singleLeft, Single singleRight) -> struct (fns.Single singleLeft singleRight originOpt, world)
        | (Double doubleLeft, Double doubleRight) -> struct (fns.Double doubleLeft doubleRight originOpt, world)
        | (String stringLeft, String stringRight) -> struct (fns.String stringLeft stringRight originOpt, world)
        | (Keyword keywordLeft, Keyword keywordRight) -> struct (fns.String keywordLeft keywordRight originOpt, world)
        | (Tuple tupleLeft, Tuple tupleRight) -> struct (fns.Tuple tupleLeft tupleRight originOpt, world)
        | (Union (nameLeft, fieldsLeft), Union (nameRight, fieldsRight)) -> struct (fns.Union nameLeft fieldsLeft nameRight fieldsRight originOpt, world)
        | (Codata codataLeft, Codata codataRight) -> struct (fns.Codata codataLeft codataRight originOpt, world)
        | (List listLeft, List listRight) -> struct (fns.List listLeft listRight originOpt, world)
        | (Ring ringLeft, Ring ringRight) -> struct (fns.Ring ringLeft ringRight originOpt, world)
        | (Table tableLeft, Table tableRight) -> struct (fns.Table tableLeft tableRight originOpt, world)
        | (Violation _ as violation, _) -> struct (violation, world)
        | (_, (Violation _ as violation)) -> struct (violation, world)
        | _ -> struct (Violation (["InvalidArgumentType"; (String.capitalize fnName)], "Cannot apply a binary function on unlike or incompatible values.", originOpt), world)

    let evalBinary fns fnName argsEvaled originOpt (world : 'w) =
        match argsEvaled with
        | [|evaledLeft; evaledRight|] -> evalBinaryInner fns fnName evaledLeft evaledRight originOpt world
        | _ -> struct (Violation (["InvalidArgumentCount"; (String.capitalize fnName)], "Incorrect number of arguments for application of '" + fnName + "'; 2 arguments required.", originOpt), world)