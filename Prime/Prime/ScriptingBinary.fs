// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2017.

namespace Prime
open System
open Prime
open Prime.Scripting
module ScriptingBinary =

    type [<NoEquality; NoComparison>] BinaryFns =
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
          Codata = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Eq"], "Cannot determine equality of codata.", originOpt)
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
          Codata = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "NotEq"], "Cannot determine inequality of codata.", originOpt)
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
          Codata = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Lt"], "Cannot compare codata.", originOpt)
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
          Codata = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Gt"], "Cannot compare codata.", originOpt)
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
          Codata = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "LtEq"], "Cannot compare codata.", originOpt)
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
          Codata = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "GtEq"], "Cannot compare codata.", originOpt)
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
          Keyword = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Add"], "Cannot add keywords.", originOpt)
          Tuple = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Add"], "Cannot add tuples.", originOpt)
          Union = fun _ _ _ _ originOpt -> Violation (["InvalidArgumentType"; "Add"], "Cannot add unions.", originOpt)
          Codata = fun left right _ -> Codata (Add (left, right))
          List = fun left right _ -> List (left @ right)
          Ring = fun left right _ -> Ring (Set.union left right)
          Table = fun left right _ -> Table (left @@ right)
          Record = fun _ _ _ _ _ _ originOpt -> Violation (["InvalidArgumentType"; "Add"], "Cannot add records.", originOpt) }

    let SubFns =
        { Bool = fun left right _ -> Bool (if left && right then false elif left then true elif right then true else false)
          Int = fun left right _ -> Int (left - right)
          Int64 = fun left right _ -> Int64 (left - right)
          Single = fun left right _ -> Single (left - right)
          Double = fun left right _ -> Double (left - right)
          String = fun left right _ -> String (left.Replace (right, String.Empty))
          Keyword = fun left right _ -> String (left.Replace (right, String.Empty))
          Tuple = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Sub"], "Cannot subtract tuples.", originOpt)
          Union = fun _ _ _ _ originOpt -> Violation (["InvalidArgumentType"; "Sub"], "Cannot subtract unions.", originOpt)
          Codata = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Sub"], "Cannot subtract codata.", originOpt)
          List = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Sub"], "Cannot subtract lists.", originOpt)
          Ring = fun left right _ -> Ring (Set.difference left right)
          Table = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Sub"], "Cannot subtract tables.", originOpt)
          Record = fun _ _ _ _ _ _ originOpt -> Violation (["InvalidArgumentType"; "Sub"], "Cannot subtract records.", originOpt) }

    let MulFns =
        { Bool = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Mul"], "Cannot multiply bools.", originOpt)
          Int = fun left right _ -> Int (left * right)
          Int64 = fun left right _ -> Int64 (left * right)
          Single = fun left right _ -> Single (left * right)
          Double = fun left right _ -> Double (left * right)
          String = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Mul"], "Cannot multiply strings.", originOpt)
          Keyword = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Mul"], "Cannot multiply keyword.", originOpt)
          Tuple = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Mul"], "Cannot multiply tuples.", originOpt)
          Union = fun _ _ _ _ originOpt -> Violation (["InvalidArgumentType"; "Mul"], "Cannot multiply unions.", originOpt)
          Codata = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Mul"], "Cannot multiply codata.", originOpt)
          List = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Mul"], "Cannot multiply lists.", originOpt)
          Ring = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Mul"], "Cannot multiply rings.", originOpt)
          Table = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Mul"], "Cannot multiply tables.", originOpt)
          Record = fun _ _ _ _ _ _ originOpt -> Violation (["InvalidArgumentType"; "Mul"], "Cannot multiply records.", originOpt) }

    let DivFns =
        { Bool = fun left right originOpt -> if right = false then Violation (["OutOfRangeArgument"; "Div"], "Cannot divide by a false bool.", originOpt) else Bool (if left && right then true else false)
          Int = fun left right originOpt -> if right = 0 then Violation (["OutOfRangeArgument"; "Div"], "Cannot divide by a zero int.", originOpt) else Int (left / right)
          Int64 = fun left right originOpt -> if right = 0L then Violation (["OutOfRangeArgument"; "Div"], "Cannot divide by a zero 64-bit int.", originOpt) else Int64 (left / right)
          Single = fun left right _ -> Single (left / right)
          Double = fun left right _ -> Double (left / right)
          String = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Div"], "Cannot divide strings.", originOpt)
          Keyword = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Div"], "Cannot divide keywords.", originOpt)
          Tuple = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Div"], "Cannot divide tuples.", originOpt)
          Union = fun _ _ _ _ originOpt -> Violation (["InvalidArgumentType"; "Div"], "Cannot divide unions.", originOpt)
          Codata = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Div"], "Cannot divide codata.", originOpt)
          List = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Div"], "Cannot divide lists.", originOpt)
          Ring = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Div"], "Cannot divide rings.", originOpt)
          Table = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Div"], "Cannot divide tables.", originOpt)
          Record = fun _ _ _ _ _ _ originOpt -> Violation (["InvalidArgumentType"; "Div"], "Cannot divide records.", originOpt) }

    let ModFns =
        { Bool = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Mod"], "Cannot modulate bools.", originOpt)
          Int = fun left right originOpt -> if right = 0 then Violation (["OutOfRangeArgument"; "Mod"], "Cannot modulate by a zero int.", originOpt) else Int (left % right)
          Int64 = fun left right originOpt -> if right = 0L then Violation (["OutOfRangeArgument"; "Mod"], "Cannot divide by a zero 64-bit int.", originOpt) else Int64 (left % right)
          Single = fun left right _ -> Single (left % right)
          Double = fun left right _ -> Double (left % right)
          String = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Mod"], "Cannot modulate strings.", originOpt)
          Keyword = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Mod"], "Cannot modulate keywords.", originOpt)
          Tuple = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Mod"], "Cannot modulate tuples.", originOpt)
          Union = fun _ _ _ _ originOpt -> Violation (["InvalidArgumentType"; "Mod"], "Cannot modulate unions.", originOpt)
          Codata = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Mod"], "Cannot modulate codata.", originOpt)
          List = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Mod"], "Cannot modulate lists.", originOpt)
          Ring = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Mod"], "Cannot modulate rings.", originOpt)
          Table = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Mod"], "Cannot modulate tables.", originOpt)
          Record = fun _ _ _ _ _ _ originOpt -> Violation (["InvalidArgumentType"; "Mod"], "Cannot modulate records.", originOpt) }

    let PowFns =
        { Bool = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Pow"], "Cannot power bools.", originOpt)
          Int = fun left right _ -> Int (int ^ Math.Pow (double left, double right))
          Int64 = fun left right _ -> Int64 (int64 ^ Math.Pow (double left, double right))
          Single = fun left right _ -> Single (single ^ Math.Pow (double left, double right))
          Double = fun left right _ -> Double (Math.Pow (double left, double right))
          String = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Pow"], "Cannot power strings.", originOpt)
          Keyword = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Pow"], "Cannot power keywords.", originOpt)
          Tuple = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Pow"], "Cannot power tuples.", originOpt)
          Union = fun _ _ _ _ originOpt -> Violation (["InvalidArgumentType"; "Pow"], "Cannot power unions.", originOpt)
          Codata = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Pow"], "Cannot power codata.", originOpt)
          List = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Pow"], "Cannot power lists.", originOpt)
          Ring = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Pow"], "Cannot power rings.", originOpt)
          Table = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Pow"], "Cannot power tables.", originOpt)
          Record = fun _ _ _ _ _ _ originOpt -> Violation (["InvalidArgumentType"; "Pow"], "Cannot power records.", originOpt) }

    let RootFns =
        { Bool = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Root"], "Cannot root bools.", originOpt)
          Int = fun left right _ -> Int (int ^ Math.Pow (double left, 1.0 / double right))
          Int64 = fun left right _ -> Int64 (int64 ^ Math.Pow (double left, 1.0 / double right))
          Single = fun left right _ -> Single (single ^ Math.Pow (double left, 1.0 / double right))
          Double = fun left right _ -> Double (Math.Pow (double left, 1.0 / double right))
          String = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Root"], "Cannot root strings.", originOpt)
          Keyword = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Root"], "Cannot root keywords.", originOpt)
          Tuple = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Root"], "Cannot root tuples.", originOpt)
          Union = fun _ _ _ _ originOpt -> Violation (["InvalidArgumentType"; "Root"], "Cannot root unions.", originOpt)
          Codata = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Root"], "Cannot root codata.", originOpt)
          List = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Root"], "Cannot root lists.", originOpt)
          Ring = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Root"], "Cannot root rings.", originOpt)
          Table = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Root"], "Cannot root tables.", originOpt)
          Record = fun _ _ _ _ _ _ originOpt -> Violation (["InvalidArgumentType"; "Root"], "Cannot root records.", originOpt) }

    let CrossFns =
        { Bool = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Cross"], "Cannot cross multiply bools.", originOpt)
          Int = fun left right _ -> Int (left * right)
          Int64 = fun left right _ -> Int64 (left * right)
          Single = fun left right _ -> Single (left * right)
          Double = fun left right _ -> Double (left * right)
          String = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Cross"], "Cannot cross multiply strings.", originOpt)
          Keyword = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Cross"], "Cannot cross multiply keywords.", originOpt)
          Tuple = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Cross"], "Cannot cross multiply tuples.", originOpt)
          Union = fun _ _ _ _ originOpt -> Violation (["InvalidArgumentType"; "Cross"], "Cannot cross multiply unions.", originOpt)
          Codata = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Cross"], "Cannot cross multiply codata.", originOpt)
          List = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Cross"], "Cannot cross multiply lists.", originOpt)
          Ring = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Cross"], "Cannot cross multiply rings.", originOpt)
          Table = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Cross"], "Cannot cross multiply tables.", originOpt)
          Record = fun _ _ _ _ _ _ originOpt -> Violation (["InvalidArgumentType"; "Cross"], "Cannot cross multiply records.", originOpt) }

    let DotFns =
        { Bool = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Dot"], "Cannot dot multiply bools.", originOpt)
          Int = fun left right _ -> Int (left * right)
          Int64 = fun left right _ -> Int64 (left * right)
          Single = fun left right _ -> Single (left * right)
          Double = fun left right _ -> Double (left * right)
          String = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Dot"], "Cannot dot multiply strings.", originOpt)
          Keyword = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Dot"], "Cannot dot multiply keywords.", originOpt)
          Tuple = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Dot"], "Cannot dot multiply tuples.", originOpt)
          Union = fun _ _ _ _ originOpt -> Violation (["InvalidArgumentType"; "Dot"], "Cannot dot multiply unions.", originOpt)
          Codata = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Dot"], "Cannot dot multiply codata.", originOpt)
          List = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Dot"], "Cannot dot multiply lists.", originOpt)
          Ring = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Dot"], "Cannot dot multiply rings.", originOpt)
          Table = fun _ _ originOpt -> Violation (["InvalidArgumentType"; "Dot"], "Cannot dot multiply tables.", originOpt)
          Record = fun _ _ _ _ _ _ originOpt -> Violation (["InvalidArgumentType"; "Dot"], "Cannot dot multiply records.", originOpt) }

    let evalBinaryInner (fns : BinaryFns) fnName originOpt evaledLeft evaledRight (world : 'w) =
        match (evaledLeft, evaledRight) with
        | (Bool boolLeft, Bool boolRight) -> (fns.Bool boolLeft boolRight originOpt, world)
        | (Int intLeft, Int intRight) -> (fns.Int intLeft intRight originOpt, world)
        | (Int64 int64Left, Int64 int64Right) -> (fns.Int64 int64Left int64Right originOpt, world)
        | (Single singleLeft, Single singleRight) -> (fns.Single singleLeft singleRight originOpt, world)
        | (Double doubleLeft, Double doubleRight) -> (fns.Double doubleLeft doubleRight originOpt, world)
        | (String stringLeft, String stringRight) -> (fns.String stringLeft stringRight originOpt, world)
        | (Keyword keywordLeft, Keyword keywordRight) -> (fns.String keywordLeft keywordRight originOpt, world)
        | (Tuple tupleLeft, Tuple tupleRight) -> (fns.Tuple tupleLeft tupleRight originOpt, world)
        | (Union (nameLeft, fieldsLeft), Union (nameRight, fieldsRight)) -> (fns.Union nameLeft fieldsLeft nameRight fieldsRight originOpt, world)
        | (Codata codataLeft, Codata codataRight) -> (fns.Codata codataLeft codataRight originOpt, world)
        | (List listLeft, List listRight) -> (fns.List listLeft listRight originOpt, world)
        | (Ring ringLeft, Ring ringRight) -> (fns.Ring ringLeft ringRight originOpt, world)
        | (Table tableLeft, Table tableRight) -> (fns.Table tableLeft tableRight originOpt, world)
        | (Violation _ as violation, _) -> (violation, world)
        | (_, (Violation _ as violation)) -> (violation, world)
        | _ -> (Violation (["InvalidArgumentType"; (String.capitalize fnName)], "Cannot apply a binary function on unlike or incompatible values.", originOpt), world)

    let evalBinary fns fnName originOpt evaledArgs (world : 'w) =
        match evaledArgs with
        | [|evaledLeft; evaledRight|] -> evalBinaryInner fns fnName originOpt evaledLeft evaledRight world                
        | _ -> (Violation (["InvalidArgumentCount"; (String.capitalize fnName)], "Incorrect number of arguments for application of '" + fnName + "'; 2 arguments required.", originOpt), world)