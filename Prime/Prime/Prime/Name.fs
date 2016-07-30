// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Prime
open System
open System.ComponentModel
open System.Reflection
open Prime

/// Converts Name types.
type NameConverter (targetType : Type) =
    inherit TypeConverter ()
    
    override this.CanConvertTo (_, destType) =
        destType = typeof<string> ||
        destType = typeof<Symbol> ||
        destType = targetType
        
    override this.ConvertTo (_, _, source, destType) =
        if destType = typeof<string> then
            let toStringMethod = targetType.GetMethod "ToString"
            toStringMethod.Invoke (source, null)
        elif destType = typeof<Symbol> then
            let toStringMethod = targetType.GetMethod "ToString"
            let nameStr = toStringMethod.Invoke (source, null) :?> string
            if Symbol.shouldBeExplicit nameStr then String (nameStr, None) :> obj
            else Atom (nameStr, None) :> obj
        elif destType = targetType then source
        else failconv "Invalid NameConverter conversion to source." None
        
    override this.CanConvertFrom (_, sourceType) =
        sourceType = typeof<string> ||
        sourceType = typeof<Symbol> ||
        sourceType = targetType
        
    override this.ConvertFrom (_, _, source) =
        match source with
        | :? string as nameStr ->
            let makeFunction = targetType.GetMethod ("make", BindingFlags.Static ||| BindingFlags.Public)
            makeFunction.Invoke (null, [|nameStr|])
        | :? Symbol as nameSymbol ->
            match nameSymbol with
            | Atom (nameStr, _) | String (nameStr, _) ->
                let makeFunction = targetType.GetMethod ("make", BindingFlags.Static ||| BindingFlags.Public)
                makeFunction.Invoke (null, [|nameStr|])
            | Number (_, _) | Quote (_, _) | Symbols (_, _) ->
                failconv "Expected Symbol or String for conversion to Name." ^ Some nameSymbol
        | _ ->
            if targetType.IsInstanceOfType source then source
            else failconv "Invalid NameConverter conversion from source." None

[<AutoOpen>]
module NameModule =

    /// A name for optimized keying in hashing containers.
    type [<CustomEquality; CustomComparison; TypeConverter (typeof<NameConverter>)>] Name =
        private
            { NameStr : string
              HashCode : int } // OPTIMIZATION: hash cached for speed

        /// Make a name from a non-empty string without whitespace.
        static member make (nameStr : string) =
#if DEBUG   
            // NOTE: we cannot do this checking at run-time because running an FParsec parser that often is too slow
            if nameStr.IndexOf '.' <> -1 then failwith ^ "Invalid name '" + nameStr + "'; must have no dot characters."
            elif nameStr.IndexOfAny Symbol.WhitespaceCharsArray <> -1 then failwith ^ "Invalid name '" + nameStr + "'; must have no whitespace characters."
            elif Symbol.isNumber nameStr then failwith ^ "Invalid name '" + nameStr + "'; cannot be a number."
            else
#endif
                { NameStr = nameStr; HashCode = nameStr.GetHashCode () }
    
        /// Equate Names.
        static member equals name name2 =
            name.HashCode = name2.HashCode && // OPTIMIZATION: first check hash equality
            strEq name.NameStr name2.NameStr
    
        /// Compare Names.
        static member compare name name2 =
            strCmp name.NameStr name2.NameStr
    
        interface Name IComparable with
            member this.CompareTo that =
                Name.compare this that
    
        interface IComparable with
            member this.CompareTo that =
                match that with
                | :? Name as that -> Name.compare this that
                | _ -> failwith "Invalid Name comparison (comparee not of type Name)."
    
        interface Name IEquatable with
            member this.Equals that =
                Name.equals this that
    
        override this.Equals that =
            match that with
            | :? Name as that -> Name.equals this that
            | _ -> false
    
        override this.GetHashCode () =
            this.HashCode
    
        override this.ToString () =
            this.NameStr

    [<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
    module Name =
    
        /// Get the name of a name key.
        let getNameStr name =
            name.NameStr
    
        /// Join a list of names by a separator string.
        let join sep names =
            let nameStrs = Seq.map getNameStr names
            let namesStr = String.Join (sep, nameStrs)
            Name.make namesStr
    
        /// Split a name on a separator char array.
        let split sep name =
            name.NameStr |>
            (fun nameStr -> nameStr.Split sep) |>
            Array.map Name.make |>
            Seq.ofArray
    
        /// Query for equality a list of names lexicographically.
        let rec equateNames (names : Name list) (names2 : Name list) =
            match (names, names2) with
            | ([], []) -> true
            | (_ :: _, []) -> false
            | ([], _ :: _) -> false
            | (head :: tail, head2 :: tail2) ->
                let result = strEq head.NameStr head2.NameStr
                if result then equateNames tail tail2
                else result
    
        /// Compare a list of names lexicographically.
        let rec compareNames (names : Name list) (names2 : Name list) =
            match (names, names2) with
            | ([], []) -> 0
            | (_ :: _, []) -> 1
            | ([], _ :: _) -> -1
            | (head :: tail, head2 :: tail2) ->
                let result = strCmp head.NameStr head2.NameStr
                if result = 0 then compareNames tail tail2
                else result
    
        /// Hash a list of names.
        let hashNames (names : Name list) =
            let mutable hashValue = 0 // OPTIMIZATION: mutation for speed
            for name in names do hashValue <- hashValue ^^^ name.GetHashCode ()
            hashValue
    
        /// The empty name, consisting of an empty string.
        let empty =
            Name.make String.Empty

[<AutoOpen>]
module NameOperators =

    /// Convert a name string to a name.
    let inline (!!) nameStr =
        Name.make nameStr

/// A name for optimized keying in hashing containers.
type Name = NameModule.Name