// Nu Game Engine.
// Copyright (C) Bryan Edds.

namespace Nu.Constants
open System
open System.Text.RegularExpressions
open Prime
open Nu

[<RequireQualifiedAccess>]
module Address =

    let [<Literal>] EmptyStr = "[]"
    let [<Literal>] SeparatorName = "/"
    let [<Literal>] WildcardName = "*"
    let [<Literal>] EllipsisName = "..."
    let [<Literal>] CurrentName = "~"
    let [<Literal>] ParentName = "^"
    let [<Uniform>] InvalidAddressName = Regex ("\[\]|\/", RegexOptions.Compiled)
    let [<Uniform>] InvalidIdentifierName = Regex ("\[\]|\/|\*|\.\.\.|\^|\~", RegexOptions.Compiled)

namespace Nu
open System
open System.ComponentModel
open System.Reflection
open Prime

/// Converts Address types.
type AddressConverter (pointType : Type) =
    inherit TypeConverter ()

    override this.CanConvertTo (_, destType) =
        destType = typeof<string> ||
        destType = typeof<Symbol> ||
        destType = pointType

    override this.ConvertTo (_, _, source, destType) =
        if destType = typeof<string> then
            let toStringMethod = pointType.GetMethod "ToString"
            toStringMethod.Invoke (source, null)
        elif destType = typeof<Symbol> then
            let toStringMethod = pointType.GetMethod "ToString"
            let addressStr = toStringMethod.Invoke (source, null) :?> string
            if Symbol.shouldBeExplicit addressStr then Text (addressStr, ValueNone) :> obj
            else Atom (addressStr, ValueNone) :> obj
        elif destType = pointType then
            source
        else failconv "Invalid AddressConverter conversion to source." None

    override this.CanConvertFrom (_, sourceType) =
        sourceType = typeof<string> ||
        sourceType = typeof<Symbol> ||
        sourceType = pointType

    override this.ConvertFrom (_, _, source) =
        match source with
        | :? string as addressStr ->
            let makeFromStringFunction = pointType.GetMethod ("makeFromString", BindingFlags.Static ||| BindingFlags.Public)
            let makeFromStringFunctionGeneric = makeFromStringFunction.MakeGenericMethod ((pointType.GetGenericArguments ()).[0])
            makeFromStringFunctionGeneric.Invoke (null, [|addressStr|])
        | :? Symbol as addressSymbol ->
            match addressSymbol with
            | Atom (addressStr, _) | Text (addressStr, _) ->
                let makeFromStringFunction = pointType.GetMethod ("makeFromString", BindingFlags.Static ||| BindingFlags.Public)
                let makeFromStringFunctionGeneric = makeFromStringFunction.MakeGenericMethod ((pointType.GetGenericArguments ()).[0])
                makeFromStringFunctionGeneric.Invoke (null, [|addressStr|])
            | Symbols (symbols, _) when symbols.Length = 0 ->
                let makeFromStringFunction = pointType.GetMethod ("makeEmpty", BindingFlags.Static ||| BindingFlags.Public)
                let makeFromStringFunctionGeneric = makeFromStringFunction.MakeGenericMethod ((pointType.GetGenericArguments ()).[0])
                makeFromStringFunctionGeneric.Invoke (null, [||])
            | Number (_, _) | Quote (_, _) | Symbols (_, _) ->
                failconv "Expected Atom, Text, Symbols (empty) for conversion to Address." (Some addressSymbol)
        | _ ->
            if pointType.IsInstanceOfType source then source
            else failconv "Invalid AddressConverter conversion from source." None

/// A generalized address.
type Address =
    inherit IComparable
    abstract Names : string array
    abstract HashCode : int
    abstract Anonymous : bool

/// Specifies the address of an identifiable value.
/// OPTIMIZATION: Names is an array only for speed; it is invalid to mutate it.
/// TODO: have Address constructors throw in Debug mode on invalid address names or if ellipses (...) are used in the
/// wrong place (not at the end).
type [<CustomEquality; CustomComparison; TypeConverter (typeof<AddressConverter>)>] 'a Address =
    { Names : string array
      HashCode : int // OPTIMIZATION: hash is cached for speed.
      Anonymous : bool } // HACK: allows for Nu to internally indicate the anonymity of an address.

    /// Get the length of an address by its names.
    member this.Length =
        Array.length this.Names

    /// Get whether an address is relative, IE, starts with the current symbol '~' or the parent symbol '^'. Otherwise,
    /// the address is absolute.
    member this.Relative =
        this.Names.Length > 0 &&
            let head = this.Names[0] in
            head = Constants.Address.ParentName || head = Constants.Address.CurrentName

    /// Make an empty address.
    /// NOTE: do not move this function as the AddressConverter's reflection code relies on it being exactly here!
    static member makeEmpty<'a> () : 'a Address =
        { Names = [||]; HashCode = 0; Anonymous = false }

    /// Make an address from a '/' delimited string.
    /// NOTE: do not move this function as the AddressConverter's reflection code relies on it being exactly here!
    static member makeFromString<'a> (addressStr : string) : 'a Address =
        if addressStr <> Constants.Address.EmptyStr then
            let names = addressStr.Split Constants.Address.SeparatorName
            { Names = names; HashCode = String.hashMany names; Anonymous = false }
        else Address.makeEmpty<'a> ()

    /// Hash an Address.
    static member inline hash (address : 'a Address) =
        address.HashCode

    /// Equate Addresses.
    static member equals<'a> (left : 'a Address) (right : 'a Address) =
        refEq left right || // OPTIMIZATION: first check ref equality.
        left.HashCode = right.HashCode && // OPTIMIZATION: check hash equality to bail as quickly as possible.
        String.equateManyBack left.Names right.Names // OPTIMIZATION: later names in an address tend to have higher variance.

    /// Compare Addresses.
    static member compare<'a> (left : 'a Address) (right : 'a Address) =
        if  refEq left right || // OPTIMIZATION: first check ref equality.
            left.HashCode = right.HashCode then // OPTIMIZATION: check hash equality to bail as quickly as possible.
            0
        else String.compareMany left.Names right.Names

    /// Convert any address to an obj Address.
    static member generalize<'a> (address : 'a Address) : obj Address =
        { Names = address.Names; HashCode = address.HashCode; Anonymous = address.Anonymous }

    /// Convert an obj address to any Address.
    static member specialize<'a> (address : obj Address) : 'a Address =
        { Names = address.Names; HashCode = address.HashCode; Anonymous = address.Anonymous }

    /// Convert a string into an address.
    static member stoa<'a> str =
        Address<'a>.makeFromString<'a> str

    /// Convert a names array into an address.
    static member rtoa<'a> (names : string array) : 'a Address =
        { Names = names; HashCode = String.hashMany names; Anonymous = false }

    /// Convert a names list into an address.
    static member ltoa<'a> (names : string list) : 'a Address =
        Address.rtoa<'a> (List.toArray names)

    /// Convert a names sequence into an address.
    static member qtoa<'a> (names : string seq) : 'a Address =
        Address.rtoa<'a> (Seq.toArray names)

    /// Convert a single name into an address.
    static member ntoa<'a> name : 'a Address =
        Address.rtoa<'a> [|name|]

    /// Convert a weakly-typed Address interface into a strongly-typed address.
    static member itoa (address : Address) =
        { Names = address.Names; HashCode = address.HashCode; Anonymous = address.Anonymous }

    /// Convert an address into a string.
    static member atos<'a> (address : 'a Address) =
        if address.Length <> 0
        then String.concat Constants.Address.SeparatorName address.Names
        else Constants.Address.EmptyStr

    /// Convert an address of type 'a to an address of type 'b.
    static member atoa<'a, 'b> (address : 'a Address) : 'b Address =
        { Names = address.Names; HashCode = address.HashCode; Anonymous = address.Anonymous }

    /// Convert any address to an obj Address.
    static member atooa<'a> (address : 'a Address) : obj Address =
        Address.generalize address

    /// Concatenate two addresses of the same type.
    static member acat<'a> (address : 'a Address) (address2 : 'a Address) : 'a Address=
        Address.rtoa<'a> (Array.append address.Names address2.Names)

    /// Concatenate two addresses, taking the type of first address.
    static member acatf<'a, 'b> (address : 'a Address) (address2 : 'b Address) : 'a Address =
        Address.acat address (Address.atoa<'b, 'a> address2)

    /// Concatenate two addresses, taking the type of second address.
    static member acats<'a, 'b> (address : 'a Address) (address2 : 'b Address) : 'b Address  =
        Address.acat (Address.atoa<'a, 'b> address) address2

    /// Make an absolute address with the name wildcard.
    static member Wildcard =
        Address.ntoa Constants.Address.WildcardName

    /// Make an absolute address with the address tail wildcard.
    static member Ellipsis =
        Address.ntoa Constants.Address.EllipsisName

    /// Concatenate two addresses of the same type.
    static member (-|-) (address : 'a Address, address2 : 'a Address) : 'a Address = Address.acat address address2

    /// Concatenate two addresses, taking the type of first address.
    static member (-->) (address : 'a Address, address2 : 'b Address) : 'a Address = Address.acatf address address2

    /// Concatenate two addresses, taking the type of second address.
    static member (<--) (address : 'a Address, address2 : 'b Address) : 'b Address = Address.acats address address2

    override this.Equals that =
        match that with
        | :? ('a Address) as that -> Address.equals<'a> this that
        | _ -> false

    override this.GetHashCode () =
        this.HashCode

    override this.ToString () =
        Address.atos<'a> this

    interface 'a Address IEquatable with
        member this.Equals that =
            Address<'a>.equals<'a> this that

    interface 'a Address IComparable with
        member this.CompareTo that =
            Address<'a>.compare this that

    interface IComparable with
        member this.CompareTo that =
            match that with
            | :? ('a Address) as that -> Address.compare this that
            | _ -> failwith "Cannot compare Address (comparee not of type Address)."

    interface Address with
        member this.Names = this.Names
        member this.HashCode = this.HashCode
        member this.Anonymous = this.Anonymous

/// Address functions.
[<RequireQualifiedAccess>]
module Address =

    /// The empty address.
    let empty<'a> : 'a Address =
        Address<'a>.makeEmpty<'a> ()

    /// Test address equality.
    let equals<'a> (left : 'a Address) (right : 'a Address) =
        Address<'a>.equals<'a> left right

    /// Make an address from a sequence of names.
    let makeFromSeq<'a> names : 'a Address =
        Address.qtoa<'a> names

    /// Make an address from a list of names.
    let makeFromArray<'a> names : 'a Address =
        Address.rtoa<'a> names

    /// Make an address from a list of names.
    let makeFromList<'a> names : 'a Address =
        Address.ltoa<'a> names

    /// Make an address from a name.
    let makeFromName<'a> name : 'a Address =
        Address.ntoa<'a> name

    /// Convert a weakly-typed Address interface into a strongly-typed address.
    let makeFromInterface<'a> address : 'a Address =
        Address.itoa<'a> address

    /// A relative address with the current symbol.
    let current<'a> =
        makeFromArray<'a> [|Constants.Address.CurrentName|]

    /// A relative address with the parent symbol.
    let parent<'a> =
        makeFromArray<'a> [|Constants.Address.ParentName|]

    /// Anonymize an address.
    let anonymize<'a> (address : 'a Address) : 'a Address =
        { Names = address.Names; HashCode = address.HashCode; Anonymous = true }

    /// Get the names of an address.
    let inline getNames address =
        address.Names

    /// Get the name of an address.
    let inline getName address =
        getNames address |> Array.last

    /// Attempt to get the name of an address if it exists.
    let tryGetName address =
        address |> getNames |> Array.tryLast

    /// Change the type of an address.
    let changeType<'a, 'b> (address : 'a Address) =
        Address.atoa<'a, 'b> address

    /// Get the address's hash code.
    let getHashCode address =
        Address.hash address

    /// Take the head of an address.
    let head address =
        Array.head address.Names
            
    /// Take the tail of an address.
    let tail<'a, 'b> (address : 'a Address) =
        makeFromArray<'b> (Array.tail address.Names)

    /// Take a name of an address.
    let item index address =
        Array.item index address.Names

    /// Take an address composed of the name of an address minus a skipped amount of names.
    let skip<'a, 'b> n (address : 'a Address) =
        makeFromArray<'b> (Array.skip n address.Names)

    /// Take an address composed of the given number of names of an address.
    let take<'a, 'b> n (address : 'a Address) =
        makeFromArray<'b> (Array.take n address.Names)

    /// Take an address composed of the given number of names of an address.
    let tryTake<'a, 'b> n (address : 'a Address) =
        makeFromArray<'b> (Array.tryTake n address.Names)

    /// Take the last name of an address.
    let last address =
        Array.last address.Names

    /// Take an address composed of all but the last name of an address.
    let allButLast<'a, 'b> (address : 'a Address) =
        makeFromArray<'b> (Array.allButLast address.Names)

    /// Find the index of a name
    let findIndex finder address =
        Array.findIndex finder address.Names

    /// Find the index of a name
    let indexOf name address =
        Array.IndexOf (address.Names, name)

    /// Get the length of an address by its names.
    let length (address : 'a Address) =
        address.Length
        
    /// Get whether an address is relative, IE, starts with the current symbol '~' or the parent symbol '^'. Otherwise,
    /// the address is absolute.
    let relative (address : 'a Address) =
        address.Relative

    /// Check that an address is devoid of names.
    let isEmpty address =
        Array.isEmpty address.Names

    /// Check that an address has one or more names.
    let notEmpty address =
        Array.notEmpty address.Names

    /// Check that an address name contains none of the invalid forms, specifically -
    /// [] is reserved as the empty address string
    /// / is reserved as the name separator
    let validateAddressName (name : string) =
        not (Constants.Address.InvalidAddressName.IsMatch name)

    /// Assert that an address name contains none of the invalid forms, specifically -
    /// [] is reserved as the empty address string
    /// / is reserved as the name separator
    let assertAddressName (name : string) =
#if DEBUG
        if not (validateAddressName name) then
            raise (ArgumentException ("Address name '" + name + "' contains an invalid form of [] or /, which are reserved."))
#else
        ignore name
#endif

    /// Check that an identifier name contains none of the invalid forms, specifically -
    /// [] is reserved as the empty address string
    /// / is reserved as the name separator
    /// * is reserved as the name wildcard
    /// ... is reserved as the address tail wildcard
    /// ^ is reserved as the parent symbol
    /// ~ is reserved as the current symbol
    let validateIdentifierName (name : string) =
        not (Constants.Address.InvalidIdentifierName.IsMatch name)

    /// Assert that an identifier name contains none of the invalid forms, specifically -
    /// [] is reserved as the empty address string
    /// / is reserved as the name separator
    /// * is reserved as the name wildcard
    /// ... is reserved as the address tail wildcard
    /// ^ is reserved as the parent symbol
    /// ~ is reserved as the current symbol
    let assertIdentifierName (name : string) =
#if DEBUG
        if not (validateIdentifierName name) then
            raise (ArgumentException ("Identifier name '" + name + "' contains an invalid form of [], /, *, ..., ^, or ~, which are reserved."))
#else
        ignore name
#endif

    let private resolveAsResizeArray<'a, 'b> (relation : 'b Address) (address : 'a Address) : string ResizeArray =
        let names = ResizeArray (relation.Length + address.Length)
        let mutable parentsUp = 0
        let addNames allowPointingPastEmpty (param : string) (a : _ Address) =
            let mutable wildcardExists = false
            for i in 0 .. dec a.Length do
                match a.Names[i] with
                | Constants.Address.ParentName as name ->
                    if names.Count - parentsUp > 0
                    then names.RemoveAt (names.Count - 1)
                    elif allowPointingPastEmpty then
                        names.Add name
                        parentsUp <- parentsUp + 1
                | Constants.Address.CurrentName -> ()
                | Constants.Address.EmptyStr as name ->
                    raise (ArgumentException (name + " cannot be an address name", param))
                | Constants.Address.EllipsisName as name ->
                    if i <> dec a.Length then raise (ArgumentException (name + " cannot be before the last name", param))
                    elif wildcardExists then raise (ArgumentException (name + " cannot appear together with another " + Constants.Address.WildcardName, param))
                    else names.Add name
                | Constants.Address.WildcardName as name ->
                    if wildcardExists then raise (ArgumentException (name + " cannot appear together with another " + name, param))
                    wildcardExists <- true
                    names.Add name
                | name -> names.Add name
        if relative relation then
            let addressRelative = relative address
            addNames addressRelative (nameof address) address
            addNames addressRelative (nameof relation) relation
            if addressRelative && (names.Count = 0 || names.[0] <> Constants.Address.ParentName) then
                names.Insert (0, Constants.Address.CurrentName)
        else addNames false (nameof relation) relation
        names
        
    /// Resolve an address from the given relation and address. When both the relation and address are relative, the
    /// result is a relative address. Otherwise, the result is an absolute address.
    let resolve<'a, 'b> (relation : 'b Address) (address : 'a Address) : 'b Address =
        let resolved = resolveAsResizeArray relation address
        makeFromSeq resolved

    /// Relate the second address to the first. The given addresses must be absolute. When the given addresses share
    /// common ancestors, the result is a relative address. Otherwise, the result is an absolute address.
    let relate<'a, 'b> (source : 'a Address) (destination : 'b Address) : 'b Address =
        if relative source then raise (ArgumentException ("Relative addresses cannot be related", nameof source))
        if relative destination then raise (ArgumentException ("Relative addresses cannot be related", nameof destination))
        let sourceNames = resolveAsResizeArray current source
        let destinationNames = resolveAsResizeArray current destination
        let namesMatching =
            let mutable namesMatching = 0
            let mutable sourceEnr = (sourceNames :> _ seq).GetEnumerator ()
            let mutable destinationEnr = (destinationNames :> _ seq).GetEnumerator ()
            while (sourceEnr.MoveNext () && destinationEnr.MoveNext ()) do
                if sourceEnr.Current = destinationEnr.Current then
                    namesMatching <- inc namesMatching
            namesMatching
        if namesMatching > 0 then
            let ancestors = sourceNames.Count - namesMatching
            let depth = destinationNames.Count - namesMatching
            Array.init (max ancestors 1 + depth) (fun i ->
                if i < ancestors then Constants.Address.ParentName
                elif i = 0 && ancestors = 0 then Constants.Address.CurrentName
                else destinationNames.[i - max ancestors 1 + namesMatching])
            |> makeFromArray
        else makeFromSeq destinationNames

/// Address operators.
[<AutoOpen>]
module AddressOperators =

    /// Convert an address of type 'a to an address of type 'b.
    let inline atoa<'a, 'b> (address : 'a Address) = Address.atoa<'a, 'b> address

    /// Convert a string into an address.
    let inline stoa<'a> str = Address<'a>.stoa<'a> str

    /// Convert a names array into an address.
    let inline rtoa<'a> names : 'a Address  = Address<'a>.rtoa<'a> names

    /// Convert a names list into an address.
    let inline ltoa<'a> names : 'a Address  = Address<'a>.ltoa<'a> names

    /// Convert a single name into an address.
    let inline ntoa<'a> name : 'a Address  = Address<'a>.ntoa<'a> name

    /// Convert a weakly-typed Address interface into a strongly-typed address.
    let inline itoa<'a> address : 'a Address  = Address<'a>.itoa<'a> address

    /// Convert an address into a string.
    let inline atos<'a> (address : 'a Address) = Address.atos<'a> address

    /// Convert any address to an obj Address.
    let inline atooa<'a> (address : 'a Address) = Address.atooa<'a> address

    /// Concatenate two addresses of the same type.
    let inline acat<'a> (address : 'a Address) (address2 : 'a Address) = Address.acat<'a> address address2

    /// Concatenate two addresses, taking the type of first address.
    let inline acatf<'a, 'b> (address : 'a Address) (address2 : 'b Address) = Address.acatf<'a, 'b> address address2

    /// Concatenate two addresses, taking the type of second address.
    let inline acats<'a, 'b> (address : 'a Address) (address2 : 'b Address) = Address.acats<'a, 'b> address address2