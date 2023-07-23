// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu.Constants
open System
open Nu

[<RequireQualifiedAccess>]
module Address =
    
    let [<Literal>] Separator = '/'
    let [<Literal>] SeparatorStr = "/"

namespace Nu
open System
open System.ComponentModel
open System.Reflection
open Prime
open Nu

/// Converts Address types.
type AddressConverter (targetType : Type) =
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
            let addressStr = toStringMethod.Invoke (source, null) :?> string
            if Symbol.shouldBeExplicit addressStr then Text (addressStr, ValueNone) :> obj
            else Atom (addressStr, ValueNone) :> obj
        elif destType = targetType then source
        else failconv "Invalid AddressConverter conversion to source." None

    override this.CanConvertFrom (_, sourceType) =
        sourceType = typeof<string> ||
        sourceType = typeof<Symbol> ||
        sourceType = targetType

    override this.ConvertFrom (_, _, source) =
        match source with
        | :? string as fullName ->
            let makeFromStringFunction = targetType.GetMethod ("makeFromString", BindingFlags.Static ||| BindingFlags.Public)
            let makeFromStringFunctionGeneric = makeFromStringFunction.MakeGenericMethod ((targetType.GetGenericArguments ()).[0])
            makeFromStringFunctionGeneric.Invoke (null, [|fullName|])
        | :? Symbol as addressSymbol ->
            match addressSymbol with
            | Atom (fullName, _) | Text (fullName, _) ->
                let makeFromStringFunction = targetType.GetMethod ("makeFromString", BindingFlags.Static ||| BindingFlags.Public)
                let makeFromStringFunctionGeneric = makeFromStringFunction.MakeGenericMethod ((targetType.GetGenericArguments ()).[0])
                makeFromStringFunctionGeneric.Invoke (null, [|fullName|])
            | Number (_, _) | Quote (_, _) | Symbols (_, _) ->
                failconv "Expected Symbol or String for conversion to Address." (Some addressSymbol)
        | _ ->
            if targetType.IsInstanceOfType source then source
            else failconv "Invalid AddressConverter conversion from source." None

[<AutoOpen>]
module Address =

    /// A generalized address.
    type Address =
        interface
            inherit IComparable
            abstract Names : string array
            abstract HashCode : int
            abstract Anonymous : bool
            end

    /// Specifies the address of an identifiable value.
    /// TODO: P1: have Address constructor throw if multiple wildcards are used (at least in Debug build mode).
    type [<CustomEquality; CustomComparison; TypeConverter (typeof<AddressConverter>)>] 'a Address =
        { Names : string array
          HashCode : int // OPTIMIZATION: hash is cached for speed
          Anonymous : bool } // HACK: allows for Nu to internally indicate the anonymity of an address.

        interface Address with
            member this.Names = this.Names
            member this.HashCode = this.HashCode
            member this.Anonymous = this.Anonymous

        /// Make an address from a '/' delimited string.
        /// NOTE: do not move this function as the AddressConverter's reflection code relies on it being exactly here!
        static member makeFromString<'a> (addressStr : string) : 'a Address =
            let names = addressStr.Split Constants.Address.Separator
            { Names = names; HashCode = String.hashMany names; Anonymous = false }

        /// Hash an Address.
        static member inline hash (address : 'a Address) =
            address.HashCode

        /// Equate Addresses.
        static member equals left right =
            refEq left right || // OPTIMIZATION: first check ref equality
            left.HashCode = right.HashCode && // OPTIMIZATION: check hash equality to bail as quickly as possible
            String.equateMany left.Names right.Names

        /// Compare Addresses.
        static member compare left right =
            String.compareMany left.Names right.Names

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

        /// Convert a single name into an address.
        static member ntoa<'a> name : 'a Address =
            Address.rtoa<'a> [|name|]

        /// Convert a weakly-typed Address interface into a strongly-typed address.
        static member itoa (address : Address) =
            { Names = address.Names; HashCode = address.HashCode; Anonymous = address.Anonymous }

        /// Convert a string into an address.
        static member atos<'a> (address : 'a Address) =
            String.concat Constants.Address.SeparatorStr address.Names

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
        static member acatf<'a> (address : 'a Address) (address2 : obj Address) : 'a Address =
            Address.rtoa<'a> (Array.append address.Names address2.Names)
    
        /// Concatenate two addresses, forcing the type of first address.
        static member acatff<'a, 'b> (address : 'a Address) (address2 : 'b Address) : 'a Address =
            Address.acatf address (Address.atooa address2)

        /// Concatenate two addresses, taking the type of the second address.
        static member acats<'a> (address : obj Address) (address2 : 'a Address) : 'a Address =
            Address.rtoa<'a> (Array.append address.Names address2.Names)
    
        /// Concatenate two addresses, forcing the type of second address.
        static member acatsf<'a, 'b> (address : 'a Address) (address2 : 'b Address) : 'b Address  =
            Address.acats (Address.atooa address) address2

        /// Concatenate two addresses of the same type.
        static member (-|-) (address : 'a Address, address2 : 'a Address) = Address.acat address address2

        /// Concatenate two addresses, taking the type of first address.
        static member (->-) (address : 'a Address, address2 : obj Address) = Address.acatf address address2

        /// Concatenate two addresses, forcing the type of first address.
        static member (-->) (address : 'a Address, address2 : 'b Address) = Address.acatff address address2

        /// Concatenate two addresses, taking the type of the second address.
        static member (-<-) (address : obj Address, address2 : 'b Address) = Address.acats address address2

        /// Concatenate two addresses, forcing the type of second address.
        static member (<--) (address : 'a Address, address2 : 'b Address) = Address.acatsf address address2

        interface 'a Address IComparable with
            member this.CompareTo that =
                Address<'a>.compare this that

        interface IComparable with
            member this.CompareTo that =
                match that with
                | :? ('a Address) as that -> Address<'a>.compare this that
                | _ -> failwith "Invalid Address comparison (comparee not of type Address)."

        interface 'a Address IEquatable with
            member this.Equals that =
                Address<'a>.equals this that

        override this.Equals that =
            match that with
            | :? ('a Address) as that -> Address<'a>.equals this that
            | _ -> false

        override this.GetHashCode () =
            this.HashCode

        override this.ToString () =
            Address.atos<'a> this

    [<RequireQualifiedAccess>]
    module Address =

        /// The empty address.
        let empty<'a> : 'a Address =
            { Names = [||]; HashCode = String.hashMany [||]; Anonymous = false }

        /// Test address equality.
        let equals (left : 'a Address) (right : 'a Address) =
            Address<'a>.equals left right

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
        let makeFromInterface address : 'a Address =
            Address.itoa<'a> address

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

        /// Get the length of an address by its names.
        let length address =
            Array.length address.Names

        /// Check that an address is devoid of names.
        let isEmpty address =
            Array.isEmpty address.Names

        /// Check that an address has one or more names.
        let notEmpty address =
            Array.notEmpty address.Names

        /// Check that a string represents a valid address name.
        let validName (name : string) =
            not (name.Contains "/") &&
            not (name.Contains "\"")

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
    let inline acatf<'a> (address : 'a Address) (address2 : obj Address) = Address.acatf<'a> address address2

    /// Concatenate two addresses, forcing the type of first address.
    let inline acatff<'a, 'b> (address : 'a Address) (address2 : 'b Address) = Address.acatff<'a, 'b> address address2

    /// Concatenate two addresses, taking the type of the second address.
    let inline acats<'a> (address : obj Address) (address2 : 'a Address) = Address.acats<'a> address address2

    /// Concatenate two addresses, forcing the type of second address.
    let inline acatsf<'a, 'b> (address : 'a Address) (address2 : 'b Address) = Address.acatsf<'a, 'b> address address2

/// Specifies the address of an identifiable value.
type 'a Address = 'a Address.Address