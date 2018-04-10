// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2018.

namespace Prime

/// An option type that doesn't generate garbage.
/// TODO: P1: turn this into a struct DU when that is working in F# - and make sure matching does not allocate!
/// TODO: document.
type 'a FOption =
    struct
        val private Exists : bool
        val private ValueOpt : 'a
        private new (_ : unit) = { Exists = false; ValueOpt = Unchecked.defaultof<'a> }
        private new (_ : unit, value : 'a) = { Exists = true; ValueOpt = value }
        static member Some (value : 'a) = new FOption<'a> ((), value)
        static member None () = new FOption<'a> (())
        static member FromOpt (opt : 'a option) = match opt with Option.Some value -> FOption.Some value | None -> FOption.None ()
        static member ToOpt (fopt : FOption<'a>) = if fopt.Exists then Some fopt.ValueOpt else None
        member this.IsSome = this.Exists
        member this.IsNone = not this.Exists
#if DEBUG
        member this.Value = if this.Exists then this.ValueOpt else failwith "FOption has no value to get."
#else
        member this.Value = this.ValueOpt
#endif
        member this.Map (fn : 'a -> 'b) = if this.Exists then new FOption<'b> ((), fn this.ValueOpt) else new FOption<'b> (())
        member this.Bind (fn : 'a -> 'b FOption) = if this.Exists then fn this.Value else new FOption<'b> (())
        end

[<RequireQualifiedAccess>]
module FOption =

    let fromOpt opt = FOption.FromOpt opt
    let toOpt fopt = FOption.ToOpt fopt
    let some value = FOption.Some value
    let none () = FOption.None ()
    let isSome (fopt : FOption<'a>) = fopt.IsSome
    let isNone (fopt : FOption<'a>) = fopt.IsNone
    let get (fopt : FOption<'a>) = fopt.Value
    let map fn (fopt : FOption<'a>) = fopt.Map fn
    let bind fn (fopt : FOption<'a>) = fopt.Bind fn