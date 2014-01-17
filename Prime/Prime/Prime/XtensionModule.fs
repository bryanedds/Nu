[<AutoOpen>]
module XtensionModule
open System.Reflection
open Microsoft.FSharp.Reflection

type XFields =
    Map<Lun, obj>

type [<StructuralEqualityAttribute; NoComparison>] Xtension =
    { OptName : Lun option
      Fields : XFields }

/// Any implementing type must contain no data as it would use XFields instead.
type IXDispatcher =
    interface
        abstract GetDispatches : unit -> MethodInfo seq
        end

type IXDispatchers =
    Map<Lun, IXDispatcher>

type IXDispatcherContainer =
    interface
        abstract GetDispatchers : unit -> IXDispatchers
        end

type [<StructuralEquality; NoComparison>] XDispatcherContainer =
    { Dispatchers : IXDispatchers }
    interface IXDispatcherContainer with
        member this.GetDispatchers () = this.Dispatchers
        end

let (?) (this : Xtension) memberName args : 'r =
    match Map.tryFind (Lun.makeFast memberName) this.Fields with
    | None ->
        let argArray =
            if box args = null then null
            elif FSharpType.IsTuple (args.GetType ()) then FSharpValue.GetTupleFields args
            else [|args|]
        if argArray = null then
            failwith "Xtension method call must have at least 1 argument."
        match Array.last argArray with
        | :? IXDispatcherContainer as context ->
            let dispatchers = context.GetDispatchers ()
            let dispatcher = Map.find this.OptName.Value dispatchers
            match (dispatcher.GetType ()).GetMethod memberName with
            | null -> failwith <| "Could not find method '" + memberName + "'."
            | aMethod -> aMethod.Invoke (dispatcher, argArray) :?> 'r
        | _ -> failwith "Last argument of Xtension meethod call must be an IDispatchContainer."
    | Some field -> field :?> 'r

let (?<-) (this : Xtension) fieldName value =
    let fields = Map.add (Lun.makeFast fieldName) (value :> obj) this.Fields
    { this with Fields = fields }