[<AutoOpen>]
module XtensionModule
open System.Reflection
open Microsoft.FSharp.Reflection

type XFields =
    Map<Lun, obj>

type [<StructuralEqualityAttribute; NoComparison>] Xtension =
    { OptName : Lun option
      Fields : XFields }

    // NOTE: this could instead be a special class with a MethodMissing method
    static member private EmptyDispatcher = new obj ()

    static member (?) (this : Xtension, memberName) : 'a -> 'r =

        // NOTE: I think the explicit args abstraction is required here to satisfy the signature
        // for op_Dynamic... maybe.
        fun args ->

            // check if dynamic member is a method or a field
            match Map.tryFind (Lun.makeFast memberName) this.Fields with
            | None ->
                
                // try to convert method args to an array
                let optArgArray =
                    if box args = null then null // not sure what this line is meant to do
                    elif FSharpType.IsTuple (args.GetType ()) then FSharpValue.GetTupleFields args
                    else [|args|]
                
                // ensure arg array exists
                match optArgArray with
                | null -> failwith "Xtension method call must have at least 1 argument."
                | argArray ->

                    // ensure last arg is a dispatcher container
                    match Array.last argArray with
                    | :? IXDispatcherContainer as context ->

                        // find dispatcher, or use the empty dispatcher
                        let dispatcher = 
                            match this.OptName with
                            | None -> Xtension.EmptyDispatcher
                            | Some name ->
                                let dispatchers = context.GetDispatchers ()
                                match Map.tryFind name dispatchers with
                                | None -> note <| "Invalid dispatcher '" + name.LunStr + "'."; Xtension.EmptyDispatcher
                                | Some dispatcher -> dispatcher

                        // dispatch method call
                        match (dispatcher.GetType ()).GetMethod memberName with
                        | null -> failwith <| "Could not find method '" + memberName + "'."
                        | aMethod -> aMethod.Invoke (dispatcher, argArray) :?> 'r
                    | _ -> failwith "Last argument of Xtension method call must be an IXDispatcherContainer."

            // just return field
            | Some field -> field :?> 'r

    static member (?<-) (this : Xtension, fieldName, value) =
        let fields = Map.add (Lun.makeFast fieldName) (value :> obj) this.Fields
        { this with Fields = fields }

and IXDispatchers =
    Map<Lun, obj>

and IXDispatcherContainer =
    interface
        abstract GetDispatchers : unit -> IXDispatchers
        end

and [<StructuralEquality; NoComparison>] XDispatcherContainer =
    { Dispatchers : IXDispatchers }
    interface IXDispatcherContainer with
        member this.GetDispatchers () = this.Dispatchers
        end

type XFieldDescriptor =
    { Name : Lun
      Type : Lun }

type XImplication =
    | NameToNames of Lun * Lun list
    | NameToFields of Lun * XFieldDescriptor list

type XImplications =
    Map<Lun, XImplication>