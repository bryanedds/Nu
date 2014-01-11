[<AutoOpen>]
module XtensionModule
open System.Reflection

type XFields = Map<Lun, obj>

type [<StructuralEqualityAttribute; NoComparison>] Xtension =
    { OptName : Lun option
      Fields : XFields }

// Any implementing type must contain no data as it would use XFields instead.
type XDispatcher =
    interface
        abstract GetDispatches : unit -> MethodInfo seq
        end

type XDispatchers = Map<Lun, XDispatcher>