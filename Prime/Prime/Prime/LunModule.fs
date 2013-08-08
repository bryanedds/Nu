// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2013.

[<AutoOpenAttribute>]
module LunModule
open System

/// The look-up name of a value.
type [<CustomEquality; CustomComparison>] Lun =
    { LunStr : string
      LunHash : int }

    static member private makeInternal str hash =
        { LunStr = str; LunHash = hash }

    static member make str =
        Lun.makeInternal str (str.GetHashCode ())

    static member (++) (left, right) =
        Lun.make (left.LunStr + right.LunStr)

    override this.Equals other =
        match other with
        | :? Lun as otherLun -> this.LunStr = otherLun.LunStr
        | _ -> false

    override this.ToString () =
        this.LunStr

    override this.GetHashCode () =
        this.LunHash

    interface System.IComparable with
        override this.CompareTo other = 
            match other with
            | :? Lun as otherLun ->
                let thisHash = this.LunHash
                let otherHash = otherLun.LunHash
                if thisHash < otherHash then -1
                elif thisHash > otherHash then 1
                else otherLun.LunStr.CompareTo this.LunStr
            | _ -> invalidArg "other" "Cannot compare a Lun value to a different type of object."