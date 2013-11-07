// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2013.

[<AutoOpenAttribute>]
module LunModule
open System

/// The look-up name of a value.
type [<CustomEquality; CustomComparison>] Lun =
    { LunStr : string
      LunHash : int
      LunOptNums : (int64 * int64) option }

    static member private isNonNumableChar chr =
        chr > '\u0080'

    static member private strToNum strStart str =
        let mutable num = 0L;
        let mutable shl = 0;
        let mutable idx = strStart
        let idxMax = Math.Min (String.length str, 9 + strStart)
        while idx < idxMax do
            num <- (num ||| (int64 str.[idx] <<< shl))
            shl <- shl + 7
            idx <- idx + 1
        num

    static member private makeOptNums str =
        let strLen = String.length str
        if strLen > 18 ||
           String.exists Lun.isNonNumableChar str then
           None
        else
            let num = Lun.strToNum 0 str
            let num2 = if strLen > 9 then Lun.strToNum 9 str else 0L
            Some (num, num2)

    static member private makeInternal str hash optNums =
        { LunStr = str; LunHash = hash; LunOptNums = optNums }

    static member make str =
        Lun.makeInternal str (str.GetHashCode ()) (Lun.makeOptNums str)

    static member (++) (left, right) =
        Lun.make (left.LunStr + right.LunStr)

    override this.Equals that =
        match that with
        | :? Lun as thatLun ->
            match (this.LunOptNums, thatLun.LunOptNums) with
            | (Some (thisNum, thisNum2), Some (thatNum, thatNum2)) ->
                thisNum = thatNum && thisNum2 = thatNum2
            | _ ->
                this.LunStr = thatLun.LunStr
        | _ -> false

    override this.ToString () =
        this.LunStr

    override this.GetHashCode () =
        this.LunHash

    interface System.IComparable with
        override this.CompareTo that =
            // OPTIMIZATION: this code is highly optimized
            match that with
            | :? Lun as thatLun ->
                // first try fast comparison...
                let thisHash = this.LunHash
                let thatHash = thatLun.LunHash
                if thisHash = thatHash then
                    // do speculatively fast comparison if possible, linear-time comparison otherwise
                    match (this.LunOptNums, thatLun.LunOptNums) with
                    | (Some (thisNum, thisNum2), Some (thatNum, thatNum2)) ->
                        // speculatively fast comparison (this has not (yet?) been emperically shown to be faster!)
                        if thisNum2 = thatNum2 then
                            if thisNum = thatNum then 0
                            elif thisNum < thatNum then -1
                            else 1
                        elif thisNum2 < thatNum2 then -1
                        else 1
                    | _ ->
                        // linear-time comparison
                        thatLun.LunStr.CompareTo this.LunStr
                elif thisHash < thatHash then -1
                else 1
            | _ -> invalidArg "that" "Cannot compare a Lun value to a different type of object."