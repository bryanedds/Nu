// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2014.

namespace Prime
open System

[<AutoOpen>]
module LunModule =

    /// The look-up name of a value with optimized performance characteristcs.
    type [<CustomEquality; CustomComparison>] Lun =
        { LunStr : string
          LunHash : int
          LunOptNums : (int64 * int64) option }

        static member private MaxChar = '\u007F'

        static member private isNonNumableChar chr =
            chr > Lun.MaxChar

        static member private strToNum strStart str =
            let idxMax = Math.Min (String.length str, 9 + strStart)
            let mutable num = 0L
            let mutable lsh = 56
            let mutable idx = strStart
            while idx < idxMax do
#if DEBUG
                let chr = str.[idx]
                if int chr &&& int Lun.MaxChar <> int chr then
                    failwith <| "Invalid character '" + string chr + "' in Lun.strToNum (must be less than Lun.MaxChar)."
#endif
                let bt7 = int64 str.[idx] <<< lsh
                num <- num ||| bt7
                lsh <- lsh - 7
                idx <- idx + 1
            num

        static member private makeOptNums str =
            let strLen = String.length str
            if strLen > 18 ||
               String.exists Lun.isNonNumableChar str then
               None
            else
                let major = Lun.strToNum 0 str
                let minor = if strLen > 9 then Lun.strToNum 9 str else 0L
                Some (major, minor)

        static member private makeInternal str hash optNums =
            { LunStr = str; LunHash = hash; LunOptNums = optNums }

        static member make str =
            Lun.makeInternal str (str.GetHashCode ()) (Lun.makeOptNums str)

        static member makeFast str =
            Lun.makeInternal str (str.GetHashCode ()) None

        static member (++) (left, right) =
            Lun.make (left.LunStr + right.LunStr)

        override this.Equals that =
            match that with
            | :? Lun as thatLun ->
                match (this.LunOptNums, thatLun.LunOptNums) with
                | (Some (thisMajor, thisMinor), Some (thatMajor, thatMinor)) -> thisMajor = thatMajor && thisMinor = thatMinor
                | _ -> this.LunStr = thatLun.LunStr
            | _ -> false

        override this.ToString () =
            this.LunStr

        override this.GetHashCode () =
            this.LunHash

        interface System.IComparable with
            override this.CompareTo that =
                match that with
                | :? Lun as thatLun ->
                    // first try fast comparison...
                    let thisHash = this.LunHash
                    let thatHash = thatLun.LunHash
                    if thisHash = thatHash then
                        // do constant-time comparison if possible, linear-time comparison otherwise
                        match (this.LunOptNums, thatLun.LunOptNums) with
                        | (Some (thisMajor, thisMinor), Some (thatMajor, thatMinor)) ->
                            // constant-time comparison
                            if thisMajor = thatMajor then
                                if thisMinor = thatMinor then 0
                                elif thisMinor < thatMinor then -1
                                else 1
                            elif thisMajor < thatMajor then -1
                            else 1
                        | _ ->
                            // linear-time comparison. Note that String.CompareTo cannot be used as it has different
                            // semantics than the above binary comparison
                            let (thisStr, thatStr) = (this.LunStr, thatLun.LunStr)
                            if thisStr = thatStr then 0
                            elif thisStr < thatStr then -1
                            else 1
                    elif thisHash < thatHash then -1
                    else 1
                | _ -> invalidArg "that" "Cannot compare a Lun value to a different type of object."

[<RequireQualifiedAccess>]
module Lun =

    /// The empty Lun.
    let empty = Lun.make String.Empty

    /// Join multiple Luns.
    let join sep (luns : Lun list) =
        if luns.IsEmpty then empty
        else
            let strs = List.map (fun lun -> lun.LunStr) luns
            let joinedStr = List.join sep.LunStr strs
            Lun.make joinedStr