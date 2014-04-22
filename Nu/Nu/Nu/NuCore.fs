namespace Nu
open System
open Prime

[<AutoOpen>]
module NuCoreModule =

    /// Specifies the address of an element in a game.
    /// Note that subscribing to a partial address results in listening to all messages whose
    /// beginning address nodes match the partial address (sort of a wild-card).
    type Address = Lun list

    /// Specifies the screen-clearing routine.
    type ScreenClear =
        | NoClear
        | ColorClear of byte * byte * byte

module NuCore =

    /// The invalid Id.
    let InvalidId = Guid.Empty

    /// Create a Nu Id.
    let getNuId = Guid.NewGuid

    let addr (str : string) : Address =
        let strs = List.ofArray <| str.Split '/'
        List.map Lun.make strs

    let straddr str (address : Address) : Address =
        addr str @ address

    let addrstr (address : Address) str : Address =
        address @ [Lun.make str]

    let straddrstr str (address : Address) str2 : Address =
        addr str @ address @ addr str2

    let addrToStr (address : Address) =
        List.fold (fun str (lun : Lun) -> str + lun.LunStr) String.Empty address

    let (</>) str str2 =
        str + "/" + str2