namespace Nu
open System

[<AutoOpen>]
module NuCoreModule =

    /// Specifies the address of an element in a game.
    /// Note that subscribing to a partial address results in listening to all messages whose
    /// beginning address nodes match the partial address (sort of a wild-card).
    type Address = Lun list

module NuCore =

    /// Create a Nu Id.
    /// NOTE: Ironically, not purely functional.
    let getNuId = createGetNextId ()

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