module Xtension
open System
open System.ComponentModel

/// The empty extension.
let empty = { OptName = None; Fields = Map.empty }

type XtensionTypeConverter () =
    inherit TypeConverter ()
    override this.CanConvertTo (_, destType) =
        destType = typeof<string>
    override this.ConvertTo (_, culture, obj : obj, _) =
        let xtension = obj :?> Xtension
        let nameStr = match xtension.OptName with None -> String.Empty | Some name -> name.LunStr
        String.Format (culture, "{0}", nameStr) :> obj
    override this.CanConvertFrom (_, sourceType) =
        sourceType = typeof<Xtension> || sourceType = typeof<string>
    override this.ConvertFrom (_, culture, obj : obj) =
        let sourceType = obj.GetType ()
        if sourceType = typeof<Xtension> then obj
        else
            let optName = match obj :?> string with "" -> None | name -> Some <| Lun.make name
            { OptName = optName; Fields = Map.empty } :> obj

let initXtensionConverters () =
    assignTypeConverter<Xtension, XtensionTypeConverter> ()