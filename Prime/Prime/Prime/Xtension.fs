module Xtension
open System
open System.ComponentModel

/// The empty extension.
let empty = { OptXTypeName = None; Fields = Map.empty }

type XtensionTypeConverter () =
    inherit TypeConverter ()
    override this.CanConvertTo (_, destType) =
        destType = typeof<string>
    override this.ConvertTo (_, culture, obj : obj, _) =
        let xtension = obj :?> Xtension
        let xTypeNameStr = match xtension.OptXTypeName with None -> String.Empty | Some xTypeName -> xTypeName.LunStr
        String.Format (culture, "{0}", xTypeNameStr) :> obj
    override this.CanConvertFrom (_, sourceType) =
        sourceType = typeof<Xtension> || sourceType = typeof<string>
    override this.ConvertFrom (_, culture, obj : obj) =
        let sourceType = obj.GetType ()
        if sourceType = typeof<Xtension> then obj
        else
            let optXTypeName = match obj :?> string with "" -> None | xTypeNameStr -> Some <| Lun.make xTypeNameStr
            { OptXTypeName = optXTypeName; Fields = Map.empty } :> obj

let initXtensionConverters () =
    assignTypeConverter<Xtension, XtensionTypeConverter> ()