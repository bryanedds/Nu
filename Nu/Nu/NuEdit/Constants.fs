namespace NuEdit
open Nu.Core
module Constants =

    let EditorScreenAddress = addr "EditorScreen"
    let EditorGroupName = Lun.make "EditorGroup"
    let EditorGroupAddress = EditorScreenAddress @ [EditorGroupName]