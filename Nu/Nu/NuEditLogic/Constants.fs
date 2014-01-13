namespace NuEdit
open Nu.Core
module Constants =

    let EditorScreenAddress = addr "editorScreen"
    let EditorGroupName = Lun.make "editorGroup"
    let EditorGroupAddress = EditorScreenAddress @ [EditorGroupName]

