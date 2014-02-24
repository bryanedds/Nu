namespace NuEdit
open Nu.NuCore
module NuEditConstants =

    let EditorScreenAddress = addr "EditorScreen"
    let EditorGroupName = Lun.make "EditorGroup"
    let EditorGroupAddress = EditorScreenAddress @ [EditorGroupName]