// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace ImGuiNET
open System
open System.IO
open ImGuiNET
open Prime
open Nu

// This code was converted from -
// https://github.com/japajoe/ImGuiFileDialog/blob/04f4b7dbb690c26b7503c6c5a09202c35684b12e/ImGuiFileDialog.cs

type internal ImGuiFileSortOrder =
    | Ascending = 0
    | Descending = 1
    | Unsorted = 2

type ImGuiFileDialogType =
    | Open = 0
    | Save = 1

type [<AllowNullLiteral>] ImGuiFileDialogState (directoryPath : string) =
    member val Title : string = "" with get, set
    member val FileDialogType : ImGuiFileDialogType = ImGuiFileDialogType.Open with get, set
    member val FileName : string = "" with get, set
    member val DirectoryPath : DirectoryInfo = DirectoryInfo (if String.notEmpty directoryPath then directoryPath else ".") with get, set
    member val ResultPath : string = "" with get, set
    member val RefreshInfo : bool = false with get, set
    member val CurrentDirectories : list<DirectoryInfo> = [] with get, set
    member val CurrentFiles : list<FileInfo> = [] with get, set
    member val CurrentIndex : UInt64 = 0UL with get, set
    member this.FilePath
        with get () = this.DirectoryPath.FullName + "\\" + this.FileName
        and set (value : string) =
            this.FileName <- Path.GetFileName value
            this.DirectoryPath <- DirectoryInfo (Path.GetDirectoryName value)

[<RequireQualifiedAccess>]
module ImGui =

    let mutable private spacingColumn0 = 230.0f
    let mutable private spacingColumn1 = 80.0f
    let mutable private spacingColumn2 = 90.0f
    let mutable private fileNameSortOrder = ImGuiFileSortOrder.Unsorted
    let mutable private fileNameSortOrderCopy = ImGuiFileSortOrder.Unsorted
    let mutable private sizeSortOrder = ImGuiFileSortOrder.Unsorted
    let mutable private sizeSortOrderCopy = ImGuiFileSortOrder.Unsorted
    let mutable private dateSortOrder = ImGuiFileSortOrder.Unsorted
    let mutable private dateSortOrderCopy = ImGuiFileSortOrder.Unsorted
    let mutable private typeSortOrder = ImGuiFileSortOrder.Unsorted
    let mutable private typeSortOrderCopy = ImGuiFileSortOrder.Unsorted

    let private refreshInfo (dialogInfo : ImGuiFileDialogState) =
        dialogInfo.RefreshInfo <- false
        dialogInfo.CurrentDirectories <- []
        dialogInfo.CurrentFiles <- []
        dialogInfo.CurrentIndex <- 0UL

        let directory = DirectoryInfo dialogInfo.DirectoryPath.FullName
        dialogInfo.CurrentDirectories <- directory.GetDirectories () |> Seq.toList
        dialogInfo.CurrentFiles <- directory.GetFiles () |> Seq.toList

    let private sort (dialogState : ImGuiFileDialogState, forceSort : bool) =
        let mutable sort = false

        if  fileNameSortOrderCopy <> fileNameSortOrder then
            fileNameSortOrderCopy <- fileNameSortOrder
            sort <- true

        if  sizeSortOrderCopy <> sizeSortOrder then
            sizeSortOrderCopy <- sizeSortOrder
            sort <- true

        if  dateSortOrderCopy <> dateSortOrder then
            dateSortOrderCopy <- dateSortOrder
            sort <- true

        if  typeSortOrderCopy <> typeSortOrder then
            typeSortOrderCopy <- typeSortOrder
            sort <- true

        if sort || forceSort then

            // Sort directories
            if fileNameSortOrder <> ImGuiFileSortOrder.Unsorted || sizeSortOrder <> ImGuiFileSortOrder.Unsorted || typeSortOrder <> ImGuiFileSortOrder.Unsorted then
                dialogState.CurrentDirectories <-
                    if fileNameSortOrder = ImGuiFileSortOrder.Descending
                    then dialogState.CurrentDirectories |> List.sortBy (fun i -> i.Name)
                    else dialogState.CurrentDirectories |> List.sortByDescending (fun i -> i.Name)
            elif dateSortOrder <> ImGuiFileSortOrder.Unsorted then
                dialogState.CurrentDirectories <-
                    if dateSortOrder = ImGuiFileSortOrder.Descending
                    then dialogState.CurrentDirectories |> List.sortBy (fun i -> i.LastWriteTime)
                    else dialogState.CurrentDirectories |> List.sortByDescending (fun i -> i.LastWriteTime)

            // Sort files
            if fileNameSortOrder <> ImGuiFileSortOrder.Unsorted then
                dialogState.CurrentFiles <-
                    if fileNameSortOrder = ImGuiFileSortOrder.Descending
                    then dialogState.CurrentFiles |> List.sortBy (fun i -> i.Name)
                    else dialogState.CurrentFiles |> List.sortByDescending (fun i -> i.Name)
            elif sizeSortOrder <> ImGuiFileSortOrder.Unsorted then
                dialogState.CurrentFiles <-
                    if sizeSortOrder = ImGuiFileSortOrder.Descending
                    then dialogState.CurrentFiles |> List.sortBy (fun i -> i.Length)
                    else dialogState.CurrentFiles |> List.sortByDescending (fun i -> i.Length)
            elif typeSortOrder <> ImGuiFileSortOrder.Unsorted then
                dialogState.CurrentFiles <-
                    if typeSortOrder = ImGuiFileSortOrder.Descending
                    then dialogState.CurrentFiles |> List.sortBy (fun i -> i.Extension)
                    else dialogState.CurrentFiles |> List.sortByDescending (fun i -> i.Extension)
            elif dateSortOrder <> ImGuiFileSortOrder.Unsorted then
                dialogState.CurrentFiles <-
                    if dateSortOrder = ImGuiFileSortOrder.Descending
                    then dialogState.CurrentFiles |> List.sortBy (fun i -> i.LastWriteTime)
                    else dialogState.CurrentFiles |> List.sortByDescending (fun i -> i.LastWriteTime)

    let FileDialog (opened : bool byref, dialogState : ImGuiFileDialogState) =

        if opened then

            let mutable complete = false

            ImGui.PushID (dialogState.GetHashCode ())
            ImGui.SetNextWindowSize (v2 740.0f 410.0f, ImGuiCond.FirstUseEver)

            if not (ImGui.IsPopupOpen dialogState.Title) then ImGui.OpenPopup dialogState.Title
            if ImGui.BeginPopupModal (dialogState.Title, &opened, ImGuiWindowFlags.NoDocking) then

                if dialogState.CurrentFiles.IsEmpty && dialogState.CurrentDirectories.IsEmpty || dialogState.RefreshInfo then
                    refreshInfo dialogState

                // Draw path
                ImGui.Text ("Path: " + dialogState.DirectoryPath.FullName)

                let contentRegionWidth = ImGui.GetWindowContentRegionMax().X - ImGui.GetWindowContentRegionMin().X

                ImGui.BeginChild ("##browser", v2 contentRegionWidth 300.0f, true, ImGuiWindowFlags.HorizontalScrollbar) |> ignore<bool>
                ImGui.Columns 4

                // Columns size
                if spacingColumn0 > 0.0f then
                    ImGui.SetColumnWidth (0, spacingColumn0)
                    spacingColumn0 <- 0.0f

                if spacingColumn1 > 0.0f then
                    ImGui.SetColumnWidth (1, spacingColumn1)
                    spacingColumn1 <- 0.0f

                if spacingColumn2 > 0.0f then
                    ImGui.SetColumnWidth (2, spacingColumn2)
                    spacingColumn2 <- 0.0f

                // File Columns
                if ImGui.Selectable "Name" then
                    sizeSortOrder <- ImGuiFileSortOrder.Unsorted
                    dateSortOrder <- ImGuiFileSortOrder.Unsorted
                    typeSortOrder <- ImGuiFileSortOrder.Unsorted
                    fileNameSortOrder <- if fileNameSortOrder = ImGuiFileSortOrder.Descending then ImGuiFileSortOrder.Ascending else ImGuiFileSortOrder.Descending
                    fileNameSortOrderCopy <- fileNameSortOrder
                    sort (dialogState, true)
                ImGui.NextColumn ()

                if ImGui.Selectable "Size" then
                    fileNameSortOrder <- ImGuiFileSortOrder.Unsorted
                    dateSortOrder <- ImGuiFileSortOrder.Unsorted
                    typeSortOrder <- ImGuiFileSortOrder.Unsorted
                    sizeSortOrder <- if sizeSortOrder = ImGuiFileSortOrder.Descending then ImGuiFileSortOrder.Ascending else ImGuiFileSortOrder.Descending
                    sizeSortOrderCopy <- sizeSortOrder
                    sort (dialogState, true)
                ImGui.NextColumn()

                if ImGui.Selectable "Type" then
                    fileNameSortOrder <- ImGuiFileSortOrder.Unsorted
                    dateSortOrder <- ImGuiFileSortOrder.Unsorted
                    sizeSortOrder <- ImGuiFileSortOrder.Unsorted
                    typeSortOrder <- if typeSortOrder = ImGuiFileSortOrder.Descending then ImGuiFileSortOrder.Ascending else ImGuiFileSortOrder.Descending
                    typeSortOrderCopy <- typeSortOrder
                    sort (dialogState, true)
                ImGui.NextColumn()

                if ImGui.Selectable "Date" then
                    fileNameSortOrder <- ImGuiFileSortOrder.Unsorted
                    sizeSortOrder <- ImGuiFileSortOrder.Unsorted
                    typeSortOrder <- ImGuiFileSortOrder.Unsorted
                    dateSortOrder <- if dateSortOrder = ImGuiFileSortOrder.Descending then ImGuiFileSortOrder.Ascending else ImGuiFileSortOrder.Descending
                    dateSortOrderCopy <- dateSortOrder
                    sort (dialogState, true)
                ImGui.NextColumn ()

                // File Separator
                ImGui.Separator ()

                // Sort directories
                let directories = dialogState.CurrentDirectories
                let files = dialogState.CurrentFiles
                sort (dialogState, false)

                let mutable index = 0UL

                // Draw parent
                if dialogState.DirectoryPath.Parent <> null then
                    let mutable parentPath = dialogState.DirectoryPath.Parent
                    let mutable parentName = ".."
                    let mutable lastWriteTime = parentPath.LastWriteTime
                    let mutable size = "-"

                    if ImGui.Selectable (parentName, dialogState.CurrentIndex = index, ImGuiSelectableFlags.AllowDoubleClick, v2 contentRegionWidth 0.0f) then
                        dialogState.CurrentIndex <- index

                        if ImGui.IsMouseDoubleClicked ImGuiMouseButton.Left then
                            dialogState.DirectoryPath <- parentPath
                            dialogState.RefreshInfo <- true
                            sort (dialogState, true)
                    ImGui.NextColumn ()
                    ImGui.TextUnformatted size
                    ImGui.NextColumn ()
                    ImGui.TextUnformatted "<parent>"
                    ImGui.NextColumn ()
                    ImGui.TextUnformatted (scstring lastWriteTime)
                    ImGui.NextColumn ()

                    index <- index + 1UL

                // Draw directories
                for directoryEntry in directories do
                    let mutable directoryPath = directoryEntry
                    let mutable directoryName = directoryEntry.Name
                    let mutable lastWriteTime = directoryEntry.LastWriteTime
                    let mutable size = "-"

                    if ImGui.Selectable (directoryName, dialogState.CurrentIndex = index, ImGuiSelectableFlags.AllowDoubleClick, v2 contentRegionWidth 0.0f) then
                        dialogState.CurrentIndex <- index

                        if ImGui.IsMouseDoubleClicked ImGuiMouseButton.Left then
                            dialogState.DirectoryPath <- directoryPath
                            dialogState.RefreshInfo <- true
                            sort (dialogState, true)
                    ImGui.NextColumn ()
                    ImGui.TextUnformatted size
                    ImGui.NextColumn ()
                    ImGui.TextUnformatted "<directory>"
                    ImGui.NextColumn ()
                    ImGui.TextUnformatted (scstring lastWriteTime)
                    ImGui.NextColumn ()

                    index <- inc index

                // Draw files
                for fileEntry in files do
                    let mutable filePath = fileEntry.FullName
                    let mutable fileName = fileEntry.Name
                    let mutable lastWriteTime = fileEntry.LastWriteTime
                    let mutable size = string fileEntry.Length

                    if ImGui.Selectable (fileName, dialogState.CurrentIndex = index, ImGuiSelectableFlags.AllowDoubleClick, v2 contentRegionWidth 0.0f) then
                        dialogState.CurrentIndex <- index
                        dialogState.FileName <- fileName
                    ImGui.NextColumn ()
                    ImGui.TextUnformatted size
                    ImGui.NextColumn ()
                    ImGui.TextUnformatted (Path.GetExtension filePath)
                    ImGui.NextColumn ()
                    ImGui.TextUnformatted (scstring lastWriteTime)
                    ImGui.NextColumn ()

                    index <- index + 1UL

                ImGui.EndChild ()

                // Draw filename
                let fileNameBufferSize = 200
                let mutable fileNameBuffer = ""
                let mutable fileNameStr = dialogState.FileName
                let mutable fileNameSize = fileNameStr.Length

                if fileNameSize >= fileNameBufferSize then
                    fileNameSize <- fileNameBufferSize - 1

                fileNameBuffer <- fileNameStr.Substring(0, fileNameSize)

                ImGui.PushItemWidth contentRegionWidth
                if ImGui.InputText ("File Name", &fileNameBuffer, uint fileNameBufferSize) then
                    dialogState.FileName <- fileNameBuffer
                    dialogState.CurrentIndex <- 0UL

                match dialogState.FileDialogType with
                | ImGuiFileDialogType.Open ->
                    if ImGui.Button "Open" || ImGui.IsKeyPressed ImGuiKey.Enter then

                        dialogState.ResultPath <- Path.Combine (dialogState.DirectoryPath.FullName, dialogState.FileName)

                        if File.Exists dialogState.ResultPath then
                            fileNameSortOrder <- ImGuiFileSortOrder.Unsorted
                            sizeSortOrder <- ImGuiFileSortOrder.Unsorted
                            typeSortOrder <- ImGuiFileSortOrder.Unsorted
                            dateSortOrder <- ImGuiFileSortOrder.Unsorted

                            dialogState.RefreshInfo <- false
                            dialogState.CurrentDirectories <- []
                            dialogState.CurrentFiles <- []
                            dialogState.CurrentIndex <- 0UL

                            complete <- true
                            opened <- false

                | ImGuiFileDialogType.Save ->
                    if ImGui.Button "Save" || ImGui.IsKeyPressed ImGuiKey.Enter then

                        dialogState.ResultPath <- Path.Combine (dialogState.DirectoryPath.FullName, dialogState.FileName)

                        if dialogState.DirectoryPath.Exists && String.notEmpty (dialogState.FileName.Trim ()) then
                            fileNameSortOrder <- ImGuiFileSortOrder.Unsorted
                            sizeSortOrder <- ImGuiFileSortOrder.Unsorted
                            typeSortOrder <- ImGuiFileSortOrder.Unsorted
                            dateSortOrder <- ImGuiFileSortOrder.Unsorted

                            dialogState.RefreshInfo <- false
                            dialogState.CurrentDirectories <- []
                            dialogState.CurrentFiles <- []
                            dialogState.CurrentIndex <- 0UL

                            complete <- true
                            opened <- false

                | _ -> ()

                ImGui.SameLine()

                if ImGui.Button "Cancel" || ImGui.IsKeyPressed ImGuiKey.Escape then
                    fileNameSortOrder <- ImGuiFileSortOrder.Unsorted
                    sizeSortOrder <- ImGuiFileSortOrder.Unsorted
                    typeSortOrder <- ImGuiFileSortOrder.Unsorted
                    dateSortOrder <- ImGuiFileSortOrder.Unsorted

                    dialogState.RefreshInfo <- false
                    dialogState.CurrentDirectories <- []
                    dialogState.CurrentFiles <- []
                    dialogState.CurrentIndex <- 0UL

                    opened <- false

            ImGui.EndPopup ()
            ImGui.PopID ()

            complete

        else false