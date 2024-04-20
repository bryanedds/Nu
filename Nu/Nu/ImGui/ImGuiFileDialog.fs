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
    member val FilePattern : string = "*" with get, set
    member val FileDialogType : ImGuiFileDialogType = ImGuiFileDialogType.Open with get, set
    member val FileName : string = "" with get, set
    member val DirectoryPath : DirectoryInfo = DirectoryInfo (if String.notEmpty directoryPath then directoryPath else ".") with get, set
    member val ResultPath : string = "" with get, set
    member val RefreshInfo : bool = false with get, set
    member val CurrentDirectories : list<DirectoryInfo> = [] with get, set
    member val CurrentFiles : list<FileInfo> = [] with get, set
    member val CurrentIndex : UInt64 = 0UL with get, set
    member this.FilePath
        with get () = PathF.Normalize this.DirectoryPath.FullName + "/" + this.FileName
        and set (value : string) =
            this.FileName <- PathF.GetFileName value
            this.DirectoryPath <- DirectoryInfo (PathF.GetDirectoryName value)

[<RequireQualifiedAccess>]
module ImGui =

    let mutable private SpacingColumn0 = 230.0f
    let mutable private SpacingColumn1 = 80.0f
    let mutable private SpacingColumn2 = 90.0f
    let mutable private FileNameSortOrder = ImGuiFileSortOrder.Unsorted
    let mutable private FileNameSortOrderCopy = ImGuiFileSortOrder.Unsorted
    let mutable private SizeSortOrder = ImGuiFileSortOrder.Unsorted
    let mutable private SizeSortOrderCopy = ImGuiFileSortOrder.Unsorted
    let mutable private DateSortOrder = ImGuiFileSortOrder.Unsorted
    let mutable private DateSortOrderCopy = ImGuiFileSortOrder.Unsorted
    let mutable private TypeSortOrder = ImGuiFileSortOrder.Unsorted
    let mutable private TypeSortOrderCopy = ImGuiFileSortOrder.Unsorted

    let private refreshInfo (dialogInfo : ImGuiFileDialogState) =
        dialogInfo.RefreshInfo <- false
        dialogInfo.CurrentDirectories <- []
        dialogInfo.CurrentFiles <- []
        dialogInfo.CurrentIndex <- 0UL

        let directory = DirectoryInfo dialogInfo.DirectoryPath.FullName
        dialogInfo.CurrentDirectories <- directory.GetDirectories () |> Seq.toList
        dialogInfo.CurrentFiles <- directory.GetFiles (dialogInfo.FilePattern) |> Seq.toList

    let private sort (dialogState : ImGuiFileDialogState, forceSort : bool) =
        let mutable sort = false

        if  FileNameSortOrderCopy <> FileNameSortOrder then
            FileNameSortOrderCopy <- FileNameSortOrder
            sort <- true

        if  SizeSortOrderCopy <> SizeSortOrder then
            SizeSortOrderCopy <- SizeSortOrder
            sort <- true

        if  DateSortOrderCopy <> DateSortOrder then
            DateSortOrderCopy <- DateSortOrder
            sort <- true

        if  TypeSortOrderCopy <> TypeSortOrder then
            TypeSortOrderCopy <- TypeSortOrder
            sort <- true

        if sort || forceSort then

            // Sort directories
            if FileNameSortOrder <> ImGuiFileSortOrder.Unsorted || SizeSortOrder <> ImGuiFileSortOrder.Unsorted || TypeSortOrder <> ImGuiFileSortOrder.Unsorted then
                dialogState.CurrentDirectories <-
                    if FileNameSortOrder = ImGuiFileSortOrder.Descending
                    then dialogState.CurrentDirectories |> List.sortBy (fun i -> i.Name)
                    else dialogState.CurrentDirectories |> List.sortByDescending (fun i -> i.Name)
            elif DateSortOrder <> ImGuiFileSortOrder.Unsorted then
                dialogState.CurrentDirectories <-
                    if DateSortOrder = ImGuiFileSortOrder.Descending
                    then dialogState.CurrentDirectories |> List.sortBy (fun i -> i.LastWriteTime)
                    else dialogState.CurrentDirectories |> List.sortByDescending (fun i -> i.LastWriteTime)

            // Sort files
            if FileNameSortOrder <> ImGuiFileSortOrder.Unsorted then
                dialogState.CurrentFiles <-
                    if FileNameSortOrder = ImGuiFileSortOrder.Descending
                    then dialogState.CurrentFiles |> List.sortBy (fun i -> i.Name)
                    else dialogState.CurrentFiles |> List.sortByDescending (fun i -> i.Name)
            elif SizeSortOrder <> ImGuiFileSortOrder.Unsorted then
                dialogState.CurrentFiles <-
                    if SizeSortOrder = ImGuiFileSortOrder.Descending
                    then dialogState.CurrentFiles |> List.sortBy (fun i -> i.Length)
                    else dialogState.CurrentFiles |> List.sortByDescending (fun i -> i.Length)
            elif TypeSortOrder <> ImGuiFileSortOrder.Unsorted then
                dialogState.CurrentFiles <-
                    if TypeSortOrder = ImGuiFileSortOrder.Descending
                    then dialogState.CurrentFiles |> List.sortBy (fun i -> i.Extension)
                    else dialogState.CurrentFiles |> List.sortByDescending (fun i -> i.Extension)
            elif DateSortOrder <> ImGuiFileSortOrder.Unsorted then
                dialogState.CurrentFiles <-
                    if DateSortOrder = ImGuiFileSortOrder.Descending
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
                ImGui.Text ("Path: " + PathF.Normalize dialogState.DirectoryPath.FullName)

                let contentRegionWidth = ImGui.GetWindowContentRegionMax().X - ImGui.GetWindowContentRegionMin().X

                ImGui.BeginChild ("##browser", v2 contentRegionWidth 300.0f, true, ImGuiWindowFlags.HorizontalScrollbar) |> ignore<bool>
                ImGui.Columns 4

                // Columns size
                if SpacingColumn0 > 0.0f then
                    ImGui.SetColumnWidth (0, SpacingColumn0)
                    SpacingColumn0 <- 0.0f

                if SpacingColumn1 > 0.0f then
                    ImGui.SetColumnWidth (1, SpacingColumn1)
                    SpacingColumn1 <- 0.0f

                if SpacingColumn2 > 0.0f then
                    ImGui.SetColumnWidth (2, SpacingColumn2)
                    SpacingColumn2 <- 0.0f

                // File Columns
                if ImGui.Selectable "Name" then
                    SizeSortOrder <- ImGuiFileSortOrder.Unsorted
                    DateSortOrder <- ImGuiFileSortOrder.Unsorted
                    TypeSortOrder <- ImGuiFileSortOrder.Unsorted
                    FileNameSortOrder <- if FileNameSortOrder = ImGuiFileSortOrder.Descending then ImGuiFileSortOrder.Ascending else ImGuiFileSortOrder.Descending
                    FileNameSortOrderCopy <- FileNameSortOrder
                    sort (dialogState, true)
                ImGui.NextColumn ()

                if ImGui.Selectable "Size" then
                    FileNameSortOrder <- ImGuiFileSortOrder.Unsorted
                    DateSortOrder <- ImGuiFileSortOrder.Unsorted
                    TypeSortOrder <- ImGuiFileSortOrder.Unsorted
                    SizeSortOrder <- if SizeSortOrder = ImGuiFileSortOrder.Descending then ImGuiFileSortOrder.Ascending else ImGuiFileSortOrder.Descending
                    SizeSortOrderCopy <- SizeSortOrder
                    sort (dialogState, true)
                ImGui.NextColumn()

                if ImGui.Selectable "Type" then
                    FileNameSortOrder <- ImGuiFileSortOrder.Unsorted
                    DateSortOrder <- ImGuiFileSortOrder.Unsorted
                    SizeSortOrder <- ImGuiFileSortOrder.Unsorted
                    TypeSortOrder <- if TypeSortOrder = ImGuiFileSortOrder.Descending then ImGuiFileSortOrder.Ascending else ImGuiFileSortOrder.Descending
                    TypeSortOrderCopy <- TypeSortOrder
                    sort (dialogState, true)
                ImGui.NextColumn()

                if ImGui.Selectable "Date" then
                    FileNameSortOrder <- ImGuiFileSortOrder.Unsorted
                    SizeSortOrder <- ImGuiFileSortOrder.Unsorted
                    TypeSortOrder <- ImGuiFileSortOrder.Unsorted
                    DateSortOrder <- if DateSortOrder = ImGuiFileSortOrder.Descending then ImGuiFileSortOrder.Ascending else ImGuiFileSortOrder.Descending
                    DateSortOrderCopy <- DateSortOrder
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
                let mutable filePicked = false
                for fileEntry in files do
                    let mutable filePath = fileEntry.FullName
                    let mutable fileName = fileEntry.Name
                    let mutable lastWriteTime = fileEntry.LastWriteTime
                    let mutable size = string fileEntry.Length

                    if ImGui.Selectable (fileName, dialogState.CurrentIndex = index, ImGuiSelectableFlags.AllowDoubleClick, v2 contentRegionWidth 0.0f) then
                        dialogState.CurrentIndex <- index
                        dialogState.FileName <- fileName

                        if ImGui.IsMouseDoubleClicked ImGuiMouseButton.Left then
                            filePicked <- true

                    ImGui.NextColumn ()
                    ImGui.TextUnformatted size
                    ImGui.NextColumn ()
                    ImGui.TextUnformatted (PathF.GetExtensionMixed filePath)
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
                    if ImGui.Button "Open" || ImGui.IsKeyPressed ImGuiKey.Enter || filePicked then

                        dialogState.ResultPath <- PathF.Combine (dialogState.DirectoryPath.FullName, dialogState.FileName)

                        if File.Exists dialogState.ResultPath then
                            FileNameSortOrder <- ImGuiFileSortOrder.Unsorted
                            SizeSortOrder <- ImGuiFileSortOrder.Unsorted
                            TypeSortOrder <- ImGuiFileSortOrder.Unsorted
                            DateSortOrder <- ImGuiFileSortOrder.Unsorted

                            dialogState.RefreshInfo <- false
                            dialogState.CurrentDirectories <- []
                            dialogState.CurrentFiles <- []
                            dialogState.CurrentIndex <- 0UL

                            complete <- true
                            opened <- false

                | ImGuiFileDialogType.Save ->
                    if ImGui.Button "Save" || ImGui.IsKeyPressed ImGuiKey.Enter || filePicked then

                        dialogState.ResultPath <- PathF.Combine (dialogState.DirectoryPath.FullName, dialogState.FileName)

                        if dialogState.DirectoryPath.Exists && String.notEmpty (dialogState.FileName.Trim ()) then
                            FileNameSortOrder <- ImGuiFileSortOrder.Unsorted
                            SizeSortOrder <- ImGuiFileSortOrder.Unsorted
                            TypeSortOrder <- ImGuiFileSortOrder.Unsorted
                            DateSortOrder <- ImGuiFileSortOrder.Unsorted

                            dialogState.RefreshInfo <- false
                            dialogState.CurrentDirectories <- []
                            dialogState.CurrentFiles <- []
                            dialogState.CurrentIndex <- 0UL

                            complete <- true
                            opened <- false

                | _ -> ()

                ImGui.SameLine()

                if ImGui.Button "Cancel" || ImGui.IsKeyPressed ImGuiKey.Escape then
                    FileNameSortOrder <- ImGuiFileSortOrder.Unsorted
                    SizeSortOrder <- ImGuiFileSortOrder.Unsorted
                    TypeSortOrder <- ImGuiFileSortOrder.Unsorted
                    DateSortOrder <- ImGuiFileSortOrder.Unsorted

                    dialogState.RefreshInfo <- false
                    dialogState.CurrentDirectories <- []
                    dialogState.CurrentFiles <- []
                    dialogState.CurrentIndex <- 0UL

                    opened <- false

            ImGui.EndPopup ()
            ImGui.PopID ()

            complete

        else false