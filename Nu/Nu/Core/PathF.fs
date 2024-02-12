// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu
open System
open System.IO

/// Performs operations on System.String instances that contain file or directory path information. These operations
/// are performed in a normalized manner where '/' is always used as the directory seperator.
[<RequireQualifiedAccess>]
module PathF =

    /// Normalize a path to use '/' separators.
    let Normalize (path : string) =
        path.Replace ('\\', '/')

    /// Denormalize a path to use '\\' separators.
    let Denormalize (path : string) =
        path.Replace ('/', '\\')

    /// Combines two strings into a path.
    let Combine (left, right) =
        Normalize (Path.Combine (left, right))

    /// Gets an array containing the characters that are not allowed in path names.
    let GetInvalidPathChars () =
        Path.GetInvalidPathChars ()

    /// Returns the file name and extension of the specified path string.
    let GetFileName (path : string) =
        Path.GetFileName path

    /// Returns the file name of the specified path string without the extension.
    let GetFileNameWithoutExtension (path : string) =
        Path.GetFileNameWithoutExtension path

    /// Returns the directory information for the specified path.
    let GetDirectoryName (path : string) =
        match Path.GetDirectoryName path with
        | null -> null
        | dirName -> Normalize dirName

    /// Returns the absolute path for the specified path string.
    let GetFullPath (path : string) =
        Normalize (Path.GetFullPath path)

    /// Returns a relative path from one path to another.
    let GetRelativePath (relativeTo : string, path : string) =
        Normalize (Path.GetRelativePath (relativeTo, path))

    /// Determines whether a path includes a file name extension.
    let HasExtension (path : string) =
        Path.HasExtension path

    /// Returns the lower-cased extension (including the period ".") of the specified path string.
    let GetExtensionLower (path : string) =
        match Path.GetExtension path with
        | null -> null
        | dirName -> dirName.ToLowerInvariant ()

    /// Returns the upper-cased extension (including the period ".") of the specified path string.
    let GetExtensionUpper (path : string) =
        match Path.GetExtension path with
        | null -> null
        | dirName -> dirName.ToUpperInvariant ()

    /// Returns the mixed-case (unaltered) extension (including the period ".") of the specified path string.
    let GetExtensionMixed (path : string) =
        Path.GetExtension path

    /// Changes the extension of a path string.
    let ChangeExtension (path : string, extension : string) =
        Path.ChangeExtension (path, extension)