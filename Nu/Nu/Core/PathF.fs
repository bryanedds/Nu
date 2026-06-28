// Nu Game Engine.
// Required Notice:
// Copyright (C) Bryan Edds.
// Nu Game Engine is licensed under the Nu Game Engine Noncommercial License.
// See https://github.com/bryanedds/Nu/blob/master/License.md.

namespace Nu
open System
open System.IO

/// Performs operations on System.String instances that contain file or directory path information. These operations
/// are performed in a normalized manner where '/' is always used as the directory separator.
type PathF private () =

    /// Normalize a path to use '/' separators.
    static member Normalize (path : string) =
        path.Replace ('\\', '/')

    /// Denormalize a path to use '\\' separators.
    static member Denormalize (path : string) =
        path.Replace ('/', '\\')

    /// Combines two strings into a path.
    static member Combine ([<ParamArray>] paths : string[]) =
        PathF.Normalize (Path.Combine paths)

    /// Gets an array containing the characters that are not allowed in path names.
    static member GetInvalidPathChars () =
        Path.GetInvalidPathChars ()

    /// Returns the file name and extension of the specified path string.
    static member GetFileName (path : string) =
        Path.GetFileName path

    /// Returns the file name of the specified path string without the extension.
    static member GetFileNameWithoutExtension (path : string) =
        Path.GetFileNameWithoutExtension path

    /// Returns the directory information for the specified path.
    static member GetDirectoryName (path : string) =
        match Path.GetDirectoryName path with
        | null -> null
        | dirName -> PathF.Normalize dirName

    /// Returns the absolute path for the specified path string.
    static member GetFullPath (path : string) =
        PathF.Normalize (Path.GetFullPath path)

    /// Returns a relative path from one path to another.
    static member GetRelativePath (relativeTo : string, path : string) =
        PathF.Normalize (Path.GetRelativePath (relativeTo, path))

    /// Determines whether a path includes a file name extension.
    static member HasExtension (path : string) =
        Path.HasExtension path

    /// Returns the lower-cased extension (including the period ".") of the specified path string.
    static member GetExtensionLower (path : string) =
        match Path.GetExtension path with
        | null -> null
        | dirName -> dirName.ToLowerInvariant ()

    /// Returns the upper-cased extension (including the period ".") of the specified path string.
    static member GetExtensionUpper (path : string) =
        match Path.GetExtension path with
        | null -> null
        | dirName -> dirName.ToUpperInvariant ()

    /// Returns the mixed-case (unaltered) extension (including the period ".") of the specified path string.
    static member GetExtensionMixed (path : string) =
        Path.GetExtension path

    /// Changes the extension of a path string.
    static member ChangeExtension (path : string, extension : string) =
        Path.ChangeExtension (path, extension)