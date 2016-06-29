// Nu Game Engine.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Nu
open System
open System.Collections.Generic
open System.Runtime.InteropServices
open OpenTK
open Prime
open Nu
open Nu.Scripting

[<AutoOpen>]
module ScriptEnvModule =

    /// An environemt in which scripting declarations can be found.
    /// Internally a Dictionary for fast look-ups, but without add functions for immutability. Append, however, stays
    /// functional without greater big-O compexity!
    type [<NoEquality; NoComparison>] ScriptEnv =
        private
            ScriptEnv of Dictionary<string, Declaration>
            
    [<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
    module ScriptEnv =

        let tryFind key (ScriptEnv env) =
            Dictionary.tryFind key env

        let make declarations =
            ScriptEnv ^ dictC declarations

        let append (ScriptEnv env) (ScriptEnv env2) =
            make (Seq.append env env2)

[<AutoOpen>]
module ScriptSystemModule =

    /// An abstract data type for executing scripts.
    type [<NoEquality; NoComparison>] ScriptSystem =
        private
            { ScriptEnvs : Vmap<obj Address, ScriptEnv>
              Scripts : Vmap<Guid, Script>
              Debugging : bool }