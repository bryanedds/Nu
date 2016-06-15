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
module ScriptingEnvModule =

    /// An environemt in which scripting declarations can be found.
    /// Internally a Dictionary for fast look-ups, but without add functions for immutability. Append, however, stays
    /// functional without greater big-O compexity!
    type [<NoEquality; NoComparison>] ScriptingEnv =
        private
            ScriptingEnv of Dictionary<string, Declaration>

    let tryFind key (ScriptingEnv env) =
        Dictionary.tryFind key env

    let make declarations =
        ScriptingEnv ^ dictC declarations

    let append (ScriptingEnv env) (ScriptingEnv env2) =
        make (Seq.append env env2)

[<AutoOpen>]
module ScriptingSystemModule =

    /// An abstract data type for executing scripts.
    type [<NoEquality; NoComparison>] ScriptingSystem =
        private
            { ScriptingEnvs : Vmap<obj Address, ScriptingEnv>
              Dispatchers : Dispatcher list
              Handlers : Event list
              Equalities : string option * Referent * Expr
              Variables : Value list
              Debugging : bool }