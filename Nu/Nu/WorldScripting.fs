// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2018.

namespace Nu
open System
open System.Collections.Generic
open Prime
open Prime.Scripting
open Nu
#nowarn "22"
#nowarn "40"

/// The Stream value that can be plugged into the scripting language.
type [<CustomEquality; CustomComparison>] StreamPluggable =
    { Stream : Stream<obj, World> }

    static member equals _ _ = false

    static member compare _ _ = -1

    override this.GetHashCode () = 0

    override this.Equals that =
        match that with
        | :? StreamPluggable as that -> StreamPluggable.equals this that
        | _ -> failwithumf ()

    interface StreamPluggable IComparable with
        member this.CompareTo that =
            StreamPluggable.compare this that

    interface Scripting.Pluggable with

        member this.CompareTo that =
            match that with
            | :? StreamPluggable as that -> (this :> StreamPluggable IComparable).CompareTo that
            | _ -> failwithumf ()

        member this.TypeName =
            "Stream"

        member this.FSharpType =
            getType this.Stream

        member this.ToSymbol () =
            Symbols ([], None)

[<AutoOpen>]
module WorldScripting =

    let mutable Extrinsics =
        Unchecked.defaultof<Dictionary<string, World ScriptingTrinsic>>

    let mutable Bindings =
        Unchecked.defaultof<Dictionary<string, World ScriptingTrinsic>>

    type World with

        static member internal evalInternal expr world : struct (Expr * World) =
            ScriptingSystem.eval expr world

        static member internal evalManyInternal exprs world : struct (Expr array * World) =
            ScriptingSystem.evalMany exprs world

        static member internal evalWithLoggingInternal expr world : struct (Expr * World) =
            ScriptingSystem.evalWithLogging expr world

        static member internal evalManyWithLoggingInternal exprs world : struct (Expr array * World) =
            ScriptingSystem.evalManyWithLogging exprs world

        static member internal evalSimulantExists fnName evaledArg originOpt world =
            match evaledArg with
            | Violation _ as error -> struct (error, world)
            | String str
            | Keyword str ->
                let context = World.getScriptContext world
                let relation = Relation.makeFromString str
                let address = Relation.resolve context.SimulantAddress relation
                match World.tryDerive address with
                | Some simulant -> struct (Bool (World.getExists simulant world), world)
                | None -> struct (Bool false, world)
            | _ -> struct (Violation (["InvalidArgumentType"; "SimulantExists"], "Function '" + fnName + "' requires 1 Relation argument.", originOpt), world)

        static member internal tryResolveRelation fnName expr originOpt (context : Simulant) world =
            match World.evalInternal expr world with
            | struct (Violation _, _) as error -> Left error
            | struct (String str, world)
            | struct (Keyword str, world) ->
                let relation = Relation.makeFromString str
                let address = Relation.resolve context.SimulantAddress relation
                match World.tryDerive address with
                | Some simulant -> Right struct (simulant, world)
                | None -> Left struct (Violation (["InvalidPropertyRelation"; String.capitalize fnName], "Relation must have 0 to 3 names.", originOpt), world)
            | struct (_, world) -> Left struct (Violation (["InvalidPropertyRelation"; String.capitalize fnName], "Relation must be either a String or Keyword.", originOpt), world)

        static member internal tryResolveRelationOpt fnName relationOpt originOpt context world =
            match relationOpt with
            | Some relationExpr -> World.tryResolveRelation fnName relationExpr originOpt context world
            | None -> Right struct (context, world)

        static member internal tryImportEvent evt world =
            match ScriptingSystem.tryImport (getType evt.Data) evt.Data world with
            | Some dataImported ->
                let evtRecord =
                    Record
                        ("Event",
                         [|"Data", 0; "Subscriber", 1; "Publisher", 2; "Address", 3|] |> Map.ofArray,
                         [|dataImported
                           String (scstring evt.Subscriber)
                           String (scstring evt.Publisher)
                           String (scstring evt.Address)|])
                Some evtRecord
            | None -> None

        static member internal evalGet fnName propertyName relationOpt originOpt world =
            let context = World.getScriptContext world
            match World.tryResolveRelationOpt fnName relationOpt originOpt context world with
            | Right struct (simulant, world) ->
                match World.tryGetProperty propertyName simulant world with
                | Some property ->
                    match property.PropertyValue with
                    | :? DesignerProperty as dp ->
                        match ScriptingSystem.tryImport dp.DesignerType dp.DesignerValue world with
                        | Some propertyValue -> struct (propertyValue, world)
                        | None -> struct (Violation (["InvalidPropertyValue"; String.capitalize fnName], "Property value could not be imported into scripting environment.", originOpt), world)
                    | _ ->
                        match ScriptingSystem.tryImport property.PropertyType property.PropertyValue world with
                        | Some propertyValue -> struct (propertyValue, world)
                        | None -> struct (Violation (["InvalidPropertyValue"; String.capitalize fnName], "Property value could not be imported into scripting environment.", originOpt), world)
                | None -> struct (Violation (["InvalidProperty"; String.capitalize fnName], "Simulant or property value could not be found.", originOpt), world)
            | Left error -> error

        static member internal evalGetAsStream fnName propertyName relationOpt originOpt world =
            let context = World.getScriptContext world
            match World.tryResolveRelationOpt fnName relationOpt originOpt context world with
            | Right struct (simulant, world) ->
                let stream =
                    Stream.make (Events.Change propertyName --> simulant.SimulantAddress) |>
                    Stream.mapEvent (fun (evt : Event<ChangeData, _>) world ->
                        match World.tryGetProperty evt.Data.Name simulant world with
                        | Some property ->
                            match property.PropertyValue with
                            | :? DesignerProperty as dp -> ScriptingSystem.tryImport dp.DesignerType dp.DesignerValue world |> box
                            | _ -> ScriptingSystem.tryImport property.PropertyType property.PropertyValue world |> box
                        | None -> None |> box)
                struct (Pluggable { Stream = stream }, world)
            | Left error -> error

        static member internal evalSet fnName propertyName relationOpt propertyValueExpr originOpt world =
            let context = World.getScriptContext world
            match World.tryResolveRelationOpt fnName relationOpt originOpt context world with
            | Right struct (simulant, world) ->
                match World.tryGetProperty propertyName simulant world with
                | Some property ->
                    let struct (propertyValue, world) = World.evalInternal propertyValueExpr world
                    let alwaysPublish = Reflection.isPropertyAlwaysPublishByName propertyName
                    let nonPersistent = not (Reflection.isPropertyPersistentByName propertyName)
                    match property.PropertyValue with
                    | :? DesignerProperty as dp ->
                        match ScriptingSystem.tryExport dp.DesignerType propertyValue world with
                        | Some propertyValue ->
                            let propertyValue = { dp with DesignerValue = propertyValue }
                            let property = { PropertyType = property.PropertyType; PropertyValue = propertyValue }
                            match World.trySetProperty propertyName alwaysPublish nonPersistent property simulant world with
                            | (true, world) -> struct (Unit, world)
                            | (false, world) -> struct (Violation (["InvalidProperty"; String.capitalize fnName], "Property value could not be set.", originOpt), world)
                        | None -> struct (Violation (["InvalidPropertyValue"; String.capitalize fnName], "Property value could not be exported into Simulant property.", originOpt), world)
                    | _ ->
                        match ScriptingSystem.tryExport property.PropertyType propertyValue world with
                        | Some propertyValue ->
                            let property = { PropertyType = property.PropertyType; PropertyValue = propertyValue }
                            match World.trySetProperty propertyName alwaysPublish nonPersistent property simulant world with
                            | (true, world) -> struct (Unit, world)
                            | (false, world) -> struct (Violation (["InvalidProperty"; String.capitalize fnName], "Property value could not be set.", originOpt), world)
                        | None -> struct (Violation (["InvalidPropertyValue"; String.capitalize fnName], "Property value could not be exported into Simulant property.", originOpt), world)
                | None -> struct (Violation (["InvalidProperty"; String.capitalize fnName], "Property value could not be set.", originOpt), world)
            | Left error -> error

        static member internal evalSetAsStream fnName propertyName relationOpt stream originOpt world =
            let context = World.getScriptContext world
            match World.tryResolveRelationOpt fnName relationOpt originOpt context world with
            | Right struct (simulant, world) ->
                match stream with
                | Pluggable stream ->
                    match stream with
                    | :? StreamPluggable as stream ->
                        let (unsubscribe, world) =
                            Stream.monitorEffect (fun (evt : Event<obj, _>) world ->
                                match evt.Data with
                                | :? (Expr option) as exprOpt ->
                                    match exprOpt with
                                    | Some expr ->
                                        match World.tryGetProperty propertyName simulant world with
                                        | Some property ->
                                            let alwaysPublish = Reflection.isPropertyAlwaysPublishByName propertyName
                                            let nonPersistent = not (Reflection.isPropertyPersistentByName propertyName)
                                            match property.PropertyValue with
                                            | :? DesignerProperty as dp ->
                                                match ScriptingSystem.tryExport dp.DesignerType expr world with
                                                | Some propertyValue ->
                                                    let propertyValue = { dp with DesignerValue = propertyValue }
                                                    let property = { PropertyType = property.PropertyType; PropertyValue = propertyValue }
                                                    let (_, world) = World.trySetProperty propertyName alwaysPublish nonPersistent property simulant world
                                                    (Cascade, world)
                                                | None -> (Cascade, world)
                                            | _ ->
                                                match ScriptingSystem.tryExport property.PropertyType expr world with
                                                | Some propertyValue ->
                                                    let property = { PropertyType = property.PropertyType; PropertyValue = propertyValue }
                                                    let (_, world) = World.trySetProperty propertyName alwaysPublish nonPersistent property simulant world
                                                    (Cascade, world)
                                                | None -> (Cascade, world)
                                        | None -> (Cascade, world)
                                    | None -> (Cascade, world)
                                | _ -> (Cascade, world))
                                (simulant :> Participant)
                                stream.Stream
                                world
                        let world = WorldModule.addSimulantScriptUnsubscription unsubscribe simulant world
                        struct (Unit, world)
                    | _ -> struct (Violation (["InvalidArgumentType"; String.capitalize fnName], "Function '" + fnName + "' requires a Stream for its last argument.", originOpt), world)
                | _ -> struct (Violation (["InvalidArgumentType"; String.capitalize fnName], "Function '" + fnName + "' requires a Stream for its last argument.", originOpt), world)
            | Left error -> error

        static member internal evalStreamEvent fnName evaledArg originOpt world =
            match evaledArg with
            | Violation _ as error -> struct (error, world)
            | String str
            | Keyword str ->
                let scriptContext = World.getScriptContext world
                let eventRelation = Relation.makeFromString str
                let eventAddress = Relation.resolve scriptContext.SimulantAddress eventRelation
                let stream =
                    Stream.make eventAddress |>
                    Stream.map box |>
                    Stream.mapEffect (fun (evt : Event<obj, _>) world ->
                        match World.tryImportEvent evt world with
                        | Some evtImported -> (Some evtImported |> box, world)
                        | None -> (None |> box, world))
                struct (Pluggable { Stream = stream }, world)
            | _ -> struct (Violation (["InvalidArgumentType"; String.capitalize fnName], "Function '" + fnName + "' requires a Function for its 1st argument.", originOpt), world)

        static member internal evalMapStream fnName fn stream originOpt world =
            match stream with
            | Pluggable stream ->
                match stream with
                | :? StreamPluggable as stream ->
                    let stream =
                        Stream.mapEffect
                            (fun (evt : Event<obj, _>) world ->
                                match evt.Data with
                                | :? (Expr option) as exprOpt ->
                                    match exprOpt with
                                    | Some expr ->
                                        let context = World.getScriptContext world
                                        match World.tryGetScriptFrame context world with
                                        | Some scriptFrame ->
                                            let breakpoint = { BreakEnabled = false; BreakCondition = Unit }
                                            let application = Apply ([|fn; expr|], breakpoint, originOpt)
                                            let struct (evaled, world) = World.evalWithLogging application scriptFrame context world
                                            (Some evaled |> box, world)
                                        | None -> (None |> box, world)
                                    | None -> (None |> box, world)
                                | _ -> (None |> box, world))
                            stream.Stream
                    struct (Pluggable { Stream = stream }, world)
                | _ -> struct (Violation (["InvalidArgumentType"; String.capitalize fnName], "Function '" + fnName + "' requires a Stream for its 2nd argument.", originOpt), world)
            | _ -> struct (Violation (["InvalidArgumentType"; String.capitalize fnName], "Function '" + fnName + "' requires a Stream for its 2nd argument.", originOpt), world)

        static member internal evalFoldStream fnName fn state stream originOpt world =
            match stream with
            | Pluggable stream ->
                match stream with
                | :? StreamPluggable as stream ->
                    let stream =
                        Stream.foldEffect
                            (fun (stateOptObj : obj) (evt : Event<obj, _>) world ->
                                match (stateOptObj, evt.Data) with
                                | ((:? (Expr option) as stateOpt), (:? (Expr option) as exprOpt)) ->
                                    match exprOpt with
                                    | Some expr ->
                                        let context = World.getScriptContext world
                                        match World.tryGetScriptFrame context world with
                                        | Some scriptFrame ->
                                            let breakpoint = { BreakEnabled = false; BreakCondition = Unit }
                                            let application = Apply ([|fn; Option.get stateOpt; expr|], breakpoint, originOpt)
                                            let struct (evaled, world) = World.evalWithLogging application scriptFrame context world
                                            (Some evaled |> box, world)
                                        | None -> (stateOpt |> box, world)
                                    | None -> (stateOpt |> box, world)
                                | _ -> (stateOptObj, world))
                            (Some state |> box)
                            stream.Stream
                    struct (Pluggable { Stream = stream }, world)
                | _ -> struct (Violation (["InvalidArgumentType"; String.capitalize fnName], "Function '" + fnName + "' requires a Stream for its 2nd argument.", originOpt), world)
            | _ -> struct (Violation (["InvalidArgumentType"; String.capitalize fnName], "Function '" + fnName + "' requires a Stream for its 2nd argument.", originOpt), world)

        static member internal evalMap2Stream fnName fn stream stream2 originOpt world =
            match (stream, stream2) with
            | (Pluggable stream, Pluggable stream2) ->
                match (stream, stream2) with
                | ((:? StreamPluggable as stream), (:? StreamPluggable as stream2)) ->
                    let stream =
                        Stream.map2Effect
                            (fun (evt : Event<obj, _>) (evt2 : Event<obj, _>) world ->
                                match (evt.Data, evt2.Data) with
                                | ((:? (Expr option) as exprOpt), (:? (Expr option) as expr2Opt)) ->
                                    match (exprOpt, expr2Opt) with
                                    | (Some expr, Some expr2) ->
                                        let context = World.getScriptContext world
                                        match World.tryGetScriptFrame context world with
                                        | Some scriptFrame ->
                                            let breakpoint = { BreakEnabled = false; BreakCondition = Unit }
                                            let application = Apply ([|fn; expr; expr2|], breakpoint, originOpt)
                                            let struct (evaled, world) = World.evalWithLogging application scriptFrame context world
                                            (Some evaled |> box, world)
                                        | None -> (None |> box, world)
                                    | (_, _) -> (None |> box, world)
                                | (_, _) -> (None |> box, world))
                            stream.Stream
                            stream2.Stream
                    struct (Pluggable { Stream = stream }, world)
                | _ -> struct (Violation (["InvalidArgumentType"; String.capitalize fnName], "Function '" + fnName + "' requires a Stream for its 2nd and 3rd arguments.", originOpt), world)
            | _ -> struct (Violation (["InvalidArgumentType"; String.capitalize fnName], "Function '" + fnName + "' requires a Stream for its 2nd and 3rd arguments.", originOpt), world)

        static member internal evalProductStream fnName stream stream2 originOpt world =
            match (stream, stream2) with
            | (Pluggable stream, Pluggable stream2) ->
                match (stream, stream2) with
                | ((:? StreamPluggable as stream), (:? StreamPluggable as stream2)) ->
                    let stream =
                        Stream.map2Effect
                            (fun (evt : Event<obj, _>) (evt2 : Event<obj, _>) world ->
                                match (evt.Data, evt2.Data) with
                                | ((:? (Expr option) as exprOpt), (:? (Expr option) as expr2Opt)) ->
                                    match (exprOpt, expr2Opt) with
                                    | (Some expr, Some expr2) -> (Some (Tuple [|expr; expr2|]) |> box, world)
                                    | (_, _) -> (None |> box, world)
                                | (_, _) -> (None |> box, world))
                            stream.Stream
                            stream2.Stream
                    struct (Pluggable { Stream = stream }, world)
                | _ -> struct (Violation (["InvalidArgumentType"; String.capitalize fnName], "Function '" + fnName + "' requires a Stream for its 1st and 2nd arguments.", originOpt), world)
            | _ -> struct (Violation (["InvalidArgumentType"; String.capitalize fnName], "Function '" + fnName + "' requires a Stream for its 1st and 2nd arguments.", originOpt), world)

        static member internal evalSumStream fnName stream stream2 originOpt world =
            match (stream, stream2) with
            | (Pluggable stream, Pluggable stream2) ->
                match (stream, stream2) with
                | ((:? StreamPluggable as stream), (:? StreamPluggable as stream2)) ->
                    let stream =
                        Stream.sum stream.Stream stream2.Stream |>
                        Stream.map box |>
                        Stream.map (fun (eirObj : obj) ->
                            match eirObj with
                            | :? Either<Expr option, Expr option> as eir ->
                                match eir with
                                | Right opt -> match opt with Some expr -> Some (Either (Right expr)) |> box | None -> None |> box
                                | Left opt -> match opt with Some expr -> Some (Either (Left expr)) |> box | None -> None |> box
                            | _ -> None |> box)
                    struct (Pluggable { Stream = stream }, world)
                | _ -> struct (Violation (["InvalidArgumentType"; String.capitalize fnName], "Function '" + fnName + "' requires a Stream for its 1st and 2nd arguments.", originOpt), world)
            | _ -> struct (Violation (["InvalidArgumentType"; String.capitalize fnName], "Function '" + fnName + "' requires a Stream for its 1st and 2nd arguments.", originOpt), world)

        static member internal evalMonitor5 subscription (eventAddress : obj Address) subscriber world =
            World.subscribe (fun evt world ->
                match World.tryGetScriptFrame subscriber world with
                | Some scriptFrame ->
                    match World.tryImportEvent evt world with
                    | Some evtImported ->
                        let breakpoint = { BreakEnabled = false; BreakCondition = Unit }
                        let application = Apply ([|subscription; evtImported|], breakpoint, None)
                        World.evalWithLogging application scriptFrame subscriber world |> snd'
                    | None -> Log.info "Event data could not be imported into scripting environment."; world
                | None -> world)
                eventAddress
                (subscriber :> Participant)
                world

        static member internal evalMonitor fnName evaledArg evaledArg2 originOpt world =
            match evaledArg with
            | Violation _ as error -> struct (error, world)
            | Binding _
            | Fun _ ->
                match evaledArg2 with
                | Violation _ as error -> struct (error, world)
                | String str
                | Keyword str ->
                    let eventAddress = Address.makeFromString str
                    let context = World.getScriptContext world
                    let world = World.evalMonitor5 evaledArg eventAddress context world
                    struct (Unit, world)
                | _ -> struct (Violation (["InvalidArgumentType"; String.capitalize fnName], "Function '" + fnName + "' requires a Relation for its 2nd argument.", originOpt), world)
            | _ -> struct (Violation (["InvalidArgumentType"; String.capitalize fnName], "Function '" + fnName + "' requires a Function for its 1st argument.", originOpt), world)

        /// Attempt to evaluate the scripting prelude.
        static member tryEvalPrelude world =
            let oldLocalFrame = World.getLocalFrame world
            let oldScriptContext = World.getScriptContext world
            let globalFrame = World.getGlobalFrame world
            World.setLocalFrame globalFrame world
            match World.tryEvalScript Assets.PreludeFilePath world with
            | Right struct (scriptStr, _, world) ->
                World.setLocalFrame oldLocalFrame world
                let world = World.setScriptContext oldScriptContext world
                Right struct (scriptStr, world)
            | Left struct (error, world) ->
                World.setLocalFrame oldLocalFrame world
                let world = World.setScriptContext oldScriptContext world
                Left struct (error, world)

        static member internal evalV2Extrinsic fnName exprs originOpt world =
            match World.evalManyInternal exprs world with
            | struct ([|Violation _ as v; _|], world) -> struct (v, world)
            | struct ([|_; Violation _ as v|], world) -> struct (v, world)
            | struct ([|Single x; Single y|], world) -> struct (Pluggable { Vector2 = Vector2 (x, y) }, world)
            | struct ([|_; _|], world) -> struct (Violation (["InvalidArgumentType"; String.capitalize fnName], "Application of " + fnName + " requires a Single for the both arguments.", originOpt), world)
            | struct (_, world) -> struct (Violation (["InvalidArgumentCount"; String.capitalize fnName], "Incorrect number of arguments for '" + fnName + "'; 2 arguments required.", originOpt), world)

        static member internal evalIndexV2Extrinsic fnName exprs originOpt world =
            match World.evalManyInternal exprs world with
            | struct ([|Violation _ as v; _|], world) -> struct (v, world)
            | struct ([|_; Violation _ as v|], world) -> struct (v, world)
            | struct ([|Keyword indexer; Pluggable pluggable|], world) ->
                match pluggable with
                | :? Vector2Pluggable as v2 ->
                    match indexer with
                    | "X" -> struct (Single v2.Vector2.X, world)
                    | "Y" -> struct (Single v2.Vector2.Y, world)
                    | _ -> struct (Violation (["InvalidIndexer"; String.capitalize fnName], "Invalid indexer '" + indexer + "' for V2.", originOpt), world)
                | _ -> failwithumf ()
            | struct ([|_; _|], world) -> struct (Violation (["InvalidArgumentType"; String.capitalize fnName], "Application of " + fnName + " requires a V2 index.", originOpt), world)
            | struct (_, world) -> struct (Violation (["InvalidArgumentCount"; String.capitalize fnName], "Incorrect number of arguments for '" + fnName + "'; 2 arguments required.", originOpt), world)

        static member internal evalUpdateV2Extrinsic fnName exprs originOpt world =
            match World.evalManyInternal exprs world with
            | struct ([|Violation _ as v; _; _|], world) -> struct (v, world)
            | struct ([|_; Violation _ as v; _|], world) -> struct (v, world)
            | struct ([|_; _; Violation _ as v|], world) -> struct (v, world)
            | struct ([|Keyword indexer; Single s; Pluggable pluggable|], world) ->
                match pluggable with
                | :? Vector2Pluggable as v2 ->
                    match indexer with
                    | "X" -> struct (Pluggable { Vector2 = Vector2 (s, v2.Vector2.Y) }, world)
                    | "Y" -> struct (Pluggable { Vector2 = Vector2 (v2.Vector2.X, s) }, world)
                    | _ -> struct (Violation (["InvalidIndexer"; String.capitalize fnName], "Invalid indexer '" + indexer + "' for V2.", originOpt), world)
                | _ -> failwithumf ()
            | struct ([|_; _; _|], world) -> struct (Violation (["InvalidArgumentType"; String.capitalize fnName], "Application of " + fnName + " requires a V2 target, a keyword index of X or Y, and a Single value.", originOpt), world)
            | struct (_, world) -> struct (Violation (["InvalidArgumentCount"; String.capitalize fnName], "Incorrect number of arguments for '" + fnName + "'; 2 arguments required.", originOpt), world)

        static member internal evalV4Extrinsic fnName exprs originOpt world =
            match World.evalManyInternal exprs world with
            | struct ([|Violation _ as v; _; _; _|], world) -> struct (v, world)
            | struct ([|_; Violation _ as v; _; _|], world) -> struct (v, world)
            | struct ([|_; _; Violation _ as v; _|], world) -> struct (v, world)
            | struct ([|_; _; _; Violation _ as v|], world) -> struct (v, world)
            | struct ([|Single x; Single y; Single z; Single w|], world) -> struct (Pluggable { Vector4 = Vector4 (x, y, z, w) }, world)
            | struct ([|_; _; _; _|], world) -> struct (Violation (["InvalidArgumentType"; String.capitalize fnName], "Application of " + fnName + " requires a Single for the all arguments.", originOpt), world)
            | struct (_, world) -> struct (Violation (["InvalidArgumentCount"; String.capitalize fnName], "Incorrect number of arguments for '" + fnName + "'; 4 arguments required.", originOpt), world)

        static member internal evalIndexV4Extrinsic fnName exprs originOpt world =
            match World.evalManyInternal exprs world with
            | struct ([|Violation _ as v; _|], world) -> struct (v, world)
            | struct ([|_; Violation _ as v|], world) -> struct (v, world)
            | struct ([|Keyword indexer; Pluggable pluggable|], world) ->
                match pluggable with
                | :? Vector4Pluggable as v4 ->
                    match indexer with
                    | "X" -> struct (Single v4.Vector4.X, world)
                    | "Y" -> struct (Single v4.Vector4.Y, world)
                    | "Z" -> struct (Single v4.Vector4.Z, world)
                    | "W" -> struct (Single v4.Vector4.W, world)
                    | _ -> struct (Violation (["InvalidIndexer"; String.capitalize fnName], "Invalid indexer '" + indexer + "' for V4.", originOpt), world)
                | _ -> failwithumf ()
            | struct ([|_; _|], world) -> struct (Violation (["InvalidArgumentType"; String.capitalize fnName], "Application of " + fnName + " requires a V4 index.", originOpt), world)
            | struct (_, world) -> struct (Violation (["InvalidArgumentCount"; String.capitalize fnName], "Incorrect number of arguments for '" + fnName + "'; 2 arguments required.", originOpt), world)

        static member internal evalUpdateV4Extrinsic fnName exprs originOpt world =
            match World.evalManyInternal exprs world with
            | struct ([|Violation _ as v; _; _|], world) -> struct (v, world)
            | struct ([|_; Violation _ as v; _|], world) -> struct (v, world)
            | struct ([|_; _; Violation _ as v|], world) -> struct (v, world)
            | struct ([|Keyword indexer; Single s; Pluggable pluggable|], world) ->
                match pluggable with
                | :? Vector4Pluggable as v4 ->
                    match indexer with
                    | "X" -> struct (Pluggable { Vector4 = Vector4 (s, v4.Vector4.Y, v4.Vector4.Z, v4.Vector4.W) }, world)
                    | "Y" -> struct (Pluggable { Vector4 = Vector4 (v4.Vector4.X, s, v4.Vector4.Z, v4.Vector4.W) }, world)
                    | "Z" -> struct (Pluggable { Vector4 = Vector4 (v4.Vector4.X, v4.Vector4.Y, s, v4.Vector4.W) }, world)
                    | "W" -> struct (Pluggable { Vector4 = Vector4 (v4.Vector4.X, v4.Vector4.Y, v4.Vector4.Z, s) }, world)
                    | _ -> struct (Violation (["InvalidIndexer"; String.capitalize fnName], "Invalid indexer '" + indexer + "' for V4.", originOpt), world)
                | _ -> failwithumf ()
            | struct ([|_; _; _|], world) -> struct (Violation (["InvalidArgumentType"; String.capitalize fnName], "Application of " + fnName + " requires a V4 target, a keyword index of X or Y, and an Int value.", originOpt), world)
            | struct (_, world) -> struct (Violation (["InvalidArgumentCount"; String.capitalize fnName], "Incorrect number of arguments for '" + fnName + "'; 2 arguments required.", originOpt), world)

        static member internal evalV2iExtrinsic fnName exprs originOpt world =
            match World.evalManyInternal exprs world with
            | struct ([|Violation _ as v; _|], world) -> struct (v, world)
            | struct ([|_; Violation _ as v|], world) -> struct (v, world)
            | struct ([|Int x; Int y|], world) -> struct (Pluggable { Vector2i = Vector2i (x, y) }, world)
            | struct ([|_; _|], world) -> struct (Violation (["InvalidArgumentType"; String.capitalize fnName], "Application of " + fnName + " requires an Int for the both arguments.", originOpt), world)
            | struct (_, world) -> struct (Violation (["InvalidArgumentCount"; String.capitalize fnName], "Incorrect number of arguments for '" + fnName + "'; 2 arguments required.", originOpt), world)

        static member internal evalIndexV2iExtrinsic fnName exprs originOpt world =
            match World.evalManyInternal exprs world with
            | struct ([|Violation _ as v; _|], world) -> struct (v, world)
            | struct ([|_; Violation _ as v|], world) -> struct (v, world)
            | struct ([|Keyword indexer; Pluggable pluggable|], world) ->
                match pluggable with
                | :? Vector2iPluggable as v2i ->
                    match indexer with
                    | "X" -> struct (Int v2i.Vector2i.X, world)
                    | "Y" -> struct (Int v2i.Vector2i.Y, world)
                    | _ -> struct (Violation (["InvalidIndexer"; String.capitalize fnName], "Invalid indexer '" + indexer + "' for V2i.", originOpt), world)
                | _ -> failwithumf ()
            | struct ([|_; _|], world) -> struct (Violation (["InvalidArgumentType"; String.capitalize fnName], "Application of " + fnName + " requires a V2i index.", originOpt), world)
            | struct (_, world) -> struct (Violation (["InvalidArgumentCount"; String.capitalize fnName], "Incorrect number of arguments for '" + fnName + "'; 2 arguments required.", originOpt), world)

        static member internal evalUpdateV2iExtrinsic fnName exprs originOpt world =
            match World.evalManyInternal exprs world with
            | struct ([|Violation _ as v; _; _|], world) -> struct (v, world)
            | struct ([|_; Violation _ as v; _|], world) -> struct (v, world)
            | struct ([|_; _; Violation _ as v|], world) -> struct (v, world)
            | struct ([|Keyword indexer; Int i; Pluggable pluggable|], world) ->
                match pluggable with
                | :? Vector2iPluggable as v2i ->
                    match indexer with
                    | "X" -> struct (Pluggable { Vector2i = Vector2i (i, v2i.Vector2i.Y) }, world)
                    | "Y" -> struct (Pluggable { Vector2i = Vector2i (v2i.Vector2i.X, i) }, world)
                    | _ -> struct (Violation (["InvalidIndexer"; String.capitalize fnName], "Invalid indexer '" + indexer + "' for V2i.", originOpt), world)
                | _ -> failwithumf ()
            | struct ([|_; _; _|], world) -> struct (Violation (["InvalidArgumentType"; String.capitalize fnName], "Application of " + fnName + " requires a V2i target, a keyword index of X or Y, and an Int value.", originOpt), world)
            | struct (_, world) -> struct (Violation (["InvalidArgumentCount"; String.capitalize fnName], "Incorrect number of arguments for '" + fnName + "'; 2 arguments required.", originOpt), world)

        static member internal evalGetExtrinsic fnName exprs originOpt world =
            match World.evalManyInternal exprs world with
            | struct ([|Violation _ as v; _|], world) -> struct (v, world)
            | struct ([|_; Violation _ as v|], world) -> struct (v, world)
            | struct ([|String propertyName; relation|], world)
            | struct ([|Keyword propertyName; relation|], world) -> World.evalGet fnName propertyName (Some relation) originOpt world
            | struct ([|_; _|], world) -> struct (Violation (["InvalidArgumentType"; String.capitalize fnName], "Application of " + fnName + " requires a String or Keyword for the first argument, and an optional Relation for the second.", originOpt), world)
            | struct ([|Violation _ as v|], world) -> struct (v, world)
            | struct ([|String propertyName|], world)
            | struct ([|Keyword propertyName|], world) -> World.evalGet fnName propertyName None originOpt world
            | struct ([|_|], world) -> struct (Violation (["InvalidArgumentType"; String.capitalize fnName], "Application of " + fnName + " requires a String or Keyword for the first argument, and an optional Relation for the second.", originOpt), world)
            | struct (_, world) -> struct (Violation (["InvalidArgumentCount"; String.capitalize fnName], "Incorrect number of arguments for '" + fnName + "'; 1 or 2 arguments required.", originOpt), world)

        static member internal evalGetAsStreamExtrinsic fnName exprs originOpt world =
            match World.evalManyInternal exprs world with
            | struct ([|Violation _ as v; _|], world) -> struct (v, world)
            | struct ([|_; Violation _ as v|], world) -> struct (v, world)
            | struct ([|String propertyName; relation|], world)
            | struct ([|Keyword propertyName; relation|], world) -> World.evalGetAsStream fnName propertyName (Some relation) originOpt world
            | struct ([|_; _|], world) -> struct (Violation (["InvalidArgumentType"; String.capitalize fnName], "Application of " + fnName + " requires a String or Keyword for the first argument, and an optional Relation for the second.", originOpt), world)
            | struct ([|Violation _ as v|], world) -> struct (v, world)
            | struct ([|String propertyName|], world)
            | struct ([|Keyword propertyName|], world) -> World.evalGetAsStream fnName propertyName None originOpt world
            | struct ([|_|], world) -> struct (Violation (["InvalidArgumentType"; String.capitalize fnName], "Application of " + fnName + " requires a String or Keyword for the first argument, and an optional Relation for the second.", originOpt), world)
            | struct (_, world) -> struct (Violation (["InvalidArgumentCount"; String.capitalize fnName], "Incorrect number of arguments for '" + fnName + "'; 1 or 2 arguments required.", originOpt), world)

        static member internal evalSetExtrinsic fnName exprs originOpt world =
            match World.evalManyInternal exprs world with
            | struct ([|Violation _ as v; _; _|], world) -> struct (v, world)
            | struct ([|_; Violation _ as v; _|], world) -> struct (v, world)
            | struct ([|_; _; Violation _ as v|], world) -> struct (v, world)
            | struct ([|String propertyName; relation; value|], world)
            | struct ([|Keyword propertyName; relation; value|], world) -> World.evalSet fnName propertyName (Some relation) value originOpt world
            | struct ([|_; _; _|], world) -> struct (Violation (["InvalidArgumentType"; String.capitalize fnName], "Application of " + fnName + " requires a String or Keyword for the first argument, and an optional Relation for the second.", originOpt), world)
            | struct ([|Violation _ as v; _|], world) -> struct (v, world)
            | struct ([|_; Violation _ as v|], world) -> struct (v, world)
            | struct ([|String propertyName; value|], world)
            | struct ([|Keyword propertyName; value|], world) -> World.evalSet fnName propertyName None value originOpt world
            | struct ([|_; _|], world) -> struct (Violation (["InvalidArgumentType"; String.capitalize fnName], "Application of " + fnName + " requires a String or Keyword for the first argument, a value for the second, and an optional Relation for the third.", originOpt), world)
            | struct (_, world) -> struct (Violation (["InvalidArgumentCount"; String.capitalize fnName], "Incorrect number of arguments for '" + fnName + "'; 2 or 3 arguments required.", originOpt), world)

        static member internal evalSetAsStreamExtrinsic fnName exprs originOpt world =
            match World.evalManyInternal exprs world with
            | struct ([|Violation _ as v; _; _|], world) -> struct (v, world)
            | struct ([|_; Violation _ as v; _|], world) -> struct (v, world)
            | struct ([|_; _; Violation _ as v|], world) -> struct (v, world)
            | struct ([|String propertyName; relation; value|], world)
            | struct ([|Keyword propertyName; relation; value|], world) -> World.evalSetAsStream fnName propertyName (Some relation) value originOpt world
            | struct ([|_; _; _|], world) -> struct (Violation (["InvalidArgumentType"; String.capitalize fnName], "Application of " + fnName + " requires a String or Keyword for the first argument, and an optional Relation for the second.", originOpt), world)
            | struct ([|Violation _ as v; _|], world) -> struct (v, world)
            | struct ([|_; Violation _ as v|], world) -> struct (v, world)
            | struct ([|String propertyName; value|], world)
            | struct ([|Keyword propertyName; value|], world) -> World.evalSetAsStream fnName propertyName None value originOpt world
            | struct ([|_; _|], world) -> struct (Violation (["InvalidArgumentType"; String.capitalize fnName], "Application of " + fnName + " requires a String or Keyword for the first argument, a value for the second, and an optional Relation for the third.", originOpt), world)
            | struct (_, world) -> struct (Violation (["InvalidArgumentCount"; String.capitalize fnName], "Incorrect number of arguments for '" + fnName + "'; 2 or 3 arguments required.", originOpt), world)

        static member internal evalStreamEventExtrinsic fnName exprs originOpt world =
            match World.evalManyInternal exprs world with
            | struct ([|Violation _ as v|], world) -> struct (v, world)
            | struct ([|evaled|], world) -> World.evalStreamEvent fnName evaled originOpt world
            | struct (_, world) -> struct (Violation (["InvalidArgumentCount"; String.capitalize fnName], "Incorrect number of arguments for '" + fnName + "'; 1 argument required.", originOpt), world)

        static member internal evalMapStreamExtrinsic fnName exprs originOpt world =
            match World.evalManyInternal exprs world with
            | struct ([|Violation _ as v; _|], world) -> struct (v, world)
            | struct ([|_; Violation _ as v|], world) -> struct (v, world)
            | struct ([|evaled; evaled2|], world) -> World.evalMapStream fnName evaled evaled2 originOpt world
            | struct (_, world) -> struct (Violation (["InvalidArgumentCount"; String.capitalize fnName], "Incorrect number of arguments for '" + fnName + "'; 2 arguments required.", originOpt), world)

        static member internal evalFoldStreamExtrinsic fnName exprs originOpt world =
            match World.evalManyInternal exprs world with
            | struct ([|Violation _ as v; _; _|], world) -> struct (v, world)
            | struct ([|_; Violation _ as v; _|], world) -> struct (v, world)
            | struct ([|_; _; Violation _ as v|], world) -> struct (v, world)
            | struct ([|evaled; evaled2; evaled3|], world) -> World.evalFoldStream fnName evaled evaled2 evaled3 originOpt world
            | struct (_, world) -> struct (Violation (["InvalidArgumentCount"; String.capitalize fnName], "Incorrect number of arguments for '" + fnName + "'; 3 arguments required.", originOpt), world)

        static member internal evalMap2StreamExtrinsic fnName exprs originOpt world =
            match World.evalManyInternal exprs world with
            | struct ([|Violation _ as v; _; _|], world) -> struct (v, world)
            | struct ([|_; Violation _ as v; _|], world) -> struct (v, world)
            | struct ([|_; _; Violation _ as v|], world) -> struct (v, world)
            | struct ([|evaled; evaled2; evaled3|], world) -> World.evalMap2Stream fnName evaled evaled2 evaled3 originOpt world
            | struct (_, world) -> struct (Violation (["InvalidArgumentCount"; String.capitalize fnName], "Incorrect number of arguments for '" + fnName + "'; 3 arguments required.", originOpt), world)

        static member internal evalProductStreamExtrinsic fnName exprs originOpt world =
            match World.evalManyInternal exprs world with
            | struct ([|Violation _ as v; _|], world) -> struct (v, world)
            | struct ([|_; Violation _ as v|], world) -> struct (v, world)
            | struct ([|evaled; evaled2|], world) -> World.evalProductStream fnName evaled evaled2 originOpt world
            | struct (_, world) -> struct (Violation (["InvalidArgumentCount"; String.capitalize fnName], "Incorrect number of arguments for '" + fnName + "'; 2 arguments required.", originOpt), world)

        static member internal evalSumStreamExtrinsic fnName exprs originOpt world =
            match World.evalManyInternal exprs world with
            | struct ([|Violation _ as v; _|], world) -> struct (v, world)
            | struct ([|_; Violation _ as v|], world) -> struct (v, world)
            | struct ([|evaled; evaled2|], world) -> World.evalSumStream fnName evaled evaled2 originOpt world
            | struct (_, world) -> struct (Violation (["InvalidArgumentCount"; String.capitalize fnName], "Incorrect number of arguments for '" + fnName + "'; 2 arguments required.", originOpt), world)

        static member internal evalMonitorExtrinsic fnName exprs originOpt world =
            match World.evalManyInternal exprs world with
            | struct ([|Violation _ as v; _|], world) -> struct (v, world)
            | struct ([|_; Violation _ as v|], world) -> struct (v, world)
            | struct ([|evaled; evaled2|], world) -> World.evalMonitor fnName evaled evaled2 originOpt world
            | struct (_, world) -> struct (Violation (["InvalidArgumentCount"; String.capitalize fnName], "Incorrect number of arguments for '" + fnName + "'; 2 arguments required.", originOpt), world)

        static member internal tryGetExtrinsic fnName =
            match Extrinsics.TryGetValue fnName with
            | (true, extrinsic) -> FOption.some extrinsic
            | (false, _) ->
                match Bindings.TryGetValue fnName with
                | (true, binding) -> FOption.some binding
                | (false, _) -> FOption.none ()

        static member internal evalSchedule fnName exprs originOpt world =
            match exprs with
            | [|Scripting.Int64 i; body|] ->
                let world =
                    World.schedule
                        (fun world ->
                            let context = World.getScriptContext world
                            match World.tryGetScriptFrame context world with
                            | Some scriptFrame -> World.eval body scriptFrame context world |> snd'
                            | None -> world)
                        i
                        world
                struct (Unit, world)
            | [|_; _|] -> struct (Violation (["InvalidArgumentType"; String.capitalize fnName], "Application of " + fnName + " requires an Int64 for the first argument.", originOpt), world)
            | _ -> struct (Violation (["InvalidArgumentCount"; String.capitalize fnName], "Incorrect number of arguments for '" + fnName + "'; 2 arguments required.", originOpt), world)

        static member internal evalSchedule1 fnName exprs originOpt world =
            match exprs with
            | [|body|] ->
                let world =
                    World.schedule2
                        (fun world ->
                            let context = World.getScriptContext world
                            match World.tryGetScriptFrame context world with
                            | Some scriptFrame -> World.eval body scriptFrame context world |> snd'
                            | None -> world)
                        world
                struct (Unit, world)
            | _ -> struct (Violation (["InvalidArgumentCount"; String.capitalize fnName], "Incorrect number of arguments for '" + fnName + "'; 1 argument required.", originOpt), world)

        static member internal evalBlank _ _ _ world =
            let context = World.getScriptContext world
            let world = World.unsubscribeSimulantScripts context world
            struct (Unit, world)

        static member internal evalInfo fnName exprs originOpt world =
            match exprs with
            | [|String addressStr|]
            | [|Keyword addressStr|] ->
                let simulant = addressStr |> stoa |> World.derive
                match World.tryGetState simulant world with
                | Some state ->
                    let propStrs =
                        World.getProperties state |>
                        List.filter (fun (name, _, _) ->
                            match name with
                            | "Xtension" | "Dispatcher" | "Facets" | "ScriptFrame" -> false
                            | _ -> true) |>
                        List.map (fun (name, ty, value) ->
                            let ty = if isNull value then ty else getType value
                            let converter = SymbolicConverter (false, None, ty)
                            let valueStr = converter.ConvertToString value |> String.escape
                            name + " = " + valueStr)
                    let propsStr = String.Join ("\n", propStrs)
                    struct (String propsStr, world)
                | None -> struct (Unit, world)
            | [|_|] -> struct (Violation (["InvalidArgumentType"; String.capitalize fnName], "Application of " + fnName + " requires an Int64 for the first argument.", originOpt), world)
            | _ -> struct (Violation (["InvalidArgumentCount"; String.capitalize fnName], "Incorrect number of arguments for '" + fnName + "'; 2 arguments required.", originOpt), world)

        static member internal initScripting () =
            let extrinsics =
                [("v2", { Fn = World.evalV2Extrinsic; Pars = [|"x"; "y"|]; DocOpt = Some "Construct a Vector2." })
                 ("index_Vector2", { Fn = World.evalIndexV2Extrinsic; Pars = [||]; DocOpt = None })
                 ("alter_Vector2", { Fn = World.evalUpdateV2Extrinsic; Pars = [||]; DocOpt = None })
                 ("v4", { Fn = World.evalV4Extrinsic; Pars = [|"x"; "y"; "z"; "w"|]; DocOpt = Some "Construct a Vector4." })
                 ("index_Vector4", { Fn = World.evalIndexV4Extrinsic; Pars = [||]; DocOpt = None })
                 ("alter_Vector4", { Fn = World.evalUpdateV4Extrinsic; Pars = [||]; DocOpt = None })
                 ("v2i", { Fn = World.evalV2iExtrinsic; Pars = [|"x"; "y"|]; DocOpt = Some "Construct a Vector2i." })
                 ("index_Vector2i", { Fn = World.evalIndexV2iExtrinsic; Pars = [||]; DocOpt = None })
                 ("alter_Vector2i", { Fn = World.evalUpdateV2iExtrinsic; Pars = [||]; DocOpt = None })
                 ("get", { Fn = World.evalGetExtrinsic; Pars = [|"simulant?"; "property"|]; DocOpt = Some "Get a simulant's property." })
                 ("getAsStream", { Fn = World.evalGetAsStreamExtrinsic; Pars = [|"simulant?"; "property"|]; DocOpt = Some "Get a simulant's property as a Stream." })
                 ("set", { Fn = World.evalSetExtrinsic; Pars = [|"simulant?"; "property"; "value"|]; DocOpt = Some "Set a simulant's property." })
                 ("setAsStream", { Fn = World.evalSetAsStreamExtrinsic; Pars = [|"simulant?"; "property"; "stream"|]; DocOpt = Some "Equate a simulant's property to a Stream." })
                 ("streamEvent", { Fn = World.evalStreamEventExtrinsic; Pars = [|"event"|]; DocOpt = Some "Construct a Stream for a given event." })
                 ("map_Stream", { Fn = World.evalMapStreamExtrinsic; Pars = [||]; DocOpt = None })
                 ("fold_Stream", { Fn = World.evalFoldStreamExtrinsic; Pars = [||]; DocOpt = None })
                 ("map2_Stream", { Fn = World.evalMapStreamExtrinsic; Pars = [||]; DocOpt = None })
                 ("product_Stream", { Fn = World.evalProductStreamExtrinsic; Pars = [||]; DocOpt = None })
                 ("sum_Stream", { Fn = World.evalSumStreamExtrinsic; Pars = [||]; DocOpt = None })
                 ("monitor", { Fn = World.evalMonitorExtrinsic; Pars = [|"handler"; "event"|]; DocOpt = Some "Subscribe to an event for the lifetime of the simulant." })
                 ("schedule", { Fn = World.evalSchedule; Pars = [|"time"; "body"|]; DocOpt = None })
                 ("schedule1", { Fn = World.evalSchedule1; Pars = [|"body"|]; DocOpt = None })
                 ("blank", { Fn = World.evalBlank; Pars = [||]; DocOpt = None })
                 ("info_String", { Fn = World.evalInfo; Pars = [||]; DocOpt = None })
                 ("info_Keyword", { Fn = World.evalInfo; Pars = [||]; DocOpt = None })] |>
                dictPlus
            Extrinsics <- extrinsics