namespace Nu.Ecs
open System
open System.Collections.Generic
open System.IO
open System.Numerics
open System.Threading.Tasks
open Prime
open Nu

/// An Ecs event callback.
type private EcsCallback<'d, 's> =
    EcsEvent<'d, 's> -> Ecs -> 's -> 's

/// The type of Ecs event.
and [<StructuralEquality; NoComparison; Struct>] EcsEventType =
    | GlobalEvent
    | EntityEvent of Entity : EcsEntity
    | ComponentEvent of Entity2 : EcsEntity * ComponentEvent : string

/// An Ecs event.
and [<StructuralEquality; NoComparison; Struct>] EcsEvent =
    { EcsEventName : string
      EcsEventType : EcsEventType }

/// An Ecs event.
and [<NoEquality; NoComparison>] EcsEvent<'d, 's> =
    { EcsEventData : 'd }

/// Data for an Ecs registration event.
and [<NoEquality; NoComparison; Struct>] EcsChangeData =
    { EcsEntity : EcsEntity
      ComponentName : string }

/// Data for an Ecs registration event.
and [<NoEquality; NoComparison; Struct>] EcsRegistrationData =
    { EcsEntity : EcsEntity
      ComponentName : string }

/// The out-of-box events for the Ecs construct.
and [<AbstractClass; Sealed>] EcsEvents =
    static member Update = { EcsEventName = "Update"; EcsEventType = GlobalEvent }
    static member UpdateParallel = { EcsEventName = "UpdateParallel"; EcsEventType = GlobalEvent }
    static member PostUpdate = { EcsEventName = "PostUpdate"; EcsEventType = GlobalEvent }
    static member PostUpdateParallel = { EcsEventName = "PostUpdateParallel"; EcsEventType = GlobalEvent }
    static member Actualize = { EcsEventName = "Actualize"; EcsEventType = GlobalEvent }
    static member Register entity compName = { EcsEventName = "Register"; EcsEventType = ComponentEvent (entity, compName) }
    static member Unregistering entity compName = { EcsEventName = "Unregistering"; EcsEventType = ComponentEvent (entity, compName) }
    static member Change entity = { EcsEventName = "Change"; EcsEventType = EntityEvent entity }

and [<StructuralEquality; NoComparison>] Term =
    | Z of int
    | R of single
    | V3 of Vector3
    | B3 of Box3
    | Cmp of IComparable
    | Arb of obj
    | Tag
    | Label of string
    | Labels of string HashSet
    | EcsEntity of EcsEntity
    | Intra of string * Type
    | Extra of string * Type * obj AlwaysEqual // only creates component when at top-level.
    | Terms of Term list
    static member equals (this : Term) (that : Term) = this.Equals that
    static member equalsMany (lefts : IReadOnlyDictionary<string, Term>) (rights : IReadOnlyDictionary<string, Term>) =
        if lefts.Count = rights.Count then
            let mutable result = true
            let mutable enr = lefts.GetEnumerator ()
            while result && enr.MoveNext () do
                let current = enr.Current
                let termName = current.Key
                match rights.TryGetValue termName with
                | (true, term) -> result <- Term.equals current.Value term
                | (false, _) -> result <- false
            result
        else false
    static member dict entries = dictPlus<string, Term> StringComparer.Ordinal entries
    static member singleton termName term = Term.dict [(termName, term)]
    static member zero = Term.dict []

and [<NoEquality; NoComparison>] Subquery =
    | Is of string
    | Of of string * string
    | Eq of string * Term
    | Gt of string * Term
    | Ge of string * Term
    | Lt of string * Term
    | Le of string * Term
    | If of string * (Term -> bool)
    | Intersect of string * Term
    | Or of Subquery list
    | And of Subquery list
    | Not of Subquery
    | It of Subquery * Subquery
    | Let of string * Subquery * Subquery
    | Literal of Term
    | At of string * int
    | Head of string
    | Tail of string
    | ByName of string * string
    | ByType of string * Type
    | Analyze of (IReadOnlyDictionary<string, Term> -> bool)

    static member private equalTo term term2 =
        match (term, term2) with
        | (Z z, Z z2) -> z = z2
        | (R r, R r2) -> r = r2
        | (V3 v, V3 v2) -> v3Eq v v2
        | (B3 b, B3 b2) -> box3Eq b b2
        | (Cmp c, Cmp c2) -> c = c2
        | (Arb o, Arb o2) -> objEq o o2
        | (Label label, Label label2) -> strEq label label2
        | (Labels labels, Labels labels2) -> labels.SetEquals labels2
        | (EcsEntity entity, EcsEntity entity2) -> genEq entity entity2
        | (Terms terms, Terms terms2) ->
            if terms.Length = terms2.Length
            then List.forall2 Subquery.equalTo terms terms2
            else false
        | _ -> false

    static member private greaterThan term term2 =
        match (term, term2) with
        | (Z z, Z z2) -> z > z2
        | (R r, R r2) -> r > r2
        | (Cmp c, Cmp c2) -> c.CompareTo c2 > 0
        | (Terms terms, Terms terms2) ->
            if terms.Length = terms2.Length
            then List.forall2 Subquery.greaterThan terms terms2
            else false
        | (_, _) -> false

    static member private greaterEqual term term2 =
        match (term, term2) with
        | (Z z, Z z2) -> z >= z2
        | (R r, R r2) -> r >= r2
        | (Cmp c, Cmp c2) -> c.CompareTo c2 >= 0
        | (Terms terms, Terms terms2) ->
            if terms.Length = terms2.Length
            then List.forall2 Subquery.greaterEqual terms terms2
            else false
        | (_, _) -> false

    static member private lesserThan term term2 =
        match (term, term2) with
        | (Z z, Z z2) -> z < z2
        | (R r, R r2) -> r < r2
        | (Cmp c, Cmp c2) -> c.CompareTo c2 < 0
        | (Terms terms, Terms terms2) ->
            if terms.Length = terms2.Length
            then List.forall2 Subquery.lesserThan terms terms2
            else false
        | (_, _) -> false

    static member private lesserEqual term term2 =
        match (term, term2) with
        | (Z z, Z z2) -> z <= z2
        | (R r, R r2) -> r <= r2
        | (Cmp c, Cmp c2) -> c.CompareTo c2 <= 0
        | (Terms terms, Terms terms2) ->
            if terms.Length = terms2.Length
            then List.forall2 Subquery.lesserEqual terms terms2
            else false
        | (_, _) -> false

    static member private tryEvalTerm (terms : Map<string, Term>) subquery =
        match subquery with
        | Literal term ->
            ValueSome term
        | Head termName ->
            match terms.TryGetValue termName with
            | (true, term) ->
                match term with
                | Terms terms ->
                    match terms with
                    | head :: _ -> ValueSome head
                    | _ -> ValueNone
                | _ -> ValueNone
            | (false, _) -> ValueNone
        | Tail termName ->
            match terms.TryGetValue termName with
            | (true, term) ->
                match term with
                | Terms terms ->
                    match terms with
                    | _ :: tail -> ValueSome (Terms tail)
                    | _ -> ValueNone
                | _ -> ValueNone
            | (false, _) -> ValueNone
        | At (termName, index) ->
            match terms.TryGetValue termName with
            | (true, term) ->
                match term with
                | Terms terms ->
                    match Seq.tryItem index terms with
                    | Some item -> ValueSome item
                    | None -> ValueNone
                | _ -> ValueNone
            | (false, _) -> ValueNone
        | _ -> ValueNone

    static member eval (terms : Map<string, Term>) (subquery : Subquery) : bool =
        match subquery with
        | Is termName ->
            terms.ContainsKey termName
        | Of (termName, label2) ->
            match terms.TryGetValue termName with
            | (true, term) ->
                match term with
                | Label label -> strEq label label2
                | Labels labels -> labels.Contains label2
                | _ -> false
            | (false, _) -> false
        | Eq (termName, term2) ->
            match terms.TryGetValue termName with
            | (true, term) -> Subquery.equalTo term term2
            | (false, _) -> false
        | Gt (termName, term2) ->
            match terms.TryGetValue termName with
            | (true, term) -> Subquery.greaterThan term term2
            | (false, _) -> false
        | Ge (termName, term2) ->
            match terms.TryGetValue termName with
            | (true, term) -> Subquery.greaterEqual term term2
            | (false, _) -> false
        | Lt (termName, term2) ->
            match terms.TryGetValue termName with
            | (true, term) -> Subquery.lesserThan term term2
            | (false, _) -> false
        | Le (termName, term2) ->
            match terms.TryGetValue termName with
            | (true, term) -> Subquery.lesserEqual term term2
            | (false, _) -> false
        | If (termName, pred) ->
            match terms.TryGetValue termName with
            | (true, term) -> pred term
            | (false, _) -> false
        | Intersect (termName, term2) ->
            match terms.TryGetValue termName with
            | (true, term) ->
                match (term, term2) with
                | (B3 box, B3 box2) -> box.Intersects box2
                | (V3 v, B3 box) | (B3 box, V3 v)-> box.Intersects v
                | (_, _) -> false
            | (false, _) -> false
        | Or subqueries ->
            List.exists (Subquery.eval terms) subqueries
        | Not subquery ->
            not (Subquery.eval terms subquery)
        | And subqueries ->
            List.forall (Subquery.eval terms) subqueries
        | Let (bindingName, subquery, subquery2) ->
            match Subquery.tryEvalTerm terms subquery with
            | ValueSome term ->
                let terms = Map.add bindingName term terms
                Subquery.eval terms subquery2
            | ValueNone -> false
        | It (subquery, subquery2) ->
            match Subquery.tryEvalTerm terms subquery with
            | ValueSome term ->
                let terms = Map.add "it" term terms
                Subquery.eval terms subquery2
            | ValueNone -> false
        | Literal _ | At (_, _) | Head _ | Tail _ ->
            false
        | ByName (termName, compName2) ->
            match terms.TryGetValue termName with
            | (true, term) ->
                match term with
                | Intra (compName, _)  -> strEq compName compName2
                | Extra (compName, _, _)  -> strEq compName compName2
                | _ -> false
            | (false, _) -> false
        | ByType (termName, ty2) ->
            match terms.TryGetValue termName with
            | (true, term) ->
                match term with
                | Intra (_, ty)  -> refEq ty ty2
                | Extra (_, ty, _) -> refEq ty ty2
                | _ -> false
            | (false, _) -> false
        | Analyze analyzer ->
            analyzer terms

    static member evalMany (terms : Map<string, Term>) (subqueries : Subquery seq) =
        let mutable result = true
        let mutable subqueryEnr = subqueries.GetEnumerator ()
        while result && subqueryEnr.MoveNext () do
            let subquery = subqueryEnr.Current
            if not (Subquery.eval terms subquery) then
                result <- false
        result

    static member dict entries = dictPlus<string, Subquery> StringComparer.Ordinal entries
    static member singleton subqueryName subquery = Subquery.dict [(subqueryName, subquery)]
    static member zero = []

/// An entity's place in an archetype.
and [<StructuralEquality; NoComparison; Struct>] ArchetypeSlot =
    { ArchetypeIndex : int
      Archetype : Archetype }

/// Identifies an archetype.
and ArchetypeId (terms : Map<string, Term>) =

    let hashCode = hash terms // OPTIMIZATION: hash is cached for speed

    new (intraComponents, subterms : Map<string, Term>) =
        ArchetypeId
            (let intraterms = intraComponents|> Seq.map (fun (compName, compTy) -> (Constants.Ecs.IntraComponentPrefix + compName, Intra (compName, compTy))) |> Map.ofSeq
             intraterms @@ subterms)

    new (intraComponentTypes : Type seq, subterms : Dictionary<string, Term>) =
        ArchetypeId
            (let intraterms = intraComponentTypes |> Seq.map (fun compTy -> let compName = compTy.Name in (Constants.Ecs.IntraComponentPrefix + compName, Intra (compName, compTy))) |> Map.ofSeq
             intraterms @@ subterms)

    member this.Terms : Map<string, Term> =
        terms

    member this.HashCode =
        hashCode

    member this.AddTerm termName term =
        ArchetypeId (Map.add termName term terms)

    member this.RemoveTerm termName =
        ArchetypeId (Map.remove termName terms)

    static member equals (left : ArchetypeId) (right : ArchetypeId) =
        left.HashCode = right.HashCode &&
        Term.equalsMany left.Terms right.Terms

    static member singleton termName term =
        ArchetypeId (Map.singleton termName term)

    static member zero =
        ArchetypeId Map.empty

    override this.GetHashCode () =
        hashCode

    override this.Equals that =
        match that with
        | :? ArchetypeId as that -> ArchetypeId.equals this that
        | _ -> failwithumf ()

    static member make (intraComponents : obj seq, subterms) =
        let intraComponentTypes = Seq.map getType intraComponents
        let intraComponentNames = Seq.map (fun (ty : Type) -> ty.Name) intraComponentTypes
        let intraComponents = Seq.zip intraComponentNames intraComponentTypes
        ArchetypeId (intraComponents, subterms)

    static member make (intraComponents : (string * obj) seq, subterms) =
        let intraComponents = Seq.map (fun (compName, compValue) -> (compName, getType compValue)) intraComponents
        ArchetypeId (intraComponents, subterms)

    static member make<'c when
        'c : struct and 'c :> 'c Component>
        (compName, subterms) =
        ArchetypeId ([(compName, typeof<'c>)], subterms)

    static member make<'c, 'c2 when
        'c : struct and 'c :> 'c Component and
        'c2 : struct and 'c2 :> 'c2 Component>
        (compName, comp2Name, subterms) =
        ArchetypeId ([(compName, typeof<'c>); (comp2Name, typeof<'c2>)], subterms)

    static member make<'c, 'c2, 'c3 when
        'c : struct and 'c :> 'c Component and
        'c2 : struct and 'c2 :> 'c2 Component and
        'c3 : struct and 'c3 :> 'c3 Component>
        (compName, comp2Name, comp3Name, subterms) =
        ArchetypeId ([(compName, typeof<'c>); (comp2Name, typeof<'c2>); (comp3Name, typeof<'c3>)], subterms)

    static member make<'c, 'c2, 'c3, 'c4 when
        'c : struct and 'c :> 'c Component and
        'c2 : struct and 'c2 :> 'c2 Component and
        'c3 : struct and 'c3 :> 'c3 Component and
        'c4 : struct and 'c4 :> 'c4 Component>
        (compName, comp2Name, comp3Name, comp4Name, subterms) =
        ArchetypeId
            ([(compName, typeof<'c>)
              (comp2Name, typeof<'c2>)
              (comp3Name, typeof<'c3>)
              (comp4Name, typeof<'c4>)],
             subterms)

    static member make<'c, 'c2, 'c3, 'c4, 'c5 when
        'c : struct and 'c :> 'c Component and
        'c2 : struct and 'c2 :> 'c2 Component and
        'c3 : struct and 'c3 :> 'c3 Component and
        'c4 : struct and 'c4 :> 'c4 Component and
        'c5 : struct and 'c5 :> 'c5 Component>
        (compName, comp2Name, comp3Name, comp4Name, comp5Name, subterms) =
        ArchetypeId
            ([(compName, typeof<'c>)
              (comp2Name, typeof<'c2>)
              (comp3Name, typeof<'c3>)
              (comp4Name, typeof<'c4>)
              (comp5Name, typeof<'c5>)],
             subterms)

    static member make<'c, 'c2, 'c3, 'c4, 'c5, 'c6 when
        'c : struct and 'c :> 'c Component and
        'c2 : struct and 'c2 :> 'c2 Component and
        'c3 : struct and 'c3 :> 'c3 Component and
        'c4 : struct and 'c4 :> 'c4 Component and
        'c5 : struct and 'c5 :> 'c5 Component and
        'c6 : struct and 'c6 :> 'c6 Component>
        (compName, comp2Name, comp3Name, comp4Name, comp5Name, comp6Name, subterms) =
        ArchetypeId
            ([(compName, typeof<'c>)
              (comp2Name, typeof<'c2>)
              (comp3Name, typeof<'c3>)
              (comp4Name, typeof<'c4>)
              (comp5Name, typeof<'c5>)
              (comp6Name, typeof<'c6>)],
             subterms)

    static member make<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'c7 when
        'c : struct and 'c :> 'c Component and
        'c2 : struct and 'c2 :> 'c2 Component and
        'c3 : struct and 'c3 :> 'c3 Component and
        'c4 : struct and 'c4 :> 'c4 Component and
        'c5 : struct and 'c5 :> 'c5 Component and
        'c6 : struct and 'c6 :> 'c6 Component and
        'c7 : struct and 'c7 :> 'c7 Component>
        (compName, comp2Name, comp3Name, comp4Name, comp5Name, comp6Name, comp7Name, subterms) =
        ArchetypeId
            ([(compName, typeof<'c>)
              (comp2Name, typeof<'c2>)
              (comp3Name, typeof<'c3>)
              (comp4Name, typeof<'c4>)
              (comp5Name, typeof<'c5>)
              (comp6Name, typeof<'c6>)
              (comp7Name, typeof<'c7>)],
             subterms)

    static member make<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'c7, 'c8 when
        'c : struct and 'c :> 'c Component and
        'c2 : struct and 'c2 :> 'c2 Component and
        'c3 : struct and 'c3 :> 'c3 Component and
        'c4 : struct and 'c4 :> 'c4 Component and
        'c5 : struct and 'c5 :> 'c5 Component and
        'c6 : struct and 'c6 :> 'c6 Component and
        'c7 : struct and 'c7 :> 'c7 Component and
        'c8 : struct and 'c8 :> 'c8 Component>
        (compName, comp2Name, comp3Name, comp4Name, comp5Name, comp6Name, comp7Name, comp8Name, subterms) =
        ArchetypeId
            ([(compName, typeof<'c>)
              (comp2Name, typeof<'c2>)
              (comp3Name, typeof<'c3>)
              (comp4Name, typeof<'c4>)
              (comp5Name, typeof<'c5>)
              (comp6Name, typeof<'c6>)
              (comp7Name, typeof<'c7>)
              (comp8Name, typeof<'c8>)],
             subterms)

    static member make<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'c7, 'c8, 'c9 when
        'c : struct and 'c :> 'c Component and
        'c2 : struct and 'c2 :> 'c2 Component and
        'c3 : struct and 'c3 :> 'c3 Component and
        'c4 : struct and 'c4 :> 'c4 Component and
        'c5 : struct and 'c5 :> 'c5 Component and
        'c6 : struct and 'c6 :> 'c6 Component and
        'c7 : struct and 'c7 :> 'c7 Component and
        'c8 : struct and 'c8 :> 'c8 Component and
        'c9 : struct and 'c9 :> 'c9 Component>
        (compName, comp2Name, comp3Name, comp4Name, comp5Name, comp6Name, comp7Name, comp8Name, comp9Name, subterms) =
        ArchetypeId
            ([(compName, typeof<'c>)
              (comp2Name, typeof<'c2>)
              (comp3Name, typeof<'c3>)
              (comp4Name, typeof<'c4>)
              (comp5Name, typeof<'c5>)
              (comp6Name, typeof<'c6>)
              (comp7Name, typeof<'c7>)
              (comp8Name, typeof<'c8>)
              (comp9Name, typeof<'c9>)],
             subterms)

    static member make<'c when
        'c : struct and 'c :> 'c Component>
        subterms =
        ArchetypeId ([typeof<'c>], subterms)

    static member make<'c, 'c2 when
        'c : struct and 'c :> 'c Component and
        'c2 : struct and 'c2 :> 'c2 Component>
        subterms =
        ArchetypeId ([typeof<'c>; typeof<'c2>], subterms)

    static member make<'c, 'c2, 'c3 when
        'c : struct and 'c :> 'c Component and
        'c2 : struct and 'c2 :> 'c2 Component and
        'c3 : struct and 'c3 :> 'c3 Component>
        subterms =
        ArchetypeId ([typeof<'c>; typeof<'c2>; typeof<'c3>], subterms)

    static member make<'c, 'c2, 'c3, 'c4 when
        'c : struct and 'c :> 'c Component and
        'c2 : struct and 'c2 :> 'c2 Component and
        'c3 : struct and 'c3 :> 'c3 Component and
        'c4 : struct and 'c4 :> 'c4 Component>
        subterms =
        ArchetypeId ([typeof<'c>; typeof<'c2>; typeof<'c3>; typeof<'c4>], subterms)

    static member make<'c, 'c2, 'c3, 'c4, 'c5 when
        'c : struct and 'c :> 'c Component and
        'c2 : struct and 'c2 :> 'c2 Component and
        'c3 : struct and 'c3 :> 'c3 Component and
        'c4 : struct and 'c4 :> 'c4 Component and
        'c5 : struct and 'c5 :> 'c5 Component>
        subterms =
        ArchetypeId ([typeof<'c>; typeof<'c2>; typeof<'c3>; typeof<'c4>; typeof<'c5>], subterms)

    static member make<'c, 'c2, 'c3, 'c4, 'c5, 'c6 when
        'c : struct and 'c :> 'c Component and
        'c2 : struct and 'c2 :> 'c2 Component and
        'c3 : struct and 'c3 :> 'c3 Component and
        'c4 : struct and 'c4 :> 'c4 Component and
        'c5 : struct and 'c5 :> 'c5 Component and
        'c6 : struct and 'c6 :> 'c6 Component>
        subterms =
        ArchetypeId ([typeof<'c>; typeof<'c2>; typeof<'c3>; typeof<'c4>; typeof<'c5>; typeof<'c6>], subterms)

    static member make<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'c7 when
        'c : struct and 'c :> 'c Component and
        'c2 : struct and 'c2 :> 'c2 Component and
        'c3 : struct and 'c3 :> 'c3 Component and
        'c4 : struct and 'c4 :> 'c4 Component and
        'c5 : struct and 'c5 :> 'c5 Component and
        'c6 : struct and 'c6 :> 'c6 Component and
        'c7 : struct and 'c7 :> 'c7 Component>
        subterms =
        ArchetypeId ([typeof<'c>; typeof<'c2>; typeof<'c3>; typeof<'c4>; typeof<'c5>; typeof<'c6>; typeof<'c7>], subterms)

    static member make<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'c7, 'c8 when
        'c : struct and 'c :> 'c Component and
        'c2 : struct and 'c2 :> 'c2 Component and
        'c3 : struct and 'c3 :> 'c3 Component and
        'c4 : struct and 'c4 :> 'c4 Component and
        'c5 : struct and 'c5 :> 'c5 Component and
        'c6 : struct and 'c6 :> 'c6 Component and
        'c7 : struct and 'c7 :> 'c7 Component and
        'c8 : struct and 'c8 :> 'c8 Component>
        subterms =
        ArchetypeId ([typeof<'c>; typeof<'c2>; typeof<'c3>; typeof<'c4>; typeof<'c5>; typeof<'c6>; typeof<'c7>; typeof<'c8>], subterms)

    static member make<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'c7, 'c8, 'c9 when
        'c : struct and 'c :> 'c Component and
        'c2 : struct and 'c2 :> 'c2 Component and
        'c3 : struct and 'c3 :> 'c3 Component and
        'c4 : struct and 'c4 :> 'c4 Component and
        'c5 : struct and 'c5 :> 'c5 Component and
        'c6 : struct and 'c6 :> 'c6 Component and
        'c7 : struct and 'c7 :> 'c7 Component and
        'c8 : struct and 'c8 :> 'c8 Component and
        'c9 : struct and 'c9 :> 'c9 Component>
        subterms =
        ArchetypeId ([typeof<'c>; typeof<'c2>; typeof<'c3>; typeof<'c4>; typeof<'c5>; typeof<'c6>; typeof<'c7>; typeof<'c8>; typeof<'c9>], subterms)

/// A collection of component stores.
and Archetype (archetypeId : ArchetypeId) =

    let mutable freeIndex = 0
    let freeList = hashSetPlus<int> HashIdentity.Structural []
    let stores = dictPlus<string, Store> StringComparer.Ordinal []

    do
        let storeTypeGeneric = typedefof<EntityId Store>
        for termEntry in archetypeId.Terms do
            match termEntry.Value with
            | Intra (name, ty)
            | Extra (name, ty, _) ->
                let storeType = storeTypeGeneric.MakeGenericType [|ty|]
                let store = Activator.CreateInstance (storeType, name) :?> Store
                stores.[name] <- store
            | _ -> ()

    member this.Id = archetypeId
    member this.Length = freeIndex
    member this.Stores = stores
    member this.ComponentNames = hashSetPlus StringComparer.Ordinal stores.Keys

    member private this.Grow () =
        for storeEntry in stores do
            storeEntry.Value.Grow ()

    member private this.AllocIndex () =
        if freeList.Count > 0 then
            let index = Seq.head freeList
            freeList.Remove index |> ignore<bool>
            index
        else
            match Seq.tryHead stores with
            | Some headStoreEntry ->
                let index = freeIndex
                if index = headStoreEntry.Value.Length then this.Grow ()
                freeIndex <- inc freeIndex
                index
            | None ->
                let index = freeIndex
                freeIndex <- inc freeIndex
                index

    member private this.FreeIndex index =
        if index = dec freeIndex
        then freeIndex <- dec freeIndex
        else freeList.Add index |> ignore<bool>

    member this.Register (comps : Dictionary<string, obj>) =
        let index = this.AllocIndex ()
        for compEntry in comps do
            stores.[compEntry.Key].SetItem index compEntry.Value
        index

    member this.Unregister (index : int) =
        for storeEntry in stores do
            storeEntry.Value.ZeroItem index
        this.FreeIndex index

    member this.GetComponents index =
        let comps = dictPlus<string, obj> StringComparer.Ordinal []
        for storeEntry in stores do
            comps.Add (storeEntry.Key, storeEntry.Value.[index])
        comps

    member this.Read count (stream : FileStream) =
        let firstIndex = freeIndex
        let lastIndex = freeIndex + count
        match Seq.tryHead stores with
        | Some headStoreEntry ->
            while headStoreEntry.Value.Length <= lastIndex do
                this.Grow ()
        | None -> ()
        for storeEntry in stores do
            let store = storeEntry.Value
            store.Read count freeIndex stream
        freeIndex <- inc lastIndex
        (firstIndex, lastIndex)

/// An archetype-based Ecs construct.
and Ecs () =

    let mutable subscriptionIdCurrent = 0u
    let mutable entityIdCurrent = 0UL
    let archetypes = dictPlus<ArchetypeId, Archetype> HashIdentity.Structural []
    let archetypeSlots = dictPlus<uint64, ArchetypeSlot> HashIdentity.Structural []
    let componentTypes = dictPlus<string, Type> StringComparer.Ordinal []
    let subscriptions = dictPlus<EcsEvent, Dictionary<uint32, obj>> HashIdentity.Structural []
    let subscribedEntities = dictPlus<EcsEntity, int> HashIdentity.Structural []
    let queries = List<Query> ()

    let createArchetype (archetypeId : ArchetypeId) =
        let archetype = Archetype archetypeId
        archetypes.Add (archetypeId, archetype)
        for query in queries do
            query.TryRegisterArchetype archetype
        archetype

    member private this.AllocSubscriptionId () =
        subscriptionIdCurrent <- inc subscriptionIdCurrent
        if subscriptionIdCurrent = UInt32.MaxValue then failwith "Unbounded use of Ecs subscription ids not supported."
        subscriptionIdCurrent

    member private this.BoxCallback<'d, 's> (callback : EcsCallback<'d, 's>) =
        let boxableCallback = fun (evt : EcsEvent<obj, 's>) store ->
            let evt = { EcsEventData = evt.EcsEventData :?> 'd }
            callback evt store
        boxableCallback :> obj

    member private this.RegisterEntityInternal comps archetypeId entity =
        let archetype =
            match archetypes.TryGetValue archetypeId with
            | (true, archetype) -> archetype
            | (false, _) -> createArchetype archetypeId
        let archetypeIndex = archetype.Register comps
        archetypeSlots.Add (entity.EntityId, { ArchetypeIndex = archetypeIndex; Archetype = archetype })

    member private this.UnregisterEntityInternal archetypeSlot entity =
        let archetype = archetypeSlot.Archetype
        let comps = archetype.GetComponents archetypeSlot.ArchetypeIndex
        archetypeSlots.Remove entity.EntityId |> ignore<bool>
        archetype.Unregister archetypeSlot.ArchetypeIndex
        comps

    member internal this.IndexArchetypeSlot (entity : EcsEntity) =
        archetypeSlots.[entity.EntityId]

    member this.Entity =
        entityIdCurrent <- inc entityIdCurrent
        if entityIdCurrent = UInt64.MaxValue then failwith "Unbounded use of Ecs entity ids not supported."
        { EntityId = entityIdCurrent; Ecs = this }

    member this.Publish<'d, 's> event (eventData : 'd) (state : 's) : 's =
        let mutable state = state
        match subscriptions.TryGetValue event with
        | (true, callbacks) ->
            for entry in callbacks do
                match entry.Value with
                | :? EcsCallback<obj, 's> as objCallback ->
                    let evt = { EcsEventData = eventData :> obj }
                    state <- objCallback evt this state
                | :? EcsCallback<obj, obj> as objCallback ->
                    let evt = { EcsEventData = eventData } : EcsEvent<obj, obj>
                    state <- objCallback evt this (state :> obj) :?> 's
                | _ -> ()
        | (false, _) -> ()
        state

    member this.PublishAsync<'d, 's> event (eventData : 'd) =
        let vsync =
            match subscriptions.TryGetValue event with
            | (true, callbacks) ->
                callbacks |>
                Seq.map (fun entry ->
                    Task.Run (fun () ->
                        match entry.Value with
                        | :? EcsCallback<obj, 's> as objCallback ->
                            let evt = { EcsEventData = eventData :> obj }
                            objCallback evt this Unchecked.defaultof<'s> |> ignore<'s>
                        | :? EcsCallback<obj, obj> as objCallback ->
                            let evt = { EcsEventData = eventData } : EcsEvent<obj, obj>
                            objCallback evt this Unchecked.defaultof<obj> |> ignore<obj>
                        | _ -> ()) |> Vsync.AwaitTask) |>
                Vsync.Parallel
            | (false, _) -> Vsync.Parallel []
        Vsync.StartAsTask vsync

    member this.SubscribePlus<'d, 's> subscriptionId event (callback : EcsCallback<'d, 's>) =
        let subscriptionId =
            match subscriptions.TryGetValue event with
            | (true, callbacks) ->
                callbacks.Add (subscriptionId, this.BoxCallback<'d, 's> callback)
                subscriptionId
            | (false, _) ->
                let callbacks = dictPlus HashIdentity.Structural [(subscriptionId, this.BoxCallback<'d, 's> callback)]
                subscriptions.Add (event, callbacks)
                subscriptionId
        match event.EcsEventType with
        | ComponentEvent (entity, _) ->
            match subscribedEntities.TryGetValue entity with
            | (true, count) -> subscribedEntities.[entity] <- inc count
            | (false, _) -> subscribedEntities.Add (entity, 1)
        | _ -> ()
        subscriptionId

    member this.Subscribe<'d, 's> event callback =
        this.SubscribePlus<'d, 's> (this.AllocSubscriptionId ()) event callback |> ignore

    member this.Unsubscribe event subscriptionId =
        let result =
            match subscriptions.TryGetValue event with
            | (true, callbacks) -> callbacks.Remove subscriptionId
            | (false, _) -> false
        if result then
            match event.EcsEventType with
            | ComponentEvent (entity, _) ->
                match subscribedEntities.TryGetValue entity with
                | (true, count) ->
                    if count = 1
                    then subscribedEntities.Remove entity |> ignore<bool>
                    else subscribedEntities.[entity] <- inc count
                | (false, _) -> failwith "Subscribed entities count mismatch."
            | _ -> failwith "Subscribed entities count mismatch."
        result

    member this.RegisterComponentName<'c when 'c : struct and 'c :> 'c Component> componentName =
        match componentTypes.TryGetValue componentName with
        | (true, _) -> failwith "Component type already registered."
        | (false, _) -> componentTypes.Add (componentName, typeof<'c>)

    member this.RegisterTerm (termName : string) term (entity : EcsEntity) =
        if termName.StartsWith Constants.Ecs.IntraComponentPrefix then failwith "Term names that start with '@' are for internal use only."
        if (match term with Intra _ -> true | _ -> false) then failwith "Intra components are for internal use only."
        match archetypeSlots.TryGetValue entity.EntityId with
        | (true, archetypeSlot) ->
            let comps = this.UnregisterEntityInternal archetypeSlot entity
            match term with Extra (compName, _, comp) -> comps.Add (compName, comp.Value) | _ -> ()
            let archetypeId = archetypeSlot.Archetype.Id.AddTerm termName term
            this.RegisterEntityInternal comps archetypeId entity
        | (false, _) ->
            let archetypeId = ArchetypeId.singleton termName term
            let comps = dictPlus StringComparer.Ordinal []
            match term with Extra (compName, _, comp) -> comps.Add (compName, comp.Value) | _ -> ()
            this.RegisterEntityInternal comps archetypeId entity

    member this.UnregisterTerm (termName : string) (entity : EcsEntity) =
        if termName.StartsWith Constants.Ecs.IntraComponentPrefix then failwith "Term names that start with '@' are for internal use only."
        match archetypeSlots.TryGetValue entity.EntityId with
        | (true, archetypeSlot) ->
            let comps = this.UnregisterEntityInternal archetypeSlot entity
            let archetypeId = archetypeSlot.Archetype.Id.RemoveTerm termName
            if archetypeId.Terms.Count > 0 then this.RegisterEntityInternal comps archetypeId entity
        | (false, _) -> ()

    member this.RegisterComponentPlus<'c, 's when 'c : struct and 'c :> 'c Component> compName (comp : 'c) (entity : EcsEntity) (state : 's) =
        match archetypeSlots.TryGetValue entity.EntityId with
        | (true, archetypeSlot) ->
            let comps = this.UnregisterEntityInternal archetypeSlot entity
            comps.Add (compName, comp)
            let archetypeId = archetypeSlot.Archetype.Id.AddTerm (Constants.Ecs.IntraComponentPrefix + compName) (Intra (compName, typeof<'c>))
            this.RegisterEntityInternal comps archetypeId entity
            let eventData = { EcsEntity = entity; ComponentName = compName }
            this.Publish<EcsRegistrationData, obj> (EcsEvents.Register entity compName) eventData (state :> obj) :?> 's
        | (false, _) ->
            let archetypeId = ArchetypeId.singleton (Constants.Ecs.IntraComponentPrefix + compName) (Intra (compName, typeof<'c>))
            let comps = Dictionary.singleton StringComparer.Ordinal compName (comp :> obj)
            this.RegisterEntityInternal comps archetypeId entity
            let eventData = { EcsEntity = entity; ComponentName = compName }
            this.Publish<EcsRegistrationData, obj> (EcsEvents.Register entity compName) eventData (state :> obj) :?> 's

    member this.RegisterComponent<'c, 's when 'c : struct and 'c :> 'c Component> (comp : 'c) (entity : EcsEntity) (state : 's) =
        this.RegisterComponentPlus<'c, 's> (typeof<'c>.Name) comp (entity : EcsEntity) state

    member this.UnregisterComponentPlus<'c, 's when 'c : struct and 'c :> 'c Component> compName (entity : EcsEntity) (state : 's) =
        match archetypeSlots.TryGetValue entity.EntityId with
        | (true, archetypeSlot) ->
            let eventData = { EcsEntity = entity; ComponentName = compName }
            let state = this.Publish<EcsRegistrationData, obj> (EcsEvents.Unregistering entity compName) eventData (state :> obj) :?> 's
            let comps = this.UnregisterEntityInternal archetypeSlot entity
            let archetypeId = archetypeSlot.Archetype.Id.RemoveTerm (Constants.Ecs.IntraComponentPrefix + compName)
            if archetypeId.Terms.Count > 0 then
                comps.Remove compName |> ignore<bool>
                this.RegisterEntityInternal comps archetypeId entity
                state
            else state
        | (false, _) -> state

    member this.UnregisterComponent<'c, 's when 'c : struct and 'c :> 'c Component> (entity : EcsEntity) (state : 's) =
        this.UnregisterComponentPlus<'c, 's> typeof<'c>.Name entity state

    member this.RegisterEntity elideEvents comps archetypeId state =
        let archetype =
            match archetypes.TryGetValue archetypeId with
            | (true, archetype) -> archetype
            | (false, _) -> createArchetype archetypeId
        let entity = this.Entity
        let archetypeIndex = archetype.Register comps
        archetypeSlots.Add (entity.EntityId, { ArchetypeIndex = archetypeIndex; Archetype = archetype })
        let mutable state = state
        if not elideEvents then
            for compName in archetype.Stores.Keys do
                let eventData = { EcsEntity = entity; ComponentName = compName }
                state <- this.Publish<EcsRegistrationData, obj> (EcsEvents.Unregistering entity compName) eventData (state :> obj) :?> 's
        (entity, state)

    member this.UnregisterEntity (entity : EcsEntity) (state : 's) =
        match archetypeSlots.TryGetValue entity.EntityId with
        | (true, archetypeSlot) ->
            let archetype = archetypeSlot.Archetype
            let mutable state = state
            if subscribedEntities.ContainsKey entity then
                for compName in archetype.Stores.Keys do
                    let eventData = { EcsEntity = entity; ComponentName = compName }
                    state <- this.Publish<EcsRegistrationData, obj> (EcsEvents.Unregistering entity compName) eventData (state :> obj) :?> 's
            archetype.Unregister archetypeSlot.ArchetypeIndex
            state
        | (false, _) -> state

    member this.RegisterEntitiesPlus elideEvents count comps archetypeId state =

        // get archetype
        let archetype =
            match archetypes.TryGetValue archetypeId with
            | (true, archetype) -> archetype
            | (false, _) -> createArchetype archetypeId

        // register entities to archetype
        let mutable state = state
        let entitys = SegmentedArray.zeroCreate count
        for i in 0 .. dec count do
            let entity = this.Entity
            let archetypeIndex = archetype.Register comps
            archetypeSlots.Add (entity.EntityId, { ArchetypeIndex = archetypeIndex; Archetype = archetype })
            entitys.[i] <- entity
            if not elideEvents then
                for compName in archetype.Stores.Keys do
                    let eventData = { EcsEntity = entity; ComponentName = compName }
                    state <- this.Publish<EcsRegistrationData, obj> (EcsEvents.Unregistering entity compName) eventData (state :> obj) :?> 's

        // fin
        (entitys, state)

    member this.RegisterEntities elideEvents count comps archetypeId state =
        let comps = dictPlus StringComparer.Ordinal (Seq.map (fun comp -> (getTypeName comp, comp)) comps)
        this.RegisterEntitiesPlus elideEvents count comps archetypeId state

    member this.RegisterQuery (query : Query) =
        for archetypeEntry in archetypes do
            query.TryRegisterArchetype archetypeEntry.Value
        queries.Add query
        query

    member this.ReadComponents count archetypeId stream =
        let archetype =
            match archetypes.TryGetValue archetypeId with
            | (true, archetype) -> archetype
            | (false, _) -> createArchetype archetypeId
        let (firstIndex, lastIndex) = archetype.Read count stream
        let entitys = SegmentedArray.zeroCreate count
        for i in firstIndex .. lastIndex do
            let entity = this.Entity
            archetypeSlots.Add (entity.EntityId, { ArchetypeIndex = i; Archetype = archetype })
            entitys.[i - firstIndex] <- entity
        entitys

and [<StructuralEquality; NoComparison; Struct>] EcsEntity =
    { EntityId : uint64
      Ecs : Ecs }

    member inline private this.IndexStore<'c when 'c : struct and 'c :> 'c Component> compName archetypeId (stores : Dictionary<string, Store>) =
        match stores.TryGetValue compName with
        | (true, store) -> store :?> 'c Store
        | (false, _) -> failwith ("Invalid entity frame for archetype " + scstring archetypeId + ".")

    member this.RegisterPlus<'c, 's when 'c : struct and 'c :> 'c Component> compName (comp : 'c) (state : 's) =
        this.Ecs.RegisterComponentPlus<'c, 's> compName comp this state

    member this.Register<'c, 's when 'c : struct and 'c :> 'c Component> (comp : 'c) (state : 's) =
        this.Ecs.RegisterComponent<'c, 's> comp this state

    member this.UnregisterPlus<'c, 's when 'c : struct and 'c :> 'c Component> compName (state : 's) =
        this.Ecs.UnregisterComponentPlus<'c, 's> compName this state

    member this.Unregister<'c, 's when 'c : struct and 'c :> 'c Component> (state : 's) =
        this.Ecs.UnregisterComponent<'c, 's> this state

    member this.RegisterTerm termName term =
        this.Ecs.RegisterTerm termName term this

    member this.UnregisterTerm termName =
        this.Ecs.UnregisterTerm termName this

    member this.ValidatePlus compName =
        let archetypeSlot = this.Ecs.IndexArchetypeSlot this
        let stores = archetypeSlot.Archetype.Stores
        stores.ContainsKey compName

    member this.Validate<'c when 'c : struct and 'c :> 'c Component> () =
        this.ValidatePlus typeof<'c>.Name

    member this.ValidateTerm termName =
        let archetypeSlot = this.Ecs.IndexArchetypeSlot this
        let terms = archetypeSlot.Archetype.Id.Terms
        terms.ContainsKey termName

    member this.IndexPlus<'c when 'c : struct and 'c :> 'c Component> compName =
        let archetypeSlot = this.Ecs.IndexArchetypeSlot this
        let stores = archetypeSlot.Archetype.Stores
        let store = stores.[compName] :?> 'c Store
        let i = archetypeSlot.ArchetypeIndex
        &store.[i]

    member this.Index<'c when 'c : struct and 'c :> 'c Component> () =
        this.IndexPlus<'c> typeof<'c>.Name

    member this.IndexTerm termName =
        let archetypeSlot = this.Ecs.IndexArchetypeSlot this
        let terms = archetypeSlot.Archetype.Id.Terms
        terms.[termName]

    member this.MutatePlus<'c when 'c : struct and 'c :> 'c Component> compName (comp : 'c) =
        let archetypeSlot = this.Ecs.IndexArchetypeSlot this
        let stores = archetypeSlot.Archetype.Stores
        let store = stores.[compName] :?> 'c Store
        let i = archetypeSlot.ArchetypeIndex
        store.[i] <- comp

    member this.Mutate<'c when 'c : struct and 'c :> 'c Component> (comp : 'c) =
        this.MutatePlus<'c> typeof<'c>.Name comp

    member this.ChangePlus<'c, 's when 'c : struct and 'c :> 'c Component> compName (comp : 'c) (state : 's) =
        let archetypeSlot = this.Ecs.IndexArchetypeSlot this
        let stores = archetypeSlot.Archetype.Stores
        let store = stores.[compName] :?> 'c Store
        let i = archetypeSlot.ArchetypeIndex
        store.[i] <- comp
        this.Ecs.Publish (EcsEvents.Change this) { EcsEntity = this; ComponentName = compName } state

    member this.Change<'c, 's when 'c : struct and 'c :> 'c Component> (comp : 'c) (state : 's) =
        this.ChangePlus<'c, 's> typeof<'c>.Name comp state

    member this.Frame (compName, state : 's, statement : Statement<'c, 's>) =
        let archetypeSlot = this.Ecs.IndexArchetypeSlot this
        let archetype = archetypeSlot.Archetype
        let archetypeId = archetype.Id
        let stores = archetype.Stores
        let store = this.IndexStore<'c> compName archetypeId stores
        let i = archetypeSlot.ArchetypeIndex
        statement.Invoke (&store.[i], state)

    member this.Frame (compName, comp2Name, state : 's, statement : Statement<'c, 'c2, 's>) =
        let archetypeSlot = this.Ecs.IndexArchetypeSlot this
        let archetype = archetypeSlot.Archetype
        let archetypeId = archetype.Id
        let stores = archetype.Stores
        let store = this.IndexStore<'c> compName archetypeId stores
        let store2 = this.IndexStore<'c2> comp2Name archetypeId stores
        let i = archetypeSlot.ArchetypeIndex
        statement.Invoke (&store.[i], &store2.[i], state)

    member this.Frame (compName, comp2Name, comp3Name, state : 's, statement : Statement<'c, 'c2, 'c3, 's>) =
        let archetypeSlot = this.Ecs.IndexArchetypeSlot this
        let archetype = archetypeSlot.Archetype
        let archetypeId = archetype.Id
        let stores = archetype.Stores
        let store = this.IndexStore<'c> compName archetypeId stores
        let store2 = this.IndexStore<'c2> comp2Name archetypeId stores
        let store3 = this.IndexStore<'c3> comp3Name archetypeId stores
        let i = archetypeSlot.ArchetypeIndex
        statement.Invoke (&store.[i], &store2.[i], &store3.[i], state)

    member this.Frame (compName, comp2Name, comp3Name, comp4Name, state : 's, statement : Statement<'c, 'c2, 'c3, 'c4, 's>) =
        let archetypeSlot = this.Ecs.IndexArchetypeSlot this
        let archetype = archetypeSlot.Archetype
        let archetypeId = archetype.Id
        let stores = archetype.Stores
        let store = this.IndexStore<'c> compName archetypeId stores
        let store2 = this.IndexStore<'c2> comp2Name archetypeId stores
        let store3 = this.IndexStore<'c3> comp3Name archetypeId stores
        let store4 = this.IndexStore<'c4> comp4Name archetypeId stores
        let i = archetypeSlot.ArchetypeIndex
        statement.Invoke (&store.[i], &store2.[i], &store3.[i], &store4.[i], state)

    member this.Frame (compName, comp2Name, comp3Name, comp4Name, comp5Name, state : 's, statement : Statement<'c, 'c2, 'c3, 'c4, 'c5, 's>) =
        let archetypeSlot = this.Ecs.IndexArchetypeSlot this
        let archetype = archetypeSlot.Archetype
        let archetypeId = archetype.Id
        let stores = archetype.Stores
        let store = this.IndexStore<'c> compName archetypeId stores
        let store2 = this.IndexStore<'c2> comp2Name archetypeId stores
        let store3 = this.IndexStore<'c3> comp3Name archetypeId stores
        let store4 = this.IndexStore<'c4> comp4Name archetypeId stores
        let store5 = this.IndexStore<'c5> comp5Name archetypeId stores
        let i = archetypeSlot.ArchetypeIndex
        statement.Invoke (&store.[i], &store2.[i], &store3.[i], &store4.[i], &store5.[i], state)

    member this.Frame (compName, comp2Name, comp3Name, comp4Name, comp5Name, comp6Name, state : 's, statement : Statement<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 's>) =
        let archetypeSlot = this.Ecs.IndexArchetypeSlot this
        let archetype = archetypeSlot.Archetype
        let archetypeId = archetype.Id
        let stores = archetype.Stores
        let store = this.IndexStore<'c> compName archetypeId stores
        let store2 = this.IndexStore<'c2> comp2Name archetypeId stores
        let store3 = this.IndexStore<'c3> comp3Name archetypeId stores
        let store4 = this.IndexStore<'c4> comp4Name archetypeId stores
        let store5 = this.IndexStore<'c5> comp5Name archetypeId stores
        let store6 = this.IndexStore<'c6> comp6Name archetypeId stores
        let i = archetypeSlot.ArchetypeIndex
        statement.Invoke (&store.[i], &store2.[i], &store3.[i], &store4.[i], &store5.[i], &store6.[i], state)

    member this.Frame (compName, comp2Name, comp3Name, comp4Name, comp5Name, comp6Name, comp7Name, state : 's, statement : Statement<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'c7, 's>) =
        let archetypeSlot = this.Ecs.IndexArchetypeSlot this
        let archetype = archetypeSlot.Archetype
        let archetypeId = archetype.Id
        let stores = archetype.Stores
        let store = this.IndexStore<'c> compName archetypeId stores
        let store2 = this.IndexStore<'c2> comp2Name archetypeId stores
        let store3 = this.IndexStore<'c3> comp3Name archetypeId stores
        let store4 = this.IndexStore<'c4> comp4Name archetypeId stores
        let store5 = this.IndexStore<'c5> comp5Name archetypeId stores
        let store6 = this.IndexStore<'c6> comp6Name archetypeId stores
        let store7 = this.IndexStore<'c7> comp7Name archetypeId stores
        let i = archetypeSlot.ArchetypeIndex
        statement.Invoke (&store.[i], &store2.[i], &store3.[i], &store4.[i], &store5.[i], &store6.[i], &store7.[i], state)

    member this.Frame (compName, comp2Name, comp3Name, comp4Name, comp5Name, comp6Name, comp7Name, comp8Name, state : 's, statement : Statement<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'c7, 'c8, 's>) =
        let archetypeSlot = this.Ecs.IndexArchetypeSlot this
        let archetype = archetypeSlot.Archetype
        let archetypeId = archetype.Id
        let stores = archetype.Stores
        let store = this.IndexStore<'c> compName archetypeId stores
        let store2 = this.IndexStore<'c2> comp2Name archetypeId stores
        let store3 = this.IndexStore<'c3> comp3Name archetypeId stores
        let store4 = this.IndexStore<'c4> comp4Name archetypeId stores
        let store5 = this.IndexStore<'c5> comp5Name archetypeId stores
        let store6 = this.IndexStore<'c6> comp6Name archetypeId stores
        let store7 = this.IndexStore<'c7> comp7Name archetypeId stores
        let store8 = this.IndexStore<'c8> comp8Name archetypeId stores
        let i = archetypeSlot.ArchetypeIndex
        statement.Invoke (&store.[i], &store2.[i], &store3.[i], &store4.[i], &store5.[i], &store6.[i], &store7.[i], &store8.[i], state)

    member this.Frame (compName, comp2Name, comp3Name, comp4Name, comp5Name, comp6Name, comp7Name, comp8Name, comp9Name, state : 's, statement : Statement<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'c7, 'c8, 'c9, 's>) =
        let archetypeSlot = this.Ecs.IndexArchetypeSlot this
        let archetype = archetypeSlot.Archetype
        let archetypeId = archetype.Id
        let stores = archetype.Stores
        let store = this.IndexStore<'c> compName archetypeId stores
        let store2 = this.IndexStore<'c2> comp2Name archetypeId stores
        let store3 = this.IndexStore<'c3> comp3Name archetypeId stores
        let store4 = this.IndexStore<'c4> comp4Name archetypeId stores
        let store5 = this.IndexStore<'c5> comp5Name archetypeId stores
        let store6 = this.IndexStore<'c6> comp6Name archetypeId stores
        let store7 = this.IndexStore<'c7> comp7Name archetypeId stores
        let store8 = this.IndexStore<'c8> comp8Name archetypeId stores
        let store9 = this.IndexStore<'c9> comp9Name archetypeId stores
        let i = archetypeSlot.ArchetypeIndex
        statement.Invoke (&store.[i], &store2.[i], &store3.[i], &store4.[i], &store5.[i], &store6.[i], &store7.[i], &store8.[i], &store9.[i], state)

    member this.Frame<'c, 's when
        'c : struct and 'c :> 'c Component>
        (state : 's, statement : Statement<'c, 's>) =
        this.Frame (typeof<'c>.Name, state, statement)

    member this.Frame<'c, 'c2, 's when
        'c : struct and 'c :> 'c Component and
        'c2 : struct and 'c2 :> 'c2 Component>
        (state : 's, statement : Statement<'c, 'c2, 's>) =
        this.Frame (typeof<'c>.Name, typeof<'c2>.Name, state, statement)

    member this.Frame<'c, 'c2, 'c3, 's when
        'c : struct and 'c :> 'c Component and
        'c2 : struct and 'c2 :> 'c2 Component and
        'c3 : struct and 'c3 :> 'c3 Component>
        (state : 's, statement : Statement<'c, 'c2, 'c3, 's>) =
        this.Frame (typeof<'c>.Name, typeof<'c2>.Name, typeof<'c3>.Name, state, statement)

    member this.Frame<'c, 'c2, 'c3, 'c4, 's when
        'c : struct and 'c :> 'c Component and
        'c2 : struct and 'c2 :> 'c2 Component and
        'c3 : struct and 'c3 :> 'c3 Component and
        'c4 : struct and 'c4 :> 'c4 Component>
        (state : 's, statement : Statement<'c, 'c2, 'c3, 'c4, 's>) =
        this.Frame (typeof<'c>.Name, typeof<'c2>.Name, typeof<'c3>.Name, typeof<'c4>.Name, state, statement)

    member this.Frame<'c, 'c2, 'c3, 'c4, 'c5, 's when
        'c : struct and 'c :> 'c Component and
        'c2 : struct and 'c2 :> 'c2 Component and
        'c3 : struct and 'c3 :> 'c3 Component and
        'c4 : struct and 'c4 :> 'c4 Component and
        'c5 : struct and 'c5 :> 'c5 Component>
        (state : 's, statement : Statement<'c, 'c2, 'c3, 'c4, 'c5, 's>) =
        this.Frame (typeof<'c>.Name, typeof<'c2>.Name, typeof<'c3>.Name, typeof<'c4>.Name, typeof<'c5>.Name, state, statement)

    member this.Frame<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 's when
        'c : struct and 'c :> 'c Component and
        'c2 : struct and 'c2 :> 'c2 Component and
        'c3 : struct and 'c3 :> 'c3 Component and
        'c4 : struct and 'c4 :> 'c4 Component and
        'c5 : struct and 'c5 :> 'c5 Component and
        'c6 : struct and 'c6 :> 'c6 Component>
        (state : 's, statement : Statement<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 's>) =
        this.Frame (typeof<'c>.Name, typeof<'c2>.Name, typeof<'c3>.Name, typeof<'c4>.Name, typeof<'c5>.Name, typeof<'c6>.Name, state, statement)

    member this.Frame<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'c7, 's when
        'c : struct and 'c :> 'c Component and
        'c2 : struct and 'c2 :> 'c2 Component and
        'c3 : struct and 'c3 :> 'c3 Component and
        'c4 : struct and 'c4 :> 'c4 Component and
        'c5 : struct and 'c5 :> 'c5 Component and
        'c6 : struct and 'c6 :> 'c6 Component and
        'c7 : struct and 'c7 :> 'c7 Component>
        (state : 's, statement : Statement<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'c7, 's>) =
        this.Frame (typeof<'c>.Name, typeof<'c2>.Name, typeof<'c3>.Name, typeof<'c4>.Name, typeof<'c5>.Name, typeof<'c6>.Name, typeof<'c7>.Name, state, statement)

    member this.Frame<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'c7, 'c8, 's when
        'c : struct and 'c :> 'c Component and
        'c2 : struct and 'c2 :> 'c2 Component and
        'c3 : struct and 'c3 :> 'c3 Component and
        'c4 : struct and 'c4 :> 'c4 Component and
        'c5 : struct and 'c5 :> 'c5 Component and
        'c6 : struct and 'c6 :> 'c6 Component and
        'c7 : struct and 'c7 :> 'c7 Component and
        'c8 : struct and 'c8 :> 'c8 Component>
        (state : 's, statement : Statement<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'c7, 'c8, 's>) =
        this.Frame (typeof<'c>.Name, typeof<'c2>.Name, typeof<'c3>.Name, typeof<'c4>.Name, typeof<'c5>.Name, typeof<'c6>.Name, typeof<'c7>.Name, typeof<'c8>.Name, state, statement)

    member this.Frame<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'c7, 'c8, 'c9, 's when
        'c : struct and 'c :> 'c Component and
        'c2 : struct and 'c2 :> 'c2 Component and
        'c3 : struct and 'c3 :> 'c3 Component and
        'c4 : struct and 'c4 :> 'c4 Component and
        'c5 : struct and 'c5 :> 'c5 Component and
        'c6 : struct and 'c6 :> 'c6 Component and
        'c7 : struct and 'c7 :> 'c7 Component and
        'c8 : struct and 'c8 :> 'c8 Component and
        'c9 : struct and 'c9 :> 'c9 Component>
        (state : 's, statement : Statement<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'c7, 'c8, 'c9, 's>) =
        this.Frame (typeof<'c>.Name, typeof<'c2>.Name, typeof<'c3>.Name, typeof<'c4>.Name, typeof<'c5>.Name, typeof<'c6>.Name, typeof<'c7>.Name, typeof<'c8>.Name, typeof<'c9>.Name, state, statement)

and Query (compNames : string HashSet, subqueries : Subquery seq) =

    let archetypes = dictPlus<ArchetypeId, Archetype> HashIdentity.Structural []
    let subqueries = List subqueries

    do
        for compName in compNames do
            subqueries.Add (Is (Constants.Ecs.IntraComponentPrefix + compName))

    member inline private this.IndexStore<'c when 'c : struct and 'c :> 'c Component> compName archetypeId (stores : Dictionary<string, Store>) =
        match stores.TryGetValue compName with
        | (true, store) -> store :?> 'c Store
        | (false, _) -> failwith ("Invalid entity frame for archetype " + scstring archetypeId + ".")

    member this.Subqueries =
        seq subqueries

    member this.TryRegisterArchetype (archetype : Archetype) =
        if  not (archetypes.ContainsKey archetype.Id) &&
            Subquery.evalMany archetype.Id.Terms subqueries then
            archetypes.Add (archetype.Id, archetype)

    member this.Iterate (compName, statement : Statement<'c, 's>) : 's -> 's =
        fun state ->
            let mutable state = state
            for archetypeEntry in archetypes do
                let archetype = archetypeEntry.Value
                let archetypeId = archetype.Id
                let length = archetype.Length
                let stores = archetype.Stores
                let store = this.IndexStore<'c> compName archetypeId stores
                let mutable i = 0
                while i < store.Length && i < length do
                    let comp = &store.[i]
                    if comp.Active then
                        state <- statement.Invoke (&comp, state)
                        i <- inc i
            state

    member this.Iterate (compName, comp2Name, statement : Statement<'c, 'c2, 's>) : 's -> 's =
        fun state ->
            let mutable state = state
            for archetypeEntry in archetypes do
                let archetype = archetypeEntry.Value
                let archetypeId = archetype.Id
                let length = archetype.Length
                let stores = archetype.Stores
                let store = this.IndexStore<'c> compName archetypeId stores
                let store2 = this.IndexStore<'c2> comp2Name archetypeId stores
                let mutable i = 0
                while i < store.Length && i < length do
                    let comp = &store.[i]
                    if comp.Active then
                        state <- statement.Invoke (&comp, &store2.[i], state)
                        i <- inc i
            state

    member this.Iterate (compName, comp2Name, comp3Name, statement : Statement<'c, 'c2, 'c3, 's>) : 's -> 's =
        fun state ->
            let mutable state = state
            for archetypeEntry in archetypes do
                let archetype = archetypeEntry.Value
                let archetypeId = archetype.Id
                let length = archetype.Length
                let stores = archetype.Stores
                let store = this.IndexStore<'c> compName archetypeId stores
                let store2 = this.IndexStore<'c2> comp2Name archetypeId stores
                let store3 = this.IndexStore<'c3> comp3Name archetypeId stores
                let mutable i = 0
                while i < store.Length && i < length do
                    let comp = &store.[i]
                    if comp.Active then
                        state <- statement.Invoke (&comp, &store2.[i], &store3.[i], state)
                        i <- inc i
            state

    member this.Iterate (compName, comp2Name, comp3Name, comp4Name, statement : Statement<'c, 'c2, 'c3, 'c4, 's>) : 's -> 's =
        fun state ->
            let mutable state = state
            for archetypeEntry in archetypes do
                let archetype = archetypeEntry.Value
                let archetypeId = archetype.Id
                let length = archetype.Length
                let stores = archetype.Stores
                let store = this.IndexStore<'c> compName archetypeId stores
                let store2 = this.IndexStore<'c2> comp2Name archetypeId stores
                let store3 = this.IndexStore<'c3> comp3Name archetypeId stores
                let store4 = this.IndexStore<'c4> comp4Name archetypeId stores
                let mutable i = 0
                while i < store.Length && i < length do
                    let comp = &store.[i]
                    if comp.Active then
                        state <- statement.Invoke (&comp, &store2.[i], &store3.[i], &store4.[i], state)
                        i <- inc i
            state

    member this.Iterate (compName, comp2Name, comp3Name, comp4Name, comp5Name, statement : Statement<'c, 'c2, 'c3, 'c4, 'c5, 's>) : 's -> 's =
        fun state ->
            let mutable state = state
            for archetypeEntry in archetypes do
                let archetype = archetypeEntry.Value
                let archetypeId = archetype.Id
                let length = archetype.Length
                let stores = archetype.Stores
                let store = this.IndexStore<'c> compName archetypeId stores
                let store2 = this.IndexStore<'c2> comp2Name archetypeId stores
                let store3 = this.IndexStore<'c3> comp3Name archetypeId stores
                let store4 = this.IndexStore<'c4> comp4Name archetypeId stores
                let store5 = this.IndexStore<'c5> comp5Name archetypeId stores
                let mutable i = 0
                while i < store.Length && i < length do
                    let comp = &store.[i]
                    if comp.Active then
                        state <- statement.Invoke (&comp, &store2.[i], &store3.[i], &store4.[i], &store5.[i], state)
                        i <- inc i
            state

    member this.Iterate (compName, comp2Name, comp3Name, comp4Name, comp5Name, comp6Name, statement : Statement<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 's>) : 's -> 's =
        fun state ->
            let mutable state = state
            for archetypeEntry in archetypes do
                let archetype = archetypeEntry.Value
                let archetypeId = archetype.Id
                let length = archetype.Length
                let stores = archetype.Stores
                let store = this.IndexStore<'c> compName archetypeId stores
                let store2 = this.IndexStore<'c2> comp2Name archetypeId stores
                let store3 = this.IndexStore<'c3> comp3Name archetypeId stores
                let store4 = this.IndexStore<'c4> comp4Name archetypeId stores
                let store5 = this.IndexStore<'c5> comp5Name archetypeId stores
                let store6 = this.IndexStore<'c6> comp6Name archetypeId stores
                let mutable i = 0
                while i < store.Length && i < length do
                    let comp = &store.[i]
                    if comp.Active then
                        state <- statement.Invoke (&comp, &store2.[i], &store3.[i], &store4.[i], &store5.[i], &store6.[i], state)
                        i <- inc i
            state

    member this.Iterate (compName, comp2Name, comp3Name, comp4Name, comp5Name, comp6Name, comp7Name, statement : Statement<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'c7, 's>) : 's -> 's =
        fun state ->
            let mutable state = state
            for archetypeEntry in archetypes do
                let archetype = archetypeEntry.Value
                let archetypeId = archetype.Id
                let length = archetype.Length
                let stores = archetype.Stores
                let store = this.IndexStore<'c> compName archetypeId stores
                let store2 = this.IndexStore<'c2> comp2Name archetypeId stores
                let store3 = this.IndexStore<'c3> comp3Name archetypeId stores
                let store4 = this.IndexStore<'c4> comp4Name archetypeId stores
                let store5 = this.IndexStore<'c5> comp5Name archetypeId stores
                let store6 = this.IndexStore<'c6> comp6Name archetypeId stores
                let store7 = this.IndexStore<'c7> comp7Name archetypeId stores
                let mutable i = 0
                while i < store.Length && i < length do
                    let comp = &store.[i]
                    if comp.Active then
                        state <- statement.Invoke (&comp, &store2.[i], &store3.[i], &store4.[i], &store5.[i], &store6.[i], &store7.[i], state)
                        i <- inc i
            state

    member this.Iterate (compName, comp2Name, comp3Name, comp4Name, comp5Name, comp6Name, comp7Name, comp8Name, statement : Statement<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'c7, 'c8, 's>) : 's -> 's =
        fun state ->
            let mutable state = state
            for archetypeEntry in archetypes do
                let archetype = archetypeEntry.Value
                let archetypeId = archetype.Id
                let length = archetype.Length
                let stores = archetype.Stores
                let store = this.IndexStore<'c> compName archetypeId stores
                let store2 = this.IndexStore<'c2> comp2Name archetypeId stores
                let store3 = this.IndexStore<'c3> comp3Name archetypeId stores
                let store4 = this.IndexStore<'c4> comp4Name archetypeId stores
                let store5 = this.IndexStore<'c5> comp5Name archetypeId stores
                let store6 = this.IndexStore<'c6> comp6Name archetypeId stores
                let store7 = this.IndexStore<'c7> comp7Name archetypeId stores
                let store8 = this.IndexStore<'c8> comp8Name archetypeId stores
                let mutable i = 0
                while i < store.Length && i < length do
                    let comp = &store.[i]
                    if comp.Active then
                        state <- statement.Invoke (&comp, &store2.[i], &store3.[i], &store4.[i], &store5.[i], &store6.[i], &store7.[i], &store8.[i], state)
                        i <- inc i
            state

    member this.Iterate (compName, comp2Name, comp3Name, comp4Name, comp5Name, comp6Name, comp7Name, comp8Name, comp9Name, statement : Statement<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'c7, 'c8, 'c9, 's>) : 's -> 's =
        fun state ->
            let mutable state = state
            for archetypeEntry in archetypes do
                let archetype = archetypeEntry.Value
                let archetypeId = archetype.Id
                let length = archetype.Length
                let stores = archetype.Stores
                let store = this.IndexStore<'c> compName archetypeId stores
                let store2 = this.IndexStore<'c2> comp2Name archetypeId stores
                let store3 = this.IndexStore<'c3> comp3Name archetypeId stores
                let store4 = this.IndexStore<'c4> comp4Name archetypeId stores
                let store5 = this.IndexStore<'c5> comp5Name archetypeId stores
                let store6 = this.IndexStore<'c6> comp6Name archetypeId stores
                let store7 = this.IndexStore<'c7> comp7Name archetypeId stores
                let store8 = this.IndexStore<'c8> comp8Name archetypeId stores
                let store9 = this.IndexStore<'c9> comp9Name archetypeId stores
                let mutable i = 0
                while i < store.Length && i < length do
                    let comp = &store.[i]
                    if comp.Active then
                        state <- statement.Invoke (&comp, &store2.[i], &store3.[i], &store4.[i], &store5.[i], &store6.[i], &store7.[i], &store8.[i], &store9.[i], state)
                        i <- inc i
            state

    member this.Iterate<'c, 's when
        'c : struct and 'c :> 'c Component>
        (statement : Statement<'c, 's>) : 's -> 's =
        this.Iterate (typeof<'c>.Name, statement)

    member this.Iterate<'c, 'c2, 's when
        'c : struct and 'c :> 'c Component and
        'c2 : struct and 'c2 :> 'c2 Component>
        (statement : Statement<'c, 'c2, 's>) : 's -> 's =
        this.Iterate (typeof<'c>.Name, typeof<'c2>.Name, statement)

    member this.Iterate<'c, 'c2, 'c3, 's when
        'c : struct and 'c :> 'c Component and
        'c2 : struct and 'c2 :> 'c2 Component and
        'c3 : struct and 'c3 :> 'c3 Component>
        (statement : Statement<'c, 'c2, 'c3, 's>) : 's -> 's =
        this.Iterate (typeof<'c>.Name, typeof<'c2>.Name, typeof<'c3>.Name, statement)

    member this.Iterate<'c, 'c2, 'c3, 'c4, 's when
        'c : struct and 'c :> 'c Component and
        'c2 : struct and 'c2 :> 'c2 Component and
        'c3 : struct and 'c3 :> 'c3 Component and
        'c4 : struct and 'c4 :> 'c4 Component>
        (statement : Statement<'c, 'c2, 'c3, 'c4, 's>) : 's -> 's =
        this.Iterate (typeof<'c>.Name, typeof<'c2>.Name, typeof<'c3>.Name, typeof<'c4>.Name, statement)

    member this.Iterate<'c, 'c2, 'c3, 'c4, 'c5, 's when
        'c : struct and 'c :> 'c Component and
        'c2 : struct and 'c2 :> 'c2 Component and
        'c3 : struct and 'c3 :> 'c3 Component and
        'c4 : struct and 'c4 :> 'c4 Component and
        'c5 : struct and 'c5 :> 'c5 Component>
        (statement : Statement<'c, 'c2, 'c3, 'c4, 'c5, 's>) : 's -> 's =
        this.Iterate (typeof<'c>.Name, typeof<'c2>.Name, typeof<'c3>.Name, typeof<'c4>.Name, typeof<'c5>.Name, statement)

    member this.Iterate<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 's when
        'c : struct and 'c :> 'c Component and
        'c2 : struct and 'c2 :> 'c2 Component and
        'c3 : struct and 'c3 :> 'c3 Component and
        'c4 : struct and 'c4 :> 'c4 Component and
        'c5 : struct and 'c5 :> 'c5 Component and
        'c6 : struct and 'c6 :> 'c6 Component>
        (statement : Statement<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 's>) : 's -> 's =
        this.Iterate (typeof<'c>.Name, typeof<'c2>.Name, typeof<'c3>.Name, typeof<'c4>.Name, typeof<'c5>.Name, typeof<'c6>.Name, statement)

    member this.Iterate<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'c7, 's when
        'c : struct and 'c :> 'c Component and
        'c2 : struct and 'c2 :> 'c2 Component and
        'c3 : struct and 'c3 :> 'c3 Component and
        'c4 : struct and 'c4 :> 'c4 Component and
        'c5 : struct and 'c5 :> 'c5 Component and
        'c6 : struct and 'c6 :> 'c6 Component and
        'c7 : struct and 'c7 :> 'c7 Component>
        (statement : Statement<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'c7, 's>) : 's -> 's =
        this.Iterate (typeof<'c>.Name, typeof<'c2>.Name, typeof<'c3>.Name, typeof<'c4>.Name, typeof<'c5>.Name, typeof<'c6>.Name, typeof<'c7>.Name, statement)

    member this.Iterate<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'c7, 'c8, 's when
        'c : struct and 'c :> 'c Component and
        'c2 : struct and 'c2 :> 'c2 Component and
        'c3 : struct and 'c3 :> 'c3 Component and
        'c4 : struct and 'c4 :> 'c4 Component and
        'c5 : struct and 'c5 :> 'c5 Component and
        'c6 : struct and 'c6 :> 'c6 Component and
        'c7 : struct and 'c7 :> 'c7 Component and
        'c8 : struct and 'c8 :> 'c8 Component>
        (statement : Statement<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'c7, 'c8, 's>) : 's -> 's =
        this.Iterate (typeof<'c>.Name, typeof<'c2>.Name, typeof<'c3>.Name, typeof<'c4>.Name, typeof<'c5>.Name, typeof<'c6>.Name, typeof<'c7>.Name, typeof<'c8>.Name, statement)

    member this.Iterate<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'c7, 'c8, 'c9, 's when
        'c : struct and 'c :> 'c Component and
        'c2 : struct and 'c2 :> 'c2 Component and
        'c3 : struct and 'c3 :> 'c3 Component and
        'c4 : struct and 'c4 :> 'c4 Component and
        'c5 : struct and 'c5 :> 'c5 Component and
        'c6 : struct and 'c6 :> 'c6 Component and
        'c7 : struct and 'c7 :> 'c7 Component and
        'c8 : struct and 'c8 :> 'c8 Component and
        'c9 : struct and 'c9 :> 'c9 Component>
        (statement : Statement<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'c7, 'c8, 'c9, 's>) : 's -> 's =
        this.Iterate (typeof<'c>.Name, typeof<'c2>.Name, typeof<'c3>.Name, typeof<'c4>.Name, typeof<'c5>.Name, typeof<'c6>.Name, typeof<'c7>.Name, typeof<'c8>.Name, typeof<'c9>.Name, statement)

    static member byName
        (compName, subqueries) =
        Query (HashSet.singleton HashIdentity.Structural compName, subqueries)

    static member byName
        (compName, comp2Name, subqueries) =
        Query (hashSetPlus HashIdentity.Structural [compName; comp2Name], subqueries)

    static member byName
        (compName, comp2Name, comp3Name, subqueries) =
        Query (hashSetPlus HashIdentity.Structural [compName; comp2Name; comp3Name], subqueries)

    static member byName
        (compName, comp2Name, comp3Name, comp4Name, subqueries) =
        Query (hashSetPlus HashIdentity.Structural [compName; comp2Name; comp3Name; comp4Name], subqueries)

    static member byName
        (compName, comp2Name, comp3Name, comp4Name, comp5Name, subqueries) =
        Query (hashSetPlus HashIdentity.Structural [compName; comp2Name; comp3Name; comp4Name; comp5Name], subqueries)

    static member byName
        (compName, comp2Name, comp3Name, comp4Name, comp5Name, comp6Name, subqueries) =
        Query (hashSetPlus HashIdentity.Structural [compName; comp2Name; comp3Name; comp4Name; comp5Name; comp6Name], subqueries)

    static member byName
        (compName, comp2Name, comp3Name, comp4Name, comp5Name, comp6Name, comp7Name, subqueries) =
        Query (hashSetPlus HashIdentity.Structural [compName; comp2Name; comp3Name; comp4Name; comp5Name; comp6Name; comp7Name], subqueries)

    static member byName
        (compName, comp2Name, comp3Name, comp4Name, comp5Name, comp6Name, comp7Name, comp8Name, subqueries) =
        Query (hashSetPlus HashIdentity.Structural [compName; comp2Name; comp3Name; comp4Name; comp5Name; comp6Name; comp7Name; comp8Name], subqueries)

    static member byName
        (compName, comp2Name, comp3Name, comp4Name, comp5Name, comp6Name, comp7Name, comp8Name, comp9Name, subqueries) =
        Query (hashSetPlus HashIdentity.Structural [compName; comp2Name; comp3Name; comp4Name; comp5Name; comp6Name; comp7Name; comp8Name; comp9Name], subqueries)

    static member byType<'c when
        'c : struct and 'c :> 'c Component>
        subqueries =
        Query (HashSet.singleton HashIdentity.Structural typeof<'c>.Name, subqueries)

    static member byType<'c, 'c2 when
        'c : struct and 'c :> 'c Component and
        'c2 : struct and 'c2 :> 'c2 Component>
        subqueries =
        Query (hashSetPlus HashIdentity.Structural [typeof<'c>.Name; typeof<'c2>.Name], subqueries)

    static member byType<'c, 'c2, 'c3 when
        'c : struct and 'c :> 'c Component and
        'c2 : struct and 'c2 :> 'c2 Component and
        'c3 : struct and 'c3 :> 'c3 Component>
        subqueries =
        Query (hashSetPlus HashIdentity.Structural [typeof<'c>.Name; typeof<'c2>.Name; typeof<'c3>.Name], subqueries)

    static member byType<'c, 'c2, 'c3, 'c4 when
        'c : struct and 'c :> 'c Component and
        'c2 : struct and 'c2 :> 'c2 Component and
        'c3 : struct and 'c3 :> 'c3 Component and
        'c4 : struct and 'c4 :> 'c4 Component>
        subqueries =
        Query (hashSetPlus HashIdentity.Structural [typeof<'c>.Name; typeof<'c2>.Name; typeof<'c3>.Name; typeof<'c4>.Name], subqueries)

    static member byType<'c, 'c2, 'c3, 'c4, 'c5 when
        'c : struct and 'c :> 'c Component and
        'c2 : struct and 'c2 :> 'c2 Component and
        'c3 : struct and 'c3 :> 'c3 Component and
        'c4 : struct and 'c4 :> 'c4 Component and
        'c5 : struct and 'c5 :> 'c5 Component>
        subqueries =
        Query (hashSetPlus HashIdentity.Structural [typeof<'c>.Name; typeof<'c2>.Name; typeof<'c3>.Name; typeof<'c4>.Name; typeof<'c5>.Name], subqueries)

    static member byType<'c, 'c2, 'c3, 'c4, 'c5, 'c6 when
        'c : struct and 'c :> 'c Component and
        'c2 : struct and 'c2 :> 'c2 Component and
        'c3 : struct and 'c3 :> 'c3 Component and
        'c4 : struct and 'c4 :> 'c4 Component and
        'c5 : struct and 'c5 :> 'c5 Component and
        'c6 : struct and 'c6 :> 'c6 Component>
        subqueries =
        Query (hashSetPlus HashIdentity.Structural [typeof<'c>.Name; typeof<'c2>.Name; typeof<'c3>.Name; typeof<'c4>.Name; typeof<'c5>.Name; typeof<'c6>.Name], subqueries)

    static member byType<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'c7 when
        'c : struct and 'c :> 'c Component and
        'c2 : struct and 'c2 :> 'c2 Component and
        'c3 : struct and 'c3 :> 'c3 Component and
        'c4 : struct and 'c4 :> 'c4 Component and
        'c5 : struct and 'c5 :> 'c5 Component and
        'c6 : struct and 'c6 :> 'c6 Component and
        'c7 : struct and 'c7 :> 'c7 Component>
        subqueries =
        Query (hashSetPlus HashIdentity.Structural [typeof<'c>.Name; typeof<'c2>.Name; typeof<'c3>.Name; typeof<'c4>.Name; typeof<'c5>.Name; typeof<'c6>.Name; typeof<'c7>.Name], subqueries)

    static member byType<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'c7, 'c8 when
        'c : struct and 'c :> 'c Component and
        'c2 : struct and 'c2 :> 'c2 Component and
        'c3 : struct and 'c3 :> 'c3 Component and
        'c4 : struct and 'c4 :> 'c4 Component and
        'c5 : struct and 'c5 :> 'c5 Component and
        'c6 : struct and 'c6 :> 'c6 Component and
        'c7 : struct and 'c7 :> 'c7 Component and
        'c8 : struct and 'c8 :> 'c8 Component>
        subqueries =
        Query (hashSetPlus HashIdentity.Structural [typeof<'c>.Name; typeof<'c2>.Name; typeof<'c3>.Name; typeof<'c4>.Name; typeof<'c5>.Name; typeof<'c6>.Name; typeof<'c7>.Name; typeof<'c8>.Name], subqueries)

    static member byType<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'c7, 'c8, 'c9 when
        'c : struct and 'c :> 'c Component and
        'c2 : struct and 'c2 :> 'c2 Component and
        'c3 : struct and 'c3 :> 'c3 Component and
        'c4 : struct and 'c4 :> 'c4 Component and
        'c5 : struct and 'c5 :> 'c5 Component and
        'c6 : struct and 'c6 :> 'c6 Component and
        'c7 : struct and 'c7 :> 'c7 Component and
        'c8 : struct and 'c8 :> 'c8 Component and
        'c9 : struct and 'c9 :> 'c9 Component>
        subqueries =
        Query (hashSetPlus HashIdentity.Structural [typeof<'c>.Name; typeof<'c2>.Name; typeof<'c3>.Name; typeof<'c4>.Name; typeof<'c5>.Name; typeof<'c6>.Name; typeof<'c7>.Name; typeof<'c8>.Name; typeof<'c9>.Name], subqueries)