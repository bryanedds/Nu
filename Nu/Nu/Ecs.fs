namespace Nu
open System
open System.Collections.Generic
open System.IO
open System.Runtime.InteropServices
open System.Threading.Tasks
open Prime

/// Allows a value to always pass as equal with another of its same type.
type [<CustomEquality; NoComparison; Struct>] 'a AlwaysEqual =
    | AlwaysEqual of 'a
    member this.Value = match this with AlwaysEqual value -> value
    override this.GetHashCode () = 0
    override this.Equals (that : obj) = that :? AlwaysEqual<'a>

/// The component that holds an entity's id.
type [<NoEquality; NoComparison; Struct>] EntityId =
    { mutable Active : bool
      mutable EntityId : uint64 }
    interface EntityId Component with
        member this.Active with get () = this.Active and set value = this.Active <- value

/// Describes a means to store components.
type Store =
    interface
        abstract Length : int
        abstract Name : string
        abstract Item : int -> obj
        abstract SetItem : int -> obj -> unit
        abstract ZeroItem : int -> unit
        abstract Grow : unit -> unit
        abstract Read : int -> int -> FileStream -> unit
        end

/// Stores components.
type Store<'c when 'c : struct and 'c :> 'c Component> (name) =
    let mutable arr = Array.zeroCreate<'c> Constants.Ecs.ArrayReserve
    member this.Length = arr.Length
    member this.Name = name
    member this.Item i = &arr.[i]
    member this.SetItem index comp = arr.[index] <- comp
    member this.ZeroItem index = arr.[index] <- Unchecked.defaultof<'c>
    member this.Grow () =
        let length = int (single (max arr.Length 2) * 1.5f)
        let arr' = Array.zeroCreate<'c> length
        Array.Copy (arr, arr', arr.Length)
        arr <- arr'
    member this.Read index count (stream : FileStream) =
        let compSize = sizeof<'c>
        let comp = Unchecked.defaultof<'c> :> obj
        let buffer = Array.zeroCreate<byte> compSize
        let gch = GCHandle.Alloc (comp, GCHandleType.Pinned)
        try 
            let mutable index = index
            for _ in 0 .. dec count do
                stream.Read (buffer, 0, compSize) |> ignore<int>
                Marshal.Copy (buffer, 0, gch.AddrOfPinnedObject (), compSize)
                if index = arr.Length then this.Grow ()
                arr.[index] <- comp :?> 'c
                index <- inc index
        finally gch.Free ()
    interface Store with
        member this.Length = this.Length
        member this.Name = this.Name
        member this.Item i = this.Item i :> obj
        member this.SetItem index compObj = this.SetItem index (compObj :?> 'c)
        member this.ZeroItem index = this.ZeroItem index
        member this.Grow () = this.Grow ()
        member this.Read index count stream = this.Read index count stream

/// Data for an Ecs registration event.
type [<NoEquality; NoComparison; Struct>] EcsChangeData =
    { EntityRef : EntityRef
      ComponentName : string }

/// Data for an Ecs registration event.
and [<NoEquality; NoComparison; Struct>] EcsRegistrationData =
    { EntityRef : EntityRef
      ContextName : string }

and [<StructuralEquality; NoComparison; Struct>] EcsEventType =
    | GlobalEvent
    | EntityEvent of EntityRef : EntityRef
    | ComponentEvent of EntityRef2 : EntityRef * ComponentEvent : string

and [<StructuralEquality; NoComparison; Struct>] EcsEvent =
    { EventName : string
      EventType : EcsEventType }

and [<AbstractClass; Sealed>] EcsEvents =
    static member Update = { EventName = "Update"; EventType = GlobalEvent }
    static member UpdateParallel = { EventName = "UpdateParallel"; EventType = GlobalEvent }
    static member PostUpdate = { EventName = "PostUpdate"; EventType = GlobalEvent }
    static member PostUpdateParallel = { EventName = "PostUpdateParallel"; EventType = GlobalEvent }
    static member Actualize = { EventName = "Actualize"; EventType = GlobalEvent }
    static member Register entityRef compName = { EventName = "Register"; EventType = ComponentEvent (entityRef, compName) }
    static member Unregistering entityRef compName = { EventName = "Unregistering"; EventType = ComponentEvent (entityRef, compName) }
    static member Change entityRef = { EventName = "Change"; EventType = EntityEvent entityRef }

and [<StructuralEquality; NoComparison>] Term =
    | Z of int
    | R of single
    | C of IComparable
    | Obj of obj
    | Tag
    | Label of string
    | Labels of string HashSet
    | EntityRef of EntityRef
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

and [<StructuralEquality; NoComparison>] Subquery =
    | Is of string
    | Of of string * string
    | ByName of string * string
    | ByType of string * Type
    | Eq of string * Term
    | Gt of string * Term
    | Ge of string * Term
    | Lt of string * Term
    | Le of string * Term
    | Not of Subquery
    | And of Subquery list
    | Or of Subquery list

    static member private equalTo term term2 =
        match (term, term2) with
        | (Z z, Z z2) -> z = z2
        | (R r, R r2) -> r = r2
        | (C c, C c2) -> c = c2
        | (Obj o, Obj o2) -> objEq o o2
        | (Label label, Label label2) -> strEq label label2
        | (Labels labels, Labels labels2) -> labels.SetEquals labels2
        | (EntityRef entityRef, EntityRef entityRef2) -> genEq entityRef entityRef2
        | (Terms terms, Terms terms2) ->
            if terms.Length = terms2.Length
            then List.forall2 Subquery.equalTo terms terms2
            else false
        | _ -> false

    static member eval (terms : IReadOnlyDictionary<string, Term>) (subquery : Subquery) : bool =
        match subquery with
        | Is termName ->
            terms.ContainsKey termName
        | Of (termName, label) ->
            match terms.TryGetValue termName with
            | (true, term) ->
                match term with
                | Labels labels -> labels.Contains label
                | _ -> false
            | (false, _) -> false
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
                | Extra (_, ty, _)  -> refEq ty ty2
                | _ -> false
            | (false, _) -> false
        | Eq (termName, term2) ->
            match terms.TryGetValue termName with
            | (true, term) ->
                Subquery.equalTo term term2
            | (false, _) -> false
        | Gt (termName, term2) ->
            match terms.TryGetValue termName with
            | (true, term) ->
                match (term, term2) with
                | (Z z, Z z2) -> z > z2
                | (R r, R r2) -> r > r2
                | (C c, C c2) -> c.CompareTo c2 > 0
                | (_, _) -> false
            | (false, _) -> false
        | Ge (termName, term2) ->
            match terms.TryGetValue termName with
            | (true, term) ->
                match (term, term2) with
                | (Z z, Z z2) -> z >= z2
                | (R r, R r2) -> r >= r2
                | (C c, C c2) -> c.CompareTo c2 >= 0
                | (_, _) -> false
            | (false, _) -> false
        | Lt (termName, term2) ->
            match terms.TryGetValue termName with
            | (true, term) ->
                match (term, term2) with
                | (Z z, Z z2) -> z < z2
                | (R r, R r2) -> r < r2
                | (C c, C c2) -> c.CompareTo c2 < 0
                | (_, _) -> false
            | (false, _) -> false
        | Le (termName, term2) ->
            match terms.TryGetValue termName with
            | (true, term) ->
                match (term, term2) with
                | (Z z, Z z2) -> z <= z2
                | (R r, R r2) -> r <= r2
                | (C c, C c2) -> c.CompareTo c2 <= 0
                | (_, _) -> false
            | (false, _) -> false
        | Not subquery ->
            not (Subquery.eval terms subquery)
        | And subqueries ->
            List.forall (Subquery.eval terms) subqueries
        | Or subqueries ->
            List.exists (Subquery.eval terms) subqueries

    static member evalMany (terms : IReadOnlyDictionary<string, Term>) (subqueries : Subquery seq) =
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

/// Identifies an archetype.
and ArchetypeId (terms) =

    let hashCode = Dictionary.hash terms // OPTIMIZATION: hash is cached for speed

    new (intraComponents, subterms : Dictionary<string, Term>) =
        ArchetypeId
            (let intraterms = Seq.map (fun (compName, compTy) -> (Constants.Ecs.IntraComponentPrefix + compName, Intra (compName, compTy))) intraComponents
             let terms = dictPlus StringComparer.Ordinal intraterms
             subterms |> Seq.iter (fun subtermEntry -> terms.Add (subtermEntry.Key, subtermEntry.Value))
             terms)

    new (intraComponentTypes : Type seq, subterms : Dictionary<string, Term>) =
        ArchetypeId
            (let intraterms = intraComponentTypes |> Seq.map (fun compTy -> let compName = compTy.Name in (Constants.Ecs.IntraComponentPrefix + compName, Intra (compName, compTy)))
             let terms = dictPlus StringComparer.Ordinal intraterms
             subterms |> Seq.iter (fun subtermEntry -> terms.Add (subtermEntry.Key, subtermEntry.Value))
             terms)

    member this.Terms = terms :> IReadOnlyDictionary<_, _>
    member this.HashCode = hashCode

    member this.AddTerm termName term =
        let terms = Dictionary (terms, StringComparer.Ordinal)
        terms.[termName] <- term
        ArchetypeId terms

    member this.RemoveTerm termName =
        let terms = Dictionary (terms, StringComparer.Ordinal)
        terms.Remove termName |> ignore<bool>
        ArchetypeId terms

    static member equals (left : ArchetypeId) (right : ArchetypeId) =
        left.HashCode = right.HashCode &&
        Term.equalsMany left.Terms right.Terms

    static member singleton termName term =
        ArchetypeId (Dictionary.singleton StringComparer.Ordinal termName term)

    static member zero =
        ArchetypeId (dictPlus StringComparer.Ordinal [])

    override this.GetHashCode () =
        hashCode

    override this.Equals that =
        match that with
        | :? ArchetypeId as that -> ArchetypeId.equals this that
        | _ -> failwithumf ()

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

/// Describes a means to query components.
and IQuery =
    interface
        abstract Subqueries : Subquery seq
        abstract TryRegisterArchetype : Archetype -> unit
        end

/// An entity's place in an archetype.
and [<StructuralEquality; NoComparison; Struct>] ArchetypeSlot =
    { ArchetypeIndex : int
      Archetype : Archetype }

/// An ECS event.
and [<NoEquality; NoComparison>] EcsEvent<'d, 's> =
    { EcsEventData : 'd }

/// An ECS event callback.
and private EcsCallback<'d, 's> =
    EcsEvent<'d, 's> -> Ecs -> 's -> 's

/// An archetype-based ECS construct.
and Ecs () =

    let mutable subscriptionIdCurrent = 0u
    let mutable entityIdCurrent = 0UL
    let archetypes = dictPlus<ArchetypeId, Archetype> HashIdentity.Structural []
    let archetypeSlots = dictPlus<uint64, ArchetypeSlot> HashIdentity.Structural []
    let componentTypes = dictPlus<string, Type> StringComparer.Ordinal []
    let subscriptions = dictPlus<EcsEvent, Dictionary<uint32, obj>> HashIdentity.Structural []
    let subscribedEntities = dictPlus<EntityRef, int> HashIdentity.Structural []
    let queries = List<IQuery> ()

    let createArchetype (archetypeId : ArchetypeId) =
        let archetype = Archetype archetypeId
        archetypes.Add (archetypeId, archetype)
        for query in queries do
            query.TryRegisterArchetype archetype
        archetype

    member private this.AllocSubscriptionId () =
        subscriptionIdCurrent <- inc subscriptionIdCurrent
        if subscriptionIdCurrent = UInt32.MaxValue then failwith "Unbounded use of ECS subscription ids not supported."
        subscriptionIdCurrent

    member private this.BoxCallback<'d, 's> (callback : EcsCallback<'d, 's>) =
        let boxableCallback = fun (evt : EcsEvent<obj, 's>) store ->
            let evt = { EcsEventData = evt.EcsEventData :?> 'd }
            callback evt store
        boxableCallback :> obj

    member private this.RegisterEntityInternal comps archetypeId entityRef =
        let archetype =
            match archetypes.TryGetValue archetypeId with
            | (true, archetype) -> archetype
            | (false, _) -> createArchetype archetypeId
        let archetypeIndex = archetype.Register comps
        archetypeSlots.Add (entityRef.EntityId, { ArchetypeIndex = archetypeIndex; Archetype = archetype })

    member private this.UnregisterEntityInternal archetypeSlot entityRef =
        let archetype = archetypeSlot.Archetype
        let comps = archetype.GetComponents archetypeSlot.ArchetypeIndex
        archetypeSlots.Remove entityRef.EntityId |> ignore<bool>
        archetype.Unregister archetypeSlot.ArchetypeIndex
        comps

    member internal this.IndexArchetypeSlot (entityRef : EntityRef) =
        archetypeSlots.[entityRef.EntityId]

    member this.EntityRef =
        entityIdCurrent <- inc entityIdCurrent
        if entityIdCurrent = UInt64.MaxValue then failwith "Unbounded use of ECS entity ids not supported."
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
        match event.EventType with
        | ComponentEvent (entityRef, _) ->
            match subscribedEntities.TryGetValue entityRef with
            | (true, count) -> subscribedEntities.[entityRef] <- inc count
            | (false, _) -> subscribedEntities.Add (entityRef, 1)
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
            match event.EventType with
            | ComponentEvent (entityRef, _) ->
                match subscribedEntities.TryGetValue entityRef with
                | (true, count) ->
                    if count = 1
                    then subscribedEntities.Remove entityRef |> ignore<bool>
                    else subscribedEntities.[entityRef] <- inc count
                | (false, _) -> failwith "Subscribed entities count mismatch."
            | _ -> failwith "Subscribed entities count mismatch."
        result

    member this.RegisterComponentName<'c when 'c : struct and 'c :> 'c Component> componentName =
        match componentTypes.TryGetValue componentName with
        | (true, _) -> failwith "Component type already registered."
        | (false, _) -> componentTypes.Add (componentName, typeof<'c>)

    member this.RegisterTerm (termName : string) term (entityRef : EntityRef) =
        if termName.StartsWith Constants.Ecs.IntraComponentPrefix then failwith "Term names that start with '@' are for internal use only."
        if (match term with Intra _ -> true | _ -> false) then failwith "Intra components are for internal use only."
        match archetypeSlots.TryGetValue entityRef.EntityId with
        | (true, archetypeSlot) ->
            let comps = this.UnregisterEntityInternal archetypeSlot entityRef
            match term with Extra (compName, _, comp) -> comps.Add (compName, comp.Value) | _ -> ()
            let archetypeId = archetypeSlot.Archetype.Id.AddTerm termName term
            this.RegisterEntityInternal comps archetypeId entityRef
        | (false, _) ->
            let archetypeId = ArchetypeId.singleton termName term
            let comps = dictPlus StringComparer.Ordinal []
            match term with Extra (compName, _, comp) -> comps.Add (compName, comp.Value) | _ -> ()
            this.RegisterEntityInternal comps archetypeId entityRef

    member this.UnregisterTerm (termName : string) (entityRef : EntityRef) =
        if termName.StartsWith Constants.Ecs.IntraComponentPrefix then failwith "Term names that start with '@' are for internal use only."
        match archetypeSlots.TryGetValue entityRef.EntityId with
        | (true, archetypeSlot) ->
            let comps = this.UnregisterEntityInternal archetypeSlot entityRef
            let archetypeId = archetypeSlot.Archetype.Id.RemoveTerm termName
            if archetypeId.Terms.Count > 0 then this.RegisterEntityInternal comps archetypeId entityRef
        | (false, _) -> ()

    member this.RegisterComponentPlus<'c, 's when 'c : struct and 'c :> 'c Component> compName (comp : 'c) (entityRef : EntityRef) (state : 's) =
        match archetypeSlots.TryGetValue entityRef.EntityId with
        | (true, archetypeSlot) ->
            let comps = this.UnregisterEntityInternal archetypeSlot entityRef
            comps.Add (compName, comp)
            let archetypeId = archetypeSlot.Archetype.Id.AddTerm (Constants.Ecs.IntraComponentPrefix + compName) (Intra (compName, typeof<'c>))
            this.RegisterEntityInternal comps archetypeId entityRef
            let eventData = { EntityRef = entityRef; ContextName = compName }
            this.Publish<EcsRegistrationData, obj> (EcsEvents.Register entityRef compName) eventData (state :> obj) :?> 's
        | (false, _) ->
            let archetypeId = ArchetypeId.singleton (Constants.Ecs.IntraComponentPrefix + compName) (Intra (compName, typeof<'c>))
            let comps = Dictionary.singleton StringComparer.Ordinal compName (comp :> obj)
            this.RegisterEntityInternal comps archetypeId entityRef
            let eventData = { EntityRef = entityRef; ContextName = compName }
            this.Publish<EcsRegistrationData, obj> (EcsEvents.Register entityRef compName) eventData (state :> obj) :?> 's

    member this.RegisterComponent<'c, 's when 'c : struct and 'c :> 'c Component> (comp : 'c) (entityRef : EntityRef) (state : 's) =
        let compName = typeof<'c>.Name
        this.RegisterComponentPlus<'c, 's> compName comp (entityRef : EntityRef) state

    member this.UnregisterComponentPlus<'c, 's when 'c : struct and 'c :> 'c Component> compName (entityRef : EntityRef) (state : 's) =
        match archetypeSlots.TryGetValue entityRef.EntityId with
        | (true, archetypeSlot) ->
            let eventData = { EntityRef = entityRef; ContextName = compName }
            let state = this.Publish<EcsRegistrationData, obj> (EcsEvents.Unregistering entityRef compName) eventData (state :> obj) :?> 's
            let comps = this.UnregisterEntityInternal archetypeSlot entityRef
            let archetypeId = archetypeSlot.Archetype.Id.RemoveTerm (Constants.Ecs.IntraComponentPrefix + compName)
            if archetypeId.Terms.Count > 0 then
                comps.Remove compName |> ignore<bool>
                this.RegisterEntityInternal comps archetypeId entityRef
                state
            else state
        | (false, _) -> state

    member this.UnregisterComponent<'c, 's when 'c : struct and 'c :> 'c Component> (entityRef : EntityRef) (state : 's) =
        this.UnregisterComponentPlus<'c, 's> typeof<'c>.Name entityRef state

    member this.RegisterEntity skipEvents comps archetypeId state =
        let archetype =
            match archetypes.TryGetValue archetypeId with
            | (true, archetype) -> archetype
            | (false, _) -> createArchetype archetypeId
        let entityRef = this.EntityRef
        let archetypeIndex = archetype.Register comps
        archetypeSlots.Add (entityRef.EntityId, { ArchetypeIndex = archetypeIndex; Archetype = archetype })
        let mutable state = state
        if not skipEvents then
            for compName in archetype.Stores.Keys do
                let eventData = { EntityRef = entityRef; ContextName = compName }
                state <- this.Publish<EcsRegistrationData, obj> (EcsEvents.Unregistering entityRef compName) eventData (state :> obj) :?> 's
        (entityRef, state)

    member this.RegisterEntities skipEvents count comps archetypeId state =

        // get archetype
        let archetype =
            match archetypes.TryGetValue archetypeId with
            | (true, archetype) -> archetype
            | (false, _) -> createArchetype archetypeId

        // register entities to archetype
        let mutable state = state
        let entityRefs = SegmentedArray.zeroCreate count
        for i in 0 .. dec count do
            let entityRef = this.EntityRef
            let archetypeIndex = archetype.Register comps
            archetypeSlots.Add (entityRef.EntityId, { ArchetypeIndex = archetypeIndex; Archetype = archetype })
            entityRefs.[i] <- entityRef
            if not skipEvents then
                for compName in archetype.Stores.Keys do
                    let eventData = { EntityRef = entityRef; ContextName = compName }
                    state <- this.Publish<EcsRegistrationData, obj> (EcsEvents.Unregistering entityRef compName) eventData (state :> obj) :?> 's

        // fin
        (entityRefs, state)

    member this.UnregisterEntity (entityRef : EntityRef) (state : 's) =
        match archetypeSlots.TryGetValue entityRef.EntityId with
        | (true, archetypeSlot) ->
            let archetype = archetypeSlot.Archetype
            let mutable state = state
            if subscribedEntities.ContainsKey entityRef then
                for compName in archetype.Stores.Keys do
                    let eventData = { EntityRef = entityRef; ContextName = compName }
                    state <- this.Publish<EcsRegistrationData, obj> (EcsEvents.Unregistering entityRef compName) eventData (state :> obj) :?> 's
            archetype.Unregister archetypeSlot.ArchetypeIndex
            state
        | (false, _) -> state

    member this.RegisterQuery<'q when 'q :> IQuery> (query : 'q) =
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
        let entityRefs = SegmentedArray.zeroCreate count
        for i in firstIndex .. lastIndex do
            let entityRef = this.EntityRef
            archetypeSlots.Add (entityRef.EntityId, { ArchetypeIndex = i; Archetype = archetype })
            entityRefs.[i - firstIndex] <- entityRef
        entityRefs

and [<StructuralEquality; NoComparison; Struct>] EntityRef =
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
        this.Ecs.Publish (EcsEvents.Change this) { EntityRef = this; ComponentName = compName } state

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