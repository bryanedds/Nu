// Copyright 2013 IntelliFactory
//
// Licensed under the Apache License, Version 2.0 (the "License"); you
// may not use this file except in compliance with the License.  You may
// obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
// implied.  See the License for the specific language governing
// permissions and limitations under the License.

/// Splits invocation of methods represented as `System.Reflection.MethodInfo`
/// into the static and dynamic phase, doing the binding work at the static phase.
/// A pre-bound method can be 100-1000x faster to invoke compared to using `.Invoke`
/// every time, which has to repeatedly do method binding.
///
/// NOTE: Modified locally for direct use in the Nu project.
/// TODO: Consider moving this to Prime.
namespace Nu
open System
open System.Reflection

/// Represents a method that can be quickly invoked dynamically.
[<AbstractClass>]
type FastInvoke () =

    abstract Invoke0 : unit -> obj
    abstract Invoke1 : obj -> obj
    abstract Invoke2 : obj * obj -> obj
    abstract Invoke3 : obj * obj * obj -> obj
    abstract Invoke4 : obj * obj * obj * obj -> obj
    abstract Invoke5 : obj * obj * obj * obj * obj -> obj
    abstract Invoke6 : obj * obj * obj * obj * obj * obj -> obj
    abstract Invoke7 : obj * obj * obj * obj * obj * obj * obj -> obj

    override this.Invoke0 () = invalidOp "Bad arguments given to a FastInvoke method."
    override this.Invoke1 (_) = invalidOp "Bad arguments given to a FastInvoke method."
    override this.Invoke2 (_, _) = invalidOp "Bad arguments given to a FastInvoke method."
    override this.Invoke3 (_, _, _) = invalidOp "Bad arguments given to a FastInvoke method."
    override this.Invoke4 (_, _, _, _) = invalidOp "Bad arguments given to a FastInvoke method."
    override this.Invoke5 (_, _, _, _, _) = invalidOp "Bad arguments given to a FastInvoke method."
    override this.Invoke6 (_, _, _, _, _, _) = invalidOp "Bad arguments given to a FastInvoke method."
    override this.Invoke7 (_, _, _, _, _, _, _) = invalidOp "Bad arguments given to a FastInvoke method."

[<RequireQualifiedAccess>]
module FastInvoke =

    let inline private (!) x = unbox x

    type private F<'R> = delegate of unit -> 'R
    type private F<'T1,'R>  = delegate of 'T1 -> 'R
    type private F<'T1,'T2,'R> = delegate of 'T1 * 'T2 -> 'R
    type private F<'T1,'T2,'T3,'R> = delegate of 'T1 * 'T2 * 'T3 -> 'R
    type private F<'T1,'T2,'T3,'T4,'R> = delegate of 'T1 * 'T2 * 'T3 * 'T4 -> 'R
    type private F<'T1,'T2,'T3,'T4,'T5,'R> = delegate of 'T1 * 'T2 * 'T3  * 'T4 * 'T5 -> 'R
    type private F<'T1,'T2,'T3,'T4,'T5,'T6,'R> = delegate of 'T1 * 'T2 * 'T3 * 'T4 * 'T5 * 'T6 -> 'R
    type private F<'T1,'T2,'T3,'T4,'T5,'T6,'T7,'R> = delegate of 'T1 * 'T2 * 'T3 * 'T4 * 'T5 * 'T6 * 'T7 -> 'R

    type private A = delegate of unit -> unit
    type private A<'T1> = delegate of 'T1 -> unit
    type private A<'T1,'T2> = delegate of 'T1 * 'T2 -> unit
    type private A<'T1,'T2,'T3> = delegate of 'T1 * 'T2 * 'T3 -> unit
    type private A<'T1,'T2,'T3,'T4> = delegate of 'T1 * 'T2 * 'T3 * 'T4 -> unit
    type private A<'T1,'T2,'T3,'T4,'T5> = delegate of 'T1 * 'T2 * 'T3 * 'T4 * 'T5 -> unit
    type private A<'T1,'T2,'T3,'T4,'T5,'T6> = delegate of 'T1 * 'T2 * 'T3 * 'T4 * 'T5 * 'T6 -> unit
    type private A<'T1,'T2,'T3,'T4,'T5,'T6,'T7> = delegate of 'T1 * 'T2 * 'T3 * 'T4 * 'T5 * 'T6 * 'T7 -> unit

    type [<AbstractClass>] private Factory () =
        abstract Prepare : obj -> FastInvoke

    type [<AbstractClass>] private Factory<'T> () =
        inherit Factory ()
        abstract Prepare : 'T -> FastInvoke
        override this.Prepare (x : obj) = this.Prepare (x :?> 'T)

    type [<Sealed>] private Func0<'R> () =
        inherit Factory<F<'R>> ()
        override this.Prepare (d : F<'R>) = { new FastInvoke () with override this.Invoke0 () = box (d.Invoke ()) }

    type [<Sealed>] private Func1<'T1,'R> () =
        inherit Factory<F<'T1,'R>> ()
        override this.Prepare (d : F<'T1,'R>) = { new FastInvoke () with override this.Invoke1 (x1) = box (d.Invoke (!x1)) }

    type [<Sealed>] private Func2<'T1,'T2,'R> () =
        inherit Factory<F<'T1,'T2,'R>> ()
        override this.Prepare (d : F<'T1,'T2,'R>) = { new FastInvoke () with override this.Invoke2 (x1, x2) = box (d.Invoke (!x1, !x2)) }

    type [<Sealed>] private Func3<'T1,'T2,'T3,'R> () =
        inherit Factory<F<'T1,'T2,'T3,'R>> ()
        override this.Prepare (d : F<'T1,'T2,'T3,'R>) = { new FastInvoke () with override this.Invoke3 (x1, x2, x3) = box (d.Invoke (!x1, !x2, !x3)) }

    type [<Sealed>] private Func4<'T1,'T2,'T3,'T4,'R> () =
        inherit Factory<F<'T1,'T2,'T3,'T4,'R>> ()
        override this.Prepare (d : F<'T1,'T2,'T3,'T4,'R>) = { new FastInvoke () with override this.Invoke4 (x1, x2, x3, x4) = box (d.Invoke (!x1, !x2, !x3, !x4)) }

    type [<Sealed>] private Func5<'T1,'T2,'T3,'T4,'T5,'R> () =
        inherit Factory<F<'T1,'T2,'T3,'T4,'T5,'R>> ()
        override this.Prepare (d : F<'T1,'T2,'T3,'T4,'T5,'R>) = { new FastInvoke () with override this.Invoke5 (x1, x2, x3, x4, x5) = box (d.Invoke (!x1, !x2, !x3, !x4, !x5)) }

    type [<Sealed>] private Func6<'T1,'T2,'T3,'T4,'T5,'T6,'R> () =
        inherit Factory<F<'T1,'T2,'T3,'T4,'T5,'T6,'R>> ()
        override this.Prepare (d : F<'T1,'T2,'T3,'T4,'T5,'T6,'R>) = { new FastInvoke () with override this.Invoke6 (x1, x2, x3, x4, x5, x6) = box (d.Invoke (!x1, !x2, !x3, !x4, !x5, !x6)) }

    type [<Sealed>] private Func7<'T1,'T2,'T3,'T4,'T5,'T6,'T7,'R> () =
        inherit Factory<F<'T1,'T2,'T3,'T4,'T5,'T6,'T7,'R>> ()
        override this.Prepare (d : F<'T1,'T2,'T3,'T4,'T5,'T6,'T7,'R>) = { new FastInvoke () with override this.Invoke7 (x1, x2, x3, x4, x5, x6, x7) = box (d.Invoke (!x1, !x2, !x3, !x4, !x5, !x6, !x7)) }

    type [<Sealed>] private Act0() =
        inherit Factory<A> ()
        override this.Prepare (d : A) = { new FastInvoke () with override this.Invoke0 () = d.Invoke (); null }

    type [<Sealed>] private Act1<'T1> () =
        inherit Factory<A<'T1>> ()
        override this.Prepare (d : A<'T1>) = { new FastInvoke () with override this.Invoke1 (x1) = d.Invoke (!x1); null }

    type [<Sealed>] private Act2<'T1,'T2> () =
        inherit Factory<A<'T1,'T2>> ()
        override this.Prepare (d : A<'T1,'T2>) = { new FastInvoke () with override this.Invoke2 (x1, x2) = d.Invoke (!x1, !x2); null }

    type [<Sealed>] private Act3<'T1,'T2,'T3> () =
        inherit Factory<A<'T1,'T2,'T3>> ()
        override this.Prepare (d : A<'T1,'T2,'T3>) = { new FastInvoke () with override this.Invoke3 (x1, x2, x3) = d.Invoke (!x1, !x2, !x3); null }

    type [<Sealed>] private Act4<'T1,'T2,'T3,'T4> () =
        inherit Factory<A<'T1,'T2,'T3,'T4>> ()
        override this.Prepare (d : A<'T1,'T2,'T3,'T4>) = { new FastInvoke () with override this.Invoke4 (x1, x2, x3, x4) = d.Invoke (!x1, !x2, !x3, !x4); null }

    type [<Sealed>] private Act5<'T1,'T2,'T3,'T4,'T5> () =
        inherit Factory<A<'T1,'T2,'T3,'T4,'T5>> ()
        override this.Prepare (d : A<'T1,'T2,'T3,'T4,'T5>) = { new FastInvoke () with override this.Invoke5 (x1, x2, x3, x4, x5) = d.Invoke (!x1, !x2, !x3, !x4, !x5); null }

    type [<Sealed>] private Act6<'T1,'T2,'T3,'T4,'T5,'T6> () =
        inherit Factory<A<'T1,'T2,'T3,'T4,'T5,'T6>> ()
        override this.Prepare (d : A<'T1,'T2,'T3,'T4,'T5,'T6>) = { new FastInvoke () with override this.Invoke6 (x1, x2, x3, x4, x5, x6) = d.Invoke (!x1, !x2, !x3, !x4, !x5, !x6); null }

    type [<Sealed>] private Act7<'T1,'T2,'T3,'T4,'T5,'T6,'T7> () =
        inherit Factory<A<'T1,'T2,'T3,'T4,'T5,'T6,'T7>> ()
        override this.Prepare (d : A<'T1,'T2,'T3,'T4,'T5,'T6,'T7>) = { new FastInvoke () with override this.Invoke7 (x1, x2, x3, x4, x5, x6, x7) = d.Invoke (!x1, !x2, !x3, !x4, !x5, !x6, !x7); null }

    /// Compiles a method to a fast invoke function.
    let compile (m : MethodInfo) : FastInvoke =
        let ts =
            [|if not m.IsStatic then yield m.DeclaringType
              for p in m.GetParameters() do yield p.ParameterType|]
        let r = m.ReturnType
        let isAction = r = typeof<Void>
        let (iT, dT) =
            if isAction then
                match ts.Length with
                | 0 -> (typedefof<Act0>, typedefof<A>)
                | 1 -> (typedefof<Act1<_>>, typedefof<A<_>>)
                | 2 -> (typedefof<Act2<_,_>>, typedefof<A<_,_>>)
                | 3 -> (typedefof<Act3<_,_,_>>, typedefof<A<_,_,_>>)
                | 4 -> (typedefof<Act4<_,_,_,_>>, typedefof<A<_,_,_,_>>)
                | 5 -> (typedefof<Act5<_,_,_,_,_>>, typedefof<A<_,_,_,_,_>>)
                | 6 -> (typedefof<Act6<_,_,_,_,_,_>>, typedefof<A<_,_,_,_,_,_>>)
                | 7 -> (typedefof<Act7<_,_,_,_,_,_,_>>, typedefof<A<_,_,_,_,_,_,_>>)
                | _ -> failwith "Actions with more than 7 parameters unsupported."
            else
                match ts.Length with
                | 0 -> (typedefof<Func0<_>>, typedefof<F<_>>)
                | 1 -> (typedefof<Func1<_,_>>, typedefof<F<_,_>>)
                | 2 -> (typedefof<Func2<_,_,_>>, typedefof<F<_,_,_>>)
                | 3 -> (typedefof<Func3<_,_,_,_>>, typedefof<F<_,_,_,_>>)
                | 4 -> (typedefof<Func4<_,_,_,_,_>>, typedefof<F<_,_,_,_,_>>)
                | 5 -> (typedefof<Func5<_,_,_,_,_,_>>, typedefof<F<_,_,_,_,_,_>>)
                | 6 -> (typedefof<Func6<_,_,_,_,_,_,_>>, typedefof<F<_,_,_,_,_,_,_>>)
                | 7 -> (typedefof<Func7<_,_,_,_,_,_,_,_>>, typedefof<F<_,_,_,_,_,_,_,_>>)
                | _ -> failwith "Functions with more than 7 parameters unsupported."
        let ts = if isAction then ts else Array.append ts [|r|]
        let dT = dT.MakeGenericType ts
        let iT = iT.MakeGenericType ts
        let factory = Activator.CreateInstance iT :?> Factory
        factory.Prepare (Delegate.CreateDelegate (dT, m))