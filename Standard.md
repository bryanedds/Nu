Correctness, Consistency, Simplicity
====================================

**An F\# Code Standard to achieve Correctness, Consistency, and Simplicity** -

**A) Correctness**

1) Set warning levels to the highest level possible in all projects.

2) Enable warnings as errors in all projects.

3) Enable `--warnon:1182` (warn on unused variables) in all F# projects.

4) Prefer immutable types and referentially-transparent functions.

5) Make illegal states unrepresentable when feasible [*Here's our friend Scott Wlaschin on the subject*](http://fsharpforfunandprofit.com/posts/designing-with-types-making-illegal-states-unrepresentable/).

6) Avoid trading away exhaustiveness checking unless you have a specific need.

7) Avoid trading away type inference unless you have a specific need.

8) Avoid creating object and struct types, as well as instance members and properties, unless you have a specific need (such as for creating a plug-in, a DSL, for interop, for efficiency, or etc).

9) Try to preserve debuggability of code by -

-   introducing local bindings to potentially-interesting intermediate results,
-   avoiding unnecessary laziness and asynchrony (but since async being strewn throughout code is rarely avoidable, consider using the [*Vsync monad*](https://github.com/bryanedds/Nu/blob/master/Prime/Prime/Vsync.fs) instead).

10) Suffix option bindings, choice bindings, either bindings, and bindings to potentially null values with `Opt`.

11) Consider prefixing functions that return an option, choice, either, or potential null with `try`.

12) Try to use unique names for public fields and discriminated union cases to avoid ambiguating type inference. For example, `Id` is not a good public field name, but `ProductId` might be.

**B) Consistency**

1) Use 4 spaces for indentation, not 2, nor 3. [*5 is right out.*](https://www.youtube.com/watch?v=xOrgLj9lOwk&t=1m48s)

2) Use column 120 as the line length limit where practicable. Column 120 is not a hard limit, but is elegantly achievable in most cases. A common exception to this rule is for code that constructs error messages.

3) Use the standard F\# naming conventions by -

-   using `UpperCamelCasing` for `Namespaces`, `Modules`, `Types`, `Fields`, `Constants`, `Properties`, and `InstanceMembers`.
-   using `lowerCamelCasing` for `variables`, `functions`, `staticMembers`, `parameters`, and `'typeParameters`.

4) Use shadowing on different bindings with the same conceptual identity rather than `'` suffixes (this helps correctness significantly). Conversely, avoid shadowing on different binding with different conceptual identities.

5) Place `open` statements at the top of each file, right below the current namespace declaration.

6) Order the parameters of functions from least to most important (that is, in the order of increasing semantic impact). This makes currying easy to leverage and consistent.

7) Prefer stepped indentation as it refactors better, keeps lines shorter, and keeps formatting normal and enforcible via automation. For example, write this -

```
    let result =
        ingest
            apple
            banana
            grape
```

- rather than this -

```
    let result = ingest apple
                        banana
                        grape
```

8) F\#'s syntax is based on ML, which is structurally derived from Lisp rather than C, so use Lisp-style bracing instead of C-style. For example, write this -

```
    let ys =
        [f x
         g x
         h x]
```

- rather than this -

```
    let ys =
        [
            f x
            g x
            h x
        ]
```

- and this -

```
    type T =
        { M : int
          N : single }
```

- rather than this -

```
    type T =
        {
            M : int
            N : single
        }
```

9) Tab out discriminated union case definitions to keep them lined up with their members. For example, write this -

```
    type T =
        | A of int
        | B of single
        static member makeA i = A (i * 2)
        static member makeB s = B (s * 2.0f)
```

- rather than this -

```
    type T =
    | A of int
    | B of single
        static member makeA i = A (i * 2)
        static member makeB s = B (s * 2.0f)
```

10) Handle the intentional case first when matching / if'ing -

```
    let fn valueOpt =
        match valueOpt with
        | Some value -> // do what we actually intended to do in this function
        | None -> // handle the edge case
```

11) Surround tuples with parens to keep evaluation ordering and intent clear. For example, write this -

```
    let (a, b) = (b, a)
```

- rather than this -

```
    let a, b = b, a
```

12) Conceptually, () is unit, so please treat it as such. For example, write `fn ()` rather than `fn()`.

13) Conceptually, (a, b, c) is a tuple, so please treat it as such. For example, write `fn (a, b, c)` rather than `fn(a, b, c)`. The exception is when you need to use F#'s flow-syntax feature.

**C) Simplicity**

1) Use F\# as a functional-first language, rather than an object-oriented one. [*Here's our friend Rich Hickey on why object-orientation in inherently complex.*](http://www.infoq.com/presentations/Simple-Made-Easy).

2) For mutation that you can't avoid, try to encapsulate it behind a referentially-transparent interface wherever feasible. For example, consider wrapping your mutable constructs with [*KeyedCache*](https://github.com/bryanedds/Nu/blob/master/Prime/Prime/KeyedCache.fs) or [*MutantCache*](https://github.com/bryanedds/Nu/blob/master/Prime/Prime/MutantCache.fs)

3) Avoid dependencies on untested, incomplete, or unnecessarily complex libraries / frameworks.

4) Avoid checking in dead / commented-out code. If unavoidable, leave a comment above the code explaining why it's commented out and / or when it will be useful again.

5) Consider passing around multiple dependency references in a single container (usually a record) rather than individually.

6) Consider making such a container an *abstract data type* by -

-   privatizing all of its fields like so - `type MyContainer = private { ... }`
-   exposing a narrow set of static member functions that provide only the desired functionality in a more abstract way.

Here are some detailed slides on leveraging abstract data types here - [*Structuring F\# Programs with Abstract Data Types*](https://jetecommerce.sharepoint.com/corporate/tech/Shared%20Documents/Tech%20Talk%20Presentations/%5bTranscripted%5d%20Structuring%20FSharp%20Programs%20with%20Abstract%20Data%20Types.pptx) (view presentation here - [*https://vimeo.com/128464151*](https://vimeo.com/128464151))