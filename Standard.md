Correctness, Consistency, Simplicity
====================================

**An F\# Code Standard**
-

**First, a quick blurb on the difficulties people have with understanding code standards -**

What people often don't understand is that good code standards aren't designed to be used as blunt instruments to foist upon others one's own stylistic preferences. Instead, they're about finding the fewest, simplest, and least  inhibiting rules that enable people to be consistent with each another (and even themselves!) when writing code. In that way, code standards are more about following an informed reasoning process to its logical conclusion than about executing a war of attrition to pursue a stylistic cultural imperium.

*Without reason driving a decision-making process, all disagreements devolve into wars of passion... to be ended satisfactorily only by attrition... and to be relived anew by each succeeding generation.*

**A) Correctness**

1) Set warning levels to the highest level possible.

2) Enable warnings as errors wherever possible.

3) Enable `--warnon:1182` wherever possible (warn on unused variables).

4) Disable individual warnings in files as needed, and document that need.

5) Prefer immutable types and referentially transparent functions.

6) Make code clearly communicate its intent by its structure and / or via inline comments.

7) Make illegal states unrepresentable wherever feasible. [*Here's our friend Scott Wlaschin on the subject :)*](http://fsharpforfunandprofit.com/posts/designing-with-types-making-illegal-states-unrepresentable/)

8) Avoid trading away exhaustiveness checking unless you have a specific need, and document that need.

9) Avoid trading away type inference unless you have a specific need, and document that need if helpful.

10) Avoid hacks, incomplete functionality, and overly clever code unless you have a specific need, and document that need clearly.

11) Avoid object and struct types, as well as instance members and properties, unless you have a specific need for them (such as for a plug-in, interop, or a DSL, etc), and document that need.

12) Avoid arrays generally, and if they can't be avoided due to, say, interop, avoid exposing them to consuming functions by converting them to an immutable data structure, or at minimum, a seq.

13) Try to preserve debuggability of code by -

-   introducing local bindings to potentially-interesting intermediate results,
-   avoiding unnecessary laziness and asynchronicity (but since async being strewn throughout code is rarely avoidable, consider using the [*Vsync computation expression*](https://gist.github.com/bryanedds/45ef1193b79e06000a9a) instead!)

14) Suffix option bindings as well as bindings to potentially null values with `opt`.

15) Suffix choice bindings with `chc`.

16) Prefix functions that return an option or potential null with `try`.

17) Prefix functions that return a choice with `attempt`.

18) Place constants and literals in a submodule inside a Constants module, in a place that is commonly accessible.

19) Write tests and documentation comments for publicly-consumable types and functions.

20) Use two or more words for each public field name or discriminated union to avoid inference ambiguity. For example, `Id` is not a good public field name, but `ProductId` might be.

21) If you have to use an abstract member function, be sure to tuple its arguments to avoid the dreaded `'base' values may only be used to make direct calls to the base implementations of overridden members` error is you need to call base for it.

This is required due to an unresolved compiler bug touched on here - http://stackoverflow.com/questions/5847202/base-values-may-only-be-used-to-make-direct-calls-to-the-base-implementations . With any luck, this rule can be removed once the bug is addressed.

**B) Consistency**

1) Use 4 spaces for indentation, not 2, nor 3. [*5 is right out.*](https://www.youtube.com/watch?v=xOrgLj9lOwk&t=1m48s)

2) Use column 120 as the line length limit where practicable. Column 120 is not a hard limit, but is elegantly acheivable in most cases. An exception to this rule is code that constructs error messages.

3) Use the standard F\# naming conventions by -

-   using UpperCamelCasing for Namespaces, Modules, Types, Fields, Constants, Properties, and InstanceMembers (but as said above, avoid authoring Properties and InstanceMembers in the first place).
-   using lowerCamelCasing for variables, functions, staticMembers, parameters, and 'typeParameters.

4) Use shadowing on different bindings with the same conceptual identity rather than ' suffixes (this also helps correctness significantly).

5) Use access specifiers for encapsulation rather than FSI files.

6) Place `open` statements at the top of each file, right below the current namespace declaration (if any).

7) Order the parameters of functions from least to most important (that is, in the order of semantic impact).

8) Prefer function modules to use `[<RequireQualifiedAccess>]` except for operators, core functions like `flip`, and DSL functions.

9) Prefer stepped indentation as it refactors better, keeps lines shorter, and keeps formatting normal and enforcible via automation. For example, write this -

```
let result =
        ingest
            apple
            banana
            caribou
```

- rather than this -

```
    let result = ingest apple
                        banana
                        caribou
```

10) F\#'s syntax is based on ML, which is structurally derived from Lisp rather than C, so use Lisp-style bracing instead of C-style. For example, write this -

```
    let ys =
        [ f x
          g x
          h x ]
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

11) Tab out discriminated union case definitions to keep them lined up with their members. For example, write this -

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

12) Handle the intentional case first when matching / if'ing -

```
    let fn valueOpt =
        match valueOpt with
        | Some value -> // do what we actually intended to do in this function
        | None -> // handle the missing case
```

13) Surround tuples with parens to keep evaluation ordering and intent clear. For example, write this -

```
    let (a, b) = (b, a)
```

- rather than this -

```
    let a, b = b, a
```

14) Conceptually, () is unit, so please treat it as such. For example, write `fn ()` rather than `fn()`.

15) Conceptually, (a, b, c) is a tuple, so please treat it as such. For example, write `fn (a, b, c)` rather than `fn(a, b, c)`.

**C) Simplicity**

1) Use F\# as a functional-first language, rather than an object-oriented one. [*Here's our friend Rich Hickey on why object-orientation in inherently complex.*](http://www.infoq.com/presentations/Simple-Made-Easy)

2) For the mutation that you can't avoid, try to encapsulate its effects behind a referentially-transparent API wherever feasible.

3) Avoid dependencies on untested, incomplete, or unnecessarily complex code, libraries, or frameworks.

4) Avoid checking in dead / commented-out code. If unavoidable, leave a comment above the code explaining why it's commented out and / or when it will be useful again.

5) Consider passing around multiple dependency references in a single container (usually a record) rather than individually.

6) Consider making such a container an *abstract data type* by -

-   privatizing all of its fields like so - `type MyContainer = private { ... }`
-   exposing a narrow set of static member functions that provide only the desired functionality in a more abstract way.
-   here are some detailed slides on leveraging abstract data types here - [*Structuring F\# Programs with Abstract Data Types*](https://jetecommerce.sharepoint.com/corporate/tech/Shared%20Documents/Tech%20Talk%20Presentations/%5bTranscripted%5d%20Structuring%20FSharp%20Programs%20with%20Abstract%20Data%20Types.pptx) (view presentation here - [*https://vimeo.com/128464151*](https://vimeo.com/128464151))

**D) And Generally...**

1) Use an automated code formatter to enforce this code standard as much as possible. *Note that this is a work-in-progress due to a lack of existing tooling for F\#.* [*Discussion was left off here.*](https://github.com/fsprojects/VisualFSharpPowerTools/issues/1068)

2) When appending to this standard, prefer the style that is most enforceable by a reasonably intelligent automated code formatter.
