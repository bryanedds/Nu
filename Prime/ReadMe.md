The Prime F# Code Library [![License](https://img.shields.io/badge/license-MIT-blue.svg)](https://github.com/bryanedds/Nu/blob/master/Prime/LICENSE.md) [![NuGet](https://img.shields.io/nuget/v/Nuget.Core.svg)](https://www.nuget.org/packages/Prime)
=

## Features

- A metaprogramming system based on symbolic expressions with the **Symbol** and **SymbolicConverter** types.
- A generalized serialization system based on the above and related types.
- A purely functional, publisher-neutral event system with **EventSystem** and related types.
- The functional-reactive **Stream** and **Chain** monads for said event system.
- A purely functional dynamic property system called **Xtension**.
- A purely functional random number generator called **Rand**.
- The incredibly valuable **Vsync** monad allowing the same program to be run in parallel or debugged sequentially.
- The fastest-known persistent hash map in F#, **Vmap** - over twice as fast as Map, and 1/3 look-up speed of Dictionary!
- Fastest pure functional **Ulist**, **Umap**, and **Uset** collections rivaling the speed of .NET List, Dictionary and HashSet.
- Innovative pure-functional wrappers for arbitrary impure objects, **KeyedCache** and **MutantCache**.
- So many extension primitives I couldn't hope to mention them all!

Prime is built with clean and modular **Abstract Data Type** programming style as presented here - https://vimeo.com/128464151

This library makes a good base for non-trivial projects like renderers, simulations, and other real thangs that absolutely *need* to be built with good program semantics and efficient execution patterns.
