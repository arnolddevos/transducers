# Transducers

Or yet another way to fold.   

These are types and combinators that operate on sequences of values. Inspired by the transducer concept in clojure, but with _types_.  See `Transducer.scala` for details. Briefly:

A `Reducer[A, S]` folds (or reduces) a sequence of `A` values to an `S`.  Its method `apply(a: A, s: State): State` folds one `A` into a reduction `State`. The `State` type is opaque to the user.

Other methods handle initialisation, normal and early termination, and the extraction of an `S` from the `State`.  The early termination ability and the opaque reduction `State` differentiate `Reducer` from the usual `Foldable` typeclass.  

A `Transducer[A, B]` is a function from `Reducer[A, S]` to `Reducer[B, S]` for any type `S`.  Or, from another point of view, it transforms a sequence of `B` to a sequence of `A`.  

Note: A `Transducer` is not the same as a `map` operation.  The input and output sequences may be different lengths and a given `B` will not necessarily correspond to a specific `A`.

