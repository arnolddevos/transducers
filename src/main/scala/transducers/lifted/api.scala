package transducers
package lifted

trait API
  extends Transducers
  with Views
  with Operators
  with StatefulOperators
  with Eduction
  with Reductions
  with Induction
  with Syntax
  with ContextIsMonad
