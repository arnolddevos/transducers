package transducers
package lifted

trait API
  extends Transducers
  with Views
  with Operators
  with StatefulOperators
  with Eduction
  with Reduction
  with Induction
  with Syntax
  with ContextIsMonad
