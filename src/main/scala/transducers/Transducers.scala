package transducers

import scala.language.higherKinds

trait Transducers {

  /**
   * Context could be Try, Future, Task, Process or other higher kinded type
   * within which values are produced.
   *
   * In the simple case, Context[S] = S and transducers work directly
   * on series of values similar to standard library collection methods.
   */
  type Context[+S]
  def inContext[S](s: S): Context[S]

  /**
   * Reducer over A's producing an S.
   *
   * The internal state of the reducer is of type State (not always the same as S).
   *
   * The init, complete and apply methods correspond to
   * the 0, 1 and 2 arity functions of a clojure reducer.
   * The isReduced predicate on the state says the reduction should stop.
   * That is, apply(s, a) should not be called if isReduced(s).
   *
   * Each reduction step produces a new State, but in context ie as a Context[State].
   * This is good for error handling, asynchronous execution, or both.
   */
  trait Reducer[-A, +S] {
    type State
    def init: Context[State]
    def apply(s: State, a: A): Context[State]
    def isReduced(s: State): Boolean
    def complete(s: State): Context[S]
  }

  /**
   * Make a basic reducer fom an initial value and function.
   * (State and result S will be the same type.)
   */
  def reducer[A, S](s: S)(f: (S, A) => Context[S]): Reducer[A, S] = new Reducer[A, S] {
    type State = S
    def init = inContext(s)
    def apply(s: S, a: A) = f(s, a)
    def isReduced(s: S) = false
    def complete(s: S) = inContext(s)
  }

  /**
   * Transducer is a (polymorphic) function from Reducer to Reducer.
   * These can be composed by andThen as with ordinary functions.
   */
  trait Transducer[+A, -B] { tb =>
    def apply[S](fa: Reducer[A, S]): Reducer[B, S]
    def andThen[C](tc: Transducer[B, C]) = new Transducer[A, C] {
      def apply[S](fa: Reducer[A, S]) = tc(tb(fa))
    }
    def compose[C](ta: Transducer[C, A]) = new Transducer[C, B] {
      def apply[S](fc: Reducer[C, S]) = tb(ta(fc))
    }
  }

  /**
   *  A transducer that effects no change.
   */
  def cat[A] = new Transducer[A, A] {
    def apply[S](fa: Reducer[A, S]) = fa
  }

  trait Educible[G, A] {
    def educe[S](g: G, f: Reducer[A, S]): Context[S]
  }

  def reduce[G, A, S](g: G, f: Reducer[A, S])(implicit e: Educible[G, A]): Context[S] =
    e.educe(g, f)

  /**
   * Apply a Reducer of B's to an Educible of A's using a transducer from A's to B's
   */
  def transduce[G, A, B, S](g: G, t: Transducer[B, A], f: Reducer[B, S])(implicit e: Educible[G, A]): Context[S] =
    reduce(g, t(f))

}
