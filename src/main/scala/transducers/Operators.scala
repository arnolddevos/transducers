package transducers

import scala.language.higherKinds

trait Operators { ops: Transducers with Views =>

  /**
   *  The stock transducers require that Context is a functor.
   *  A map and a lifting operation are needed.
   */
  def mapContext[S, T](c: Context[S])( f: S => T ): Context[T]
  def inContext[S](s: S): Context[S]


  /**
   *  Standard operators for Educibles including those needed
   *  by for comprehensions.
   */
  implicit class EductionOps[R[_]:Educible, A]( ra: R[A] ) {
    def map[B](g: A => B) = view(ra, ops.map(g))
    def flatMap[S[_]:Educible, B](g: A => S[B]) = view(ra, ops.flatMap(g))
    def >>=[S[_]:Educible, B](g: A => S[B]) = flatMap(g)
    def >>[S[_]:Educible, B]( k: => S[B] ) = flatMap(_ => k)
    def withFilter(p: A => Boolean) = view(ra, ops.filter(p))
  }

  /**
   * This helper performs the basic transformation for a stateless transducer.
   */
  private def proxy[A, B, S](f: Reducer[A, S])(g: (f.State, B) => Context[f.State]) = 
    new Reducer[B, S] {
      type State = f.State
      def init = f.init
      def apply(s: State, b: B) = g(s, b)
      def isReduced(s: State) = f.isReduced(s)
      def complete(s: State) = f.complete(s)
    }


  /**
   * Fundamental transducer for map.
   */
  def map[A, B](f: B => A) = new Transducer[A, B] {
    def apply[S](r: Reducer[A, S]) = proxy(r) {
      (s, b) => r(s, f(b))
    }
  }

  /**
   * Fundamental transducer for filter.
   */
  def filter[A](p: A => Boolean) = new Transducer[A, A] {
    def apply[S](r: Reducer[A, S]) = proxy(r) {
      (s, a) => if(p(a)) r(s, a) else inContext(s)
    }
  }

  /**
   * Fundamental transducer for flatMap.
   */
  def flatMap[A, B, R[_]:Educible](g: B => R[A]) = new Transducer[A, B] {
    def apply[S](f: Reducer[A, S]) = proxy(f) {
      (s0, b) =>
        val inner = new Reducer[A, f.State] {
          type State = f.State
          def init = s0
          def apply(s: State, a: A) = f(s, a)
          def isReduced(s: State) = f.isReduced(s)
          def complete(s: State) = s
        }
        educe(g(b), inner)
    }
  }

  /**
   *  Takes the initial series of elements that satisfy a predicate.
   *
   *  This is a stateful transducer: it augments the reduction state
   *  with a boolean tracking the predicate value.
   *
   */
  def takeWhile[A](p: A => Boolean) = new Transducer[A, A] {
    def apply[S](f: Reducer[A, S]) = new Reducer[A, S] {
      type State = (f.State, Boolean)
      def init = (f.init, true)
      def apply(s: State, a: A) = 
        if(s._2 && p(a)) mapContext(f(s._1, a))((_, true)) 
        else inContext((s._1, false))
      def isReduced(s: State) = ! s._2  || f.isReduced(s._1)
      def complete(s: State) = f.complete(s._1)
    }
  }

  /**
   *  Drops the initial series of elements that satisfy a predicate.
   *
   *  Augments the reduction state with a boolean tracking 
   *  the predicate value.
   *
   */
  def dropWhile[A](p: A => Boolean) = new Transducer[A, A] {
    def apply[S](f: Reducer[A, S]) = new Reducer[A, S] {
      type State = (f.State, Boolean)
      def init = (f.init, true)
      def apply(s: State, a: A) = 
        if(s._2 && p(a)) inContext((s._1, true))
        else mapContext(f(s._1, a))((_, false))
      def isReduced(s: State) = ! s._2  && f.isReduced(s._1)
      def complete(s: State) = f.complete(s._1)
    }
  }

  /**
   * Takes the first n elements.
   * 
   * Augments the reduction state with an integer
   * for the remaining number of elements to take.
   */
  def take[A](n: Int) = new Transducer[A, A] {
    def apply[S](r: Reducer[A, S]) = new Reducer[A, S] {
      type State = (r.State, Int)
      def init = (r.init, n)
      def apply(s: State, a: A): Context[State] =
        if(s._2 > 0) mapContext(r(s._1, a))((_, s._2-1)) 
        else inContext(s)
      def isReduced(s: State) = s._2 <= 0 || r.isReduced(s._1)
      def complete(s: State): S = r.complete(s._1)
    }
  }

  /**
   * Drops the first n elements.
   *
   * Augments the reduction state with an integer
   * for the remaining number of elements to drop.
   */
  def drop[A](n: Int) = new Transducer[A, A] {
    def apply[S](r: Reducer[A, S]) = new Reducer[A, S] {
      type State = (r.State, Int)
      def init = (r.init, n)
      def apply(s: State, a: A): Context[State] =
        if(s._2 > 0) inContext((s._1, s._2-1))
        else mapContext(r(s._1, a))((_, 0))
      def isReduced(s: State) = s._2 <= 0 && r.isReduced(s._1)
      def complete(s: State): S = r.complete(s._1)
    }
  }
}
