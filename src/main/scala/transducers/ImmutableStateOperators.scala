package transducers

trait ImmutableStateOperators { this: Transducers with ContextIsFunctor =>

  /**
   *  Takes the initial series of elements that satisfy a predicate.
   *
   *  This is a stateful transducer: it augments the reduction state
   *  with a boolean tracking the predicate value.
   *
   */
  def takeWhile[A](p: A => Boolean): Transducer[A, A] = new Transducer[A, A] {
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
  def dropWhile[A](p: A => Boolean): Transducer[A, A] = new Transducer[A, A] {
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
  def take[A](n: Int): Transducer[A, A] = new Transducer[A, A] {
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
  def drop[A](n: Int): Transducer[A, A] = new Transducer[A, A] {
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

  /**
   * An unconditional stateful transducer
   */
  def transducer[A, B, T](t0: T)(f: (T, A) => (T, B)): Transducer[B, A] = new Transducer[B, A] {
    def apply[S](r: Reducer[B, S]) = new Reducer[A, S] {
      type State = (r.State, T)
      def init = (r.init, t0)
      def apply(s: State, a: A): Context[State] = {
        val (t, b) = f(s._2, a)
        mapContext(r(s._1, b))((_, t))
      }
      def isReduced(s: State) = r.isReduced(s._1)
      def complete(s: State): S = r.complete(s._1)
    }
  }
}
