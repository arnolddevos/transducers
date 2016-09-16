package transducers

trait ImmutableStateOperators { this: Transducers with ContextIsFunctor =>

  trait Reduction[A, S] {
    def isReduced: Boolean
    def update(a: A): Context[Reduction[A, S]]
    def complete: Context[S]
  }

  abstract class StatefulTransducer[A, B] extends Transducer[A, B] {
    def inner[S](f: Reducer[A, S]): Context[Reduction[B, S]]
    def apply[S](f: Reducer[A, S]): Reducer[B, S] = new Reducer[B, S] {
      type State = Reduction[B, S]
      final def init = inner(f)
      final def isReduced(s: State) = s.isReduced
      final def apply(s: State, b: B) = s.update(b)
      final def complete(s: State) = s.complete
    }
  }

  /**
   * Takes the first n elements.
   *
   * Augments the reduction state with an integer
   * for the remaining number of elements to take.
   */
  def take[A](n: Int): Transducer[A, A] = new StatefulTransducer[A, A] {
    def inner[S](f: Reducer[A, S]) = {
      def loop(count: Int, s: f.State): Reduction[A, S] = new Reduction[A, S] {
        def update(a: A) =
          if(count > 0) mapContext(f(s, a))(loop(count-1, _))
          else inContext(this)
        def isReduced = count <= 0 || f.isReduced(s)
        def complete = f.complete(s)
      }
      mapContext(f.init)(loop(n, _))
    }
  }

  /**
   * Drops the first n elements.
   *
   * Augments the reduction state with an integer
   * for the remaining number of elements to drop.
   */
  def drop[A](n: Int): Transducer[A, A] = new StatefulTransducer[A, A] {
    def inner[S](f: Reducer[A, S]) = {
      def loop(count: Int, s: f.State): Reduction[A, S] = new Reduction[A, S] {
        def update(a: A) =
          if(count > 0) inContext(loop(count-1, s))
          else mapContext(f(s, a))(loop(0, _))
        def isReduced = count <= 0 && f.isReduced(s)
        def complete = f.complete(s)
      }
      mapContext(f.init)(loop(n, _))
    }
  }

  /**
   *  Takes the initial series of elements that satisfy a predicate.
   *
   *  This is a stateful transducer: it augments the reduction state
   *  with a boolean tracking the predicate value.
   *
   */
  def takeWhile[A](p: A => Boolean): Transducer[A, A] = new StatefulTransducer[A, A] {
    def inner[S](f: Reducer[A, S]) = {
      def loop(pass: Boolean, s: f.State): Reduction[A, S] = new Reduction[A, S] {
        def update(a: A) =
          if(pass && p(a)) mapContext(f(s, a))(loop(true, _))
          else inContext(loop(false, s))
        def isReduced = ! pass  || f.isReduced(s)
        def complete = f.complete(s)
      }
      mapContext(f.init)(loop(true, _))
    }
  }

  /**
   *  Drops the initial series of elements that satisfy a predicate.
   *
   *  Augments the reduction state with a boolean tracking
   *  the predicate value.
   *
   */
  def dropWhile[A](p: A => Boolean): Transducer[A, A] = new StatefulTransducer[A, A] {
    def inner[S](f: Reducer[A, S]) = {
      def loop(pass: Boolean, s: f.State): Reduction[A, S] = new Reduction[A, S] {
        def update(a: A) =
          if(pass || ! p(a)) mapContext(f(s, a))(loop(true, _))
          else inContext(loop(false, s))
        def isReduced = pass && f.isReduced(s)
        def complete = f.complete(s)
      }
      mapContext(f.init)(loop(false, _))
    }
  }

  /**
   * An unconditional stateful transducer
   */
  def transducer[A, B, T](t0: T)(r: (T, A) => (T, B)): Transducer[B, A] = new StatefulTransducer[B, A] {
    def inner[S](f: Reducer[B, S]) = {
      def loop(t: T, s: f.State): Reduction[A, S] = new Reduction[A, S] {
        def update(a: A) = {
          val (t1, b) = r(t, a)
          mapContext(f(s, b))(loop(t1, _))
        }
        def isReduced = f.isReduced(s)
        def complete = f.complete(s)
      }
      mapContext(f.init)(loop(t0, _))
    }
  }
}
