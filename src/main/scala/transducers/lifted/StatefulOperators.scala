package transducers
package lifted

trait StatefulOperators { this: Transducers with ContextIsMonad =>

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
          if(count > 0) f(s, a) map (loop(count-1, _))
          else inContext(this)
        def isReduced = count <= 0 || f.isReduced(s)
        def complete = f.complete(s)
      }
      f.init map (loop(n, _))
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
          else f(s, a) map (loop(0, _))
        def isReduced = count <= 0 && f.isReduced(s)
        def complete = f.complete(s)
      }
      f.init map (loop(n, _))
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
          if(pass && p(a)) f(s, a) map (loop(true, _))
          else inContext(loop(false, s))
        def isReduced = ! pass  || f.isReduced(s)
        def complete = f.complete(s)
      }
      f.init map (loop(true, _))
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
          if(pass || ! p(a)) f(s, a) map (loop(true, _))
          else inContext(loop(false, s))
        def isReduced = pass && f.isReduced(s)
        def complete = f.complete(s)
      }
      f.init map (loop(false, _))
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
          f(s, b) map (loop(t1, _))
        }
        def isReduced = f.isReduced(s)
        def complete = f.complete(s)
      }
      mapContext(f.init)(loop(t0, _))
    }
  }

  def prefix[A](a0: A): Transducer[A, A] = new Transducer[A, A] {
    def apply[S](f: Reducer[A, S]): Reducer[A, S] = new Reducer[A, S] {
      type State = f.State
      def init =
        f.init >>= {
          s0 =>
            if(f.isReduced(s0)) inContext(s0)
            else f(s0, a0)
        }

      def isReduced(s: State) = f.isReduced(s)
      def apply(s: State, a: A) = f(s, a)
      def complete(s: State) = f.complete(s)
    }
  }

  def suffix[A](an: A): Transducer[A, A] = new Transducer[A, A] {
    def apply[S](f: Reducer[A, S]): Reducer[A, S] = new Reducer[A, S] {
      type State = f.State
      def init = f.init
      def isReduced(s: State) = f.isReduced(s)
      def apply(s: State, a: A) = f(s, a)
      def complete(s: State) =
        if(f.isReduced(s)) f.complete(s)
        else f(s, an) >>= f.complete
    }
  }

  def integrate[A, S](g: Reducer[A, S]): Transducer[S, A] = new StatefulTransducer[S, A] {
    def inner[T](f: Reducer[S, T]) = {
      def loop(sg: g.State, sf: f.State): Reduction[A, T] = new Reduction[A, T] {
        def update(a: A) =
          g(sg, a) >>= { sg1 =>
            g.complete(sg1) >>= { s =>
              f(sf, s) map { sf1 =>
                loop(sg1, sf1)
              }
            }
          }
        def isReduced = g.isReduced(sg) || f.isReduced(sf)
        def complete = f.complete(sf)
      }
      g.init >>= (sg => f.init map (sf => loop(sg, sf)))
    }
  }
}
