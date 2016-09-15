package transducers

trait ImmutableStateOperators { this: Transducers with ContextIsFunctor =>

  trait Reduction[A, S] {
    def isReduced: Boolean
    def update(a: A): Context[Reduction[A, S]]
    def complete: S
  }

  abstract class StatefulTransducer[A, B] extends Transducer[A, B] {
    def inner[S](r: Reducer[A, S]): Reduction[B, S]
    def apply[S](r: Reducer[A, S]): Reducer[B, S] = new Reducer[B, S] {
      type State = Reduction[B, S]
      final def init = inner(r)
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
    def inner[S](f: Reducer[A, S]): Reduction[A, S] = {
      case class Inner(count: Int, s: f.State) extends Reduction[A, S] {
        def update(a: A) =
          if(count > 0) mapContext(f(s, a))(Inner(count-1, _))
          else inContext(this)
        def isReduced = count <= 0 || f.isReduced(s)
        def complete = f.complete(s)
      }
      Inner(n, f.init)
    }
  }

  /**
   * Drops the first n elements.
   *
   * Augments the reduction state with an integer
   * for the remaining number of elements to drop.
   */
  def drop[A](n: Int): Transducer[A, A] = new StatefulTransducer[A, A] {
    def inner[S](f: Reducer[A, S]): Reduction[A, S] = {
      case class Inner(count: Int, s: f.State) extends Reduction[A, S] {
        def update(a: A) =
          if(count > 0) inContext(Inner(count-1, s))
          else mapContext(f(s, a))(Inner(0, _))
        def isReduced = count <= 0 && f.isReduced(s)
        def complete = f.complete(s)
      }
      Inner(n, f.init)
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
    def inner[S](f: Reducer[A, S]): Reduction[A, S] = {
      case class Inner(pass: Boolean, s: f.State) extends Reduction[A, S] {
        def update(a: A) =
          if(pass && p(a)) mapContext(f(s, a))(Inner(true, _))
          else inContext(Inner(false, s))
        def isReduced = ! pass  || f.isReduced(s)
        def complete = f.complete(s)
      }
      Inner(true, f.init)
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
    def inner[S](f: Reducer[A, S]): Reduction[A, S] = {
      case class Inner(pass: Boolean, s: f.State) extends Reduction[A, S] {
        def update(a: A) =
          if(pass || ! p(a)) mapContext(f(s, a))(Inner(true, _))
          else inContext(Inner(false, s))
        def isReduced = pass && f.isReduced(s)
        def complete = f.complete(s)
      }
      Inner(false, f.init)
    }
  }

  /**
   * An unconditional stateful transducer
   */
  def transducer[A, B, T](t0: T)(r: (T, A) => (T, B)): Transducer[B, A] = new StatefulTransducer[B, A] {
    def inner[S](f: Reducer[B, S]): Reduction[A, S] = {
      case class Inner(t: T, s: f.State) extends Reduction[A, S] {
        def update(a: A) = {
          val (t1, b) = r(t, a)
          mapContext(f(s, b))(Inner(t1, _))
        }
        def isReduced = f.isReduced(s)
        def complete = f.complete(s)
      }
      Inner(t0, f.init)
    }
  }
}
