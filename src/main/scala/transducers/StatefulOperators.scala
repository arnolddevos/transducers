package transducers

import scala.collection.mutable.HashSet

trait StatefulOperators { this: Transducers with ContextIsId =>

  trait MutableReduction[A, S] {
    def isReduced: Boolean
    def update(a: A): Unit
    def complete: S
  }

  abstract class MutableReducer[A, S] extends Reducer[A, S] {
    type State = MutableReduction[A, S]
    final def isReduced(s: State) = s.isReduced
    final def apply(s: State, a: A) = { s.update(a); s }
    final def complete(s: State) = s.complete
  }

  abstract class StatefulTransducer[A, B] extends Transducer[A, B] {
    def inner[S](r: Reducer[A, S]): MutableReduction[B, S]
    def apply[S](r: Reducer[A, S]): Reducer[B, S] = new MutableReducer[B, S] {
      final def init = inner(r)
    }
  }

  def take[A](n: Int): Transducer[A, A] = new StatefulTransducer[A, A] {
    def inner[S](f: Reducer[A, S]) = new MutableReduction[A, S] {
      var count = n
      var s = f.init
      def update(a: A) = if(count > 0) { s = f(s, a); count -= 1 }
      def isReduced = count <= 0 || f.isReduced(s)
      def complete = f.complete(s)
    }
  }

  def drop[A](n: Int): Transducer[A, A] = new StatefulTransducer[A, A] {
    def inner[S](f: Reducer[A, S]) = new MutableReduction[A, S] {
      var count = n
      var s = f.init
      def update(a: A) = if(count > 0) count -= 1 else s = f(s, a)
      def isReduced = count <= 0 && f.isReduced(s)
      def complete = f.complete(s)
    }
  }

  def takeWhile[A](p: A => Boolean): Transducer[A, A] = new StatefulTransducer[A, A] {
    def inner[S](f: Reducer[A, S]) = new MutableReduction[A, S] {
      var pass = true
      var s = f.init
      def update(a: A) = if(pass && p(a)) s = f(s, a) else pass = false
      def isReduced = ! pass  || f.isReduced(s)
      def complete = f.complete(s)
    }
  }

  def dropWhile[A](p: A => Boolean): Transducer[A, A] = new StatefulTransducer[A, A] {
    def inner[S](f: Reducer[A, S]) = new MutableReduction[A, S] {
      var pass = false
      var s = f.init
      def update(a: A) = if(pass || ! p(a)) { s = f(s, a); pass = true }
      def isReduced = pass && f.isReduced(s)
      def complete = f.complete(s)
    }
  }

  def unique[A]: Transducer[A, A] = new StatefulTransducer[A, A] {
    def inner[S](f: Reducer[A, S]) = new MutableReduction[A, S] {
      val as = HashSet[A]()
      var s = f.init
      def update(a: A) = if( ! (as contains a)) { as += a; s = f(s, a) }
      def isReduced = f.isReduced(s)
      def complete = f.complete(s)
    }
  }

  def group[K, V, A](g: Reducer[A, V])(p: A => K) = new StatefulTransducer[(K, V), A] {
    def inner[S](f: Reducer[(K, V), S]) = new MutableReduction[A, S] {
      var s = f.init
      var t = g.init
      var d: Option[K] = None
      def update( a: A) = {
        val i = p(a)
        if(d.isEmpty) d = Some(i)
        else
          for( j <- d if j != i ) {
            s = f(s, (j, g.complete(t)))
            t = g.init
            d = Some(i)
          }
        if(!g.isReduced(t)) t = g(t, a)
      }
      def isReduced = f.isReduced(s)
      def complete = {
        for( j <- d)
          if(!f.isReduced(s)) s = f(s, (j, g.complete(t)))
        f.complete(s)
      }
    }
  }

  def prefix[A](a0: A): Transducer[A, A] = new Transducer[A, A] {
    def apply[S](f: Reducer[A, S]): Reducer[A, S] = new Reducer[A, S] {
      type State = f.State

      def init = {
        val s0 = f.init
        if(f.isReduced(s0)) s0
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
        else f.complete(f(s, an))
    }
  }

  def interleave[A](an: A): Transducer[A, A] = new StatefulTransducer[A, A] {
    def inner[S](f: Reducer[A, S]) = new MutableReduction[A, S] {
      var sep = false
      var s = f.init
      def update(a: A) = {
        if(sep) s = f(s, an)
        if( ! f.isReduced(s)) s = f(s, a)
        sep = true
      }
      def isReduced = f.isReduced(s)
      def complete = f.complete(s)
    }
  }

  def integrate[A, S]( g: Reducer[A, S]): Transducer[S, A] = new StatefulTransducer[S, A] {
    def inner[T]( f: Reducer[S, T]) = new MutableReduction[A, T] {
      var sg = g.init
      var sf = f.init
      def update(a: A) = {
        sg = g(sg, a)
        sf = f(sf, g.complete(sg))
      }
      def isReduced = f.isReduced(sf) || g.isReduced(sg)
      def complete = f.complete(sf)
    }
  }
}
