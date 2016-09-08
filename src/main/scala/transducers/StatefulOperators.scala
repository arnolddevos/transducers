package transducers

trait StatefulOperators { this: Transducers with ContextIsId =>

  trait MutableReduction[A, S] {
    def isReduced: Boolean
    def update(a: A): Unit
    def complete: S
  }

  abstract class StatefulTransducer[A, B] extends Transducer[A, B] {
    def inner[S](r: Reducer[A, S]): MutableReduction[B, S]
    def apply[S](r: Reducer[A, S]): Reducer[B, S] = new Reducer[B, S] {
      type State = MutableReduction[B, S]
      def init = inner(r)
      def isReduced(s: State) = s.isReduced
      def apply(s: State, b: B) = { s.update(b); s }
      def complete(s: State) = s.complete
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

}
