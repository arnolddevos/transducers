package transducers

trait AsyncOperators { this: Transducers with ContextIsMonad with ImmutableStateOperators =>

  def prefix[A](a0: A): Transducer[A, A] = new Transducer[A, A] {
    def apply[S](f: Reducer[A, S]): Reducer[A, S] = new Reducer[A, S] {
      type State = f.State
      def init =
        bindContext(f.init) {
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
        else bindContext(f(s, an))(f.complete)
    }
  }

  def integrate[A, S](g: Reducer[A, S]): Transducer[S, A] = new StatefulTransducer[S, A] {
    def inner[T](f: Reducer[S, T]) = {
      def loop(sg: g.State, sf: f.State): Reduction[A, T] = new Reduction[A, T] {
        def update(a: A) =
          bindContext(g(sg, a)) { sg1 =>
            bindContext(g.complete(sg1)) { s =>
              mapContext(f(sf, s)) { sf1 =>
                loop(sg1, sf1)
              }
            }
          }
        def isReduced = g.isReduced(sg) || f.isReduced(sf)
        def complete = f.complete(sf)
      }
      bindContext(g.init)(sg => mapContext(f.init)(sf => loop(sg, sf)))
    }
  }
}
