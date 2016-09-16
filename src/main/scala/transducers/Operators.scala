package transducers

trait Operators { this: Transducers =>

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
  def map[A, B](f: B => A): Transducer[A, B] = new Transducer[A, B] {
    def apply[S](r: Reducer[A, S]) = proxy(r) {
      (s, b) => r(s, f(b))
    }
  }

  /**
   * Fundamental transducer for filter.
   */
  def filter[A](p: A => Boolean): Transducer[A, A] = new Transducer[A, A] {
    def apply[S](r: Reducer[A, S]) = proxy(r) {
      (s, a) => if(p(a)) r(s, a) else inContext(s)
    }
  }

  /**
   * Map with a partial function
   */
  def collect[A, B](pf: PartialFunction[B, A]): Transducer[A, B] = new Transducer[A, B] {
    def apply[S](r: Reducer[A, S]) = proxy(r) {
      (s, b) => pf.andThen(r(s, _)).applyOrElse(b, (_: B) => inContext(s))
    }
  }

  /**
   * Fundamental transducer for flatMap.
   */
  def flatMap[A, B, G](g: B => G)(implicit e: Educible[G, A]): Transducer[A, B] = new Transducer[A, B] {
    def apply[S](f: Reducer[A, S]) = proxy(f) {
      (s0, b) =>
        val inner = new Reducer[A, f.State] {
          type State = f.State
          def init = s0
          def apply(s: State, a: A) = f(s, a)
          def isReduced(s: State) = f.isReduced(s)
          def complete(s: State) = s
        }
        reduce(g(b), inner)
    }
  }
}
