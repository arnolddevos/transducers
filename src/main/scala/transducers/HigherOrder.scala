package transducers

trait HigherOrder { this: Transducers with ContextIsId =>

  def integrate[A, S]( f: Reducer[A, S]): Transducer[S, A] = new Transducer[S, A] {
    def apply[T]( h: Reducer[S, T]): Reducer[A, T] = new Reducer[A, T] {
      type State = (f.State, h.State)
      def init = (f.init, h.init)
      def apply(s: State, a: A): State = {
        val s1 = f(s._1, a)
        (s1, h(s._2, f.complete(s._1)))
      }
      def isReduced(s: State) = f.isReduced(s._1) || h.isReduced(s._2)
      def complete(s: State) = h.complete(s._2)
    }
  }

  def spans[A, S]( f: Reducer[A, S]): Transducer[S, A] = new Transducer[S, A] {
    def apply[T]( h: Reducer[S, T]): Reducer[A, T] = new Reducer[A, T] {
      type State = (f.State, h.State)
      def init = (f.init, h.init)
      @annotation.tailrec
      def apply(s: State, a: A): State = {
        if(f.isReduced(s._1)) apply((f.init, h(s._2, f.complete(s._1))), a)
        else (f(s._1, a), s._2)
      }
      def isReduced(s: State) =  h.isReduced(s._2)
      def complete(s: State) = h.complete(s._2)
    }
  }
  
}
