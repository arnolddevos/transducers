package transducers

/**
 *  Realise Transducers for the unlifted case where 
 *  a reduction step returns the new state directly as a value.
 */
trait Educers { this: Transducers =>

  type Context[+S] = S
  def inContext[S](s: S) = s
  def mapContext[S, T](s: S)( f: S => T ) = f(s)

  implicit def listIsEducible[X] = new Educible[List[X], X] {
    def educe[S]( xs: List[X], f: Reducer[X, S]): S = {
      @annotation.tailrec
      def loop( xs: List[X], s: f.State ): S = {
        if(xs.isEmpty || f.isReduced(s)) f.complete(s)
        else loop(xs.tail, f(s, xs.head)) 
      }
      loop(xs, f.init)
    }
  }

  implicit def optionIsEducible[X] = new Educible[Option[X], X] {
    def educe[S]( xs: Option[X], f: Reducer[X, S]): S = {
      val s0 = f.init
      if(xs.isEmpty || f.isReduced(s0)) f.complete(s0)
      else f.complete(f(s0, xs.get))
    }
  }

  implicit def iteratorIsEducible[X] = new Educible[Iterator[X], X] {
    def educe[S]( xs: Iterator[X], f: Reducer[X, S]): S = {
      var s = f.init
      while(xs.hasNext && ! f.isReduced(s)) 
        s = f(s, xs.next)
      f.complete(s)
    }
  }

  implicit def iterableIsEducible[X] = new Educible[Iterable[X], X] {
    def educe[S]( xs: Iterable[X], f: Reducer[X, S]): S = 
      iteratorIsEducible.educe[S](xs.iterator, f)
  }

  implicit def vectorIsEducible[X] = new Educible[Vector[X], X] {
    def educe[S]( xs: Vector[X], f: Reducer[X, S]): S = 
      iteratorIsEducible.educe[S](xs.iterator, f)
  }

  implicit def setIsEducible[X] = new Educible[Set[X], X] {
    def educe[S]( xs: Set[X], f: Reducer[X, S]): S = 
      iteratorIsEducible.educe[S](xs.iterator, f)
  }

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
}
