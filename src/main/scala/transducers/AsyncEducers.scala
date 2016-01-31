package transducers

trait AsyncEducers { this: Transducers =>

  /**
   * Context is required to be a monad and one that can nest
   * flatMaps deeply.  That generally implies a trampoline or
   * an asynchronous executor is used for evaluation.
   * 
   * If there is a Monad typeclass instance for Context 
   * (from scalaz or cats perhaps) then bindContext, 
   * mapContext and inContext can delegate to it.
   */
  def bindContext[A, B](ca: Context[A])(f: A => Context[B]): Context[B]
  def mapContext[S, T](c: Context[S])( f: S => T ): Context[T]

  implicit def listIsEducible[X] = new Educible[List[X], X] {
    def educe[S]( xs: List[X], f: Reducer[X, S]): Context[S] = {
      def loop( xs: List[X], s: f.State ): Context[S] = {
        if(xs.isEmpty || f.isReduced(s)) inContext(f.complete(s))
        else bindContext(f(s, xs.head))(loop(xs.tail, _)) 
      }
      loop(xs, f.init)
    }
  }

  implicit def vectorIsEducible[X] = new Educible[Vector[X], X] {
    def educe[S]( xs: Vector[X], f: Reducer[X, S]): Context[S] = {
      val mx = xs.size
      def loop( ix: Int, s: f.State ): Context[S] = {
        if(ix >= mx || f.isReduced(s)) inContext(f.complete(s))
        else bindContext(f(s, xs(ix)))(loop(ix+1, _)) 
      }
      loop(0, f.init)
    }
  }

  implicit def optionIsEducible[X] = new Educible[Option[X], X] {
    def educe[S]( xs: Option[X], f: Reducer[X, S]): Context[S] = {
      val s0 = f.init
      if(xs.isEmpty || f.isReduced(s0)) inContext(f.complete(s0))
      else mapContext(f(s0, xs.get))(f.complete)
    }
  }

  def integrate[A, S]( f: Reducer[A, S]): Transducer[S, A] = new Transducer[S, A] {
    def apply[T]( h: Reducer[S, T]): Reducer[A, T] = new Reducer[A, T] {
      type State = (f.State, h.State)
      def init = (f.init, h.init)
      def apply(s: State, a: A): Context[State] = {
        val cs1 = f(s._1, a)
        bindContext(cs1)( s1 => mapContext(h(s._2, f.complete(s._1)))(s2 => (s1, s2)))
      }
      def isReduced(s: State) = f.isReduced(s._1) || h.isReduced(s._2)
      def complete(s: State) = h.complete(s._2)
    }
  }
}
