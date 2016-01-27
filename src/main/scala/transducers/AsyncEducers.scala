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
  def inContext[S](s: S): Context[S]

  implicit val listIsEducible = new Educible[List] {
    def educe[X, S]( xs: List[X], f: Reducer[X, S]): Context[S] = {
      def loop( xs: List[X], s: f.State ): Context[S] = {
        if(xs.isEmpty || f.isReduced(s)) inContext(f.complete(s))
        else bindContext(f(s, xs.head))(loop(xs.tail, _)) 
      }
      loop(xs, f.init)
    }
  }

  implicit val vectorIsEducible = new Educible[Vector] {
    def educe[X, S]( xs: Vector[X], f: Reducer[X, S]): Context[S] = {
      val mx = xs.size
      def loop( ix: Int, s: f.State ): Context[S] = {
        if(ix >= mx || f.isReduced(s)) inContext(f.complete(s))
        else bindContext(f(s, xs(ix)))(loop(ix+1, _)) 
      }
      loop(0, f.init)
    }
  }

  implicit val optionIsEducible = new Educible[Option] {
    def educe[X, S]( xs: Option[X], f: Reducer[X, S]): Context[S] = {
      val s0 = f.init
      if(xs.isEmpty || f.isReduced(s0)) inContext(f.complete(s0))
      else mapContext(f(s0, xs.get))(f.complete)
    }
  }
}
