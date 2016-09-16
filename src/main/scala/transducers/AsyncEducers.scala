package transducers

trait AsyncEducers { this: Transducers with ContextIsMonad =>

  implicit def listIsEducible[X] = new Educible[List[X], X] {
    def educe[S]( xs: List[X], f: Reducer[X, S]): Context[S] = {
      def loop( xs: List[X], s: f.State ): Context[S] = {
        if(xs.isEmpty || f.isReduced(s)) f.complete(s)
        else bindContext(f(s, xs.head))(loop(xs.tail, _))
      }
      bindContext(f.init)(loop(xs, _))
    }
  }

  implicit def vectorIsEducible[X] = new Educible[Vector[X], X] {
    def educe[S]( xs: Vector[X], f: Reducer[X, S]): Context[S] = {
      val mx = xs.size
      def loop( ix: Int, s: f.State ): Context[S] = {
        if(ix >= mx || f.isReduced(s)) f.complete(s)
        else bindContext(f(s, xs(ix)))(loop(ix+1, _))
      }
      bindContext(f.init)(loop(0, _))
    }
  }

  implicit def optionIsEducible[X] = new Educible[Option[X], X] {
    def educe[S]( xs: Option[X], f: Reducer[X, S]): Context[S] = {
      bindContext(f.init) { s0 =>
        if(xs.isEmpty || f.isReduced(s0)) f.complete(s0)
        else bindContext(f(s0, xs.get))(f.complete)
      }
    }
  }
}
