package transducers
package lifted

trait Educers { this: Transducers with ContextIsMonad =>

  implicit def listIsEducible[X] = new Educible[List[X], X] {
    def educe[S]( xs: List[X], f: Reducer[X, S]): Context[S] = {
      def loop( xs: List[X], s: f.State ): Context[S] = {
        if(xs.isEmpty || f.isReduced(s)) f.complete(s)
        else f(s, xs.head) >>= (loop(xs.tail, _))
      }
      f.init >>= (loop(xs, _))
    }
  }

  implicit def vectorIsEducible[X] = new Educible[Vector[X], X] {
    def educe[S]( xs: Vector[X], f: Reducer[X, S]): Context[S] = {
      val mx = xs.size
      def loop( ix: Int, s: f.State ): Context[S] = {
        if(ix >= mx || f.isReduced(s)) f.complete(s)
        else f(s, xs(ix)) >>= (loop(ix+1, _))
      }
      f.init >>= (loop(0, _))
    }
  }

  implicit def optionIsEducible[X] = new Educible[Option[X], X] {
    def educe[S]( xs: Option[X], f: Reducer[X, S]): Context[S] = {
      f.init >>= { s0 =>
        if(xs.isEmpty || f.isReduced(s0)) f.complete(s0)
        else f(s0, xs.get) >>= f.complete
      }
    }
  }
}
