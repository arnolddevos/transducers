package transducers
package lifted

trait Eduction { this: Transducers with Induction with ContextIsMonad =>

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

  implicit def producerIsEducible[X] = new Educible[Producer[X], X] {
    import Series._

    def educe[S](g: Producer[X], f: Reducer[X, S]): Context[S] = {
      def loop(g: Producer[X], s: f.State ): Context[S] = {
        if(f.isReduced(s)) f.complete(s)
        else
          g >>= {
            _ match {
              case NonEmpty(a, g1) => f(s, a) >>= (loop(g1, _))
              case Empty => f.complete(s)
            }
          }
      }
      f.init >>= (loop(g, _))
    }
  }

  implicit def seriesIsEducible[X] = new Educible[Series[X], X] {
    def educe[S]( xs: Series[X], f: Reducer[X, S]): Context[S] =
      producerIsEducible[X].educe[S](inContext(xs), f)
  }
}
