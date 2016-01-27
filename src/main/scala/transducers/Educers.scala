package transducers

/**
 *  Realise Transducers for the unlifted case where 
 *  a reduction step returns the new state directly as a value.
 */
trait Educers { this: Transducers =>

  type Context[+S] = S

  implicit val listIsEducible = new Educible[List] {
    def educe[X, S]( xs: List[X], f: Reducer[X, S]): S = {
      @annotation.tailrec
      def loop( xs: List[X], s: f.State ): S = {
        if(xs.isEmpty || f.isReduced(s)) f.complete(s)
        else loop(xs.tail, f(s, xs.head)) 
      }
      loop(xs, f.init)
    }
  }

  implicit val optionIsEducible = new Educible[Option] {
    def educe[X, S]( xs: Option[X], f: Reducer[X, S]): S = {
      val s0 = f.init
      if(xs.isEmpty || f.isReduced(s0)) f.complete(s0)
      else f.complete(f(s0, xs.get))
    }
  }

  implicit val iteratorIsEducible = new Educible[Iterator] {
    def educe[X, S]( xs: Iterator[X], f: Reducer[X, S]): S = {
      var s = f.init
      while(xs.hasNext && ! f.isReduced(s)) 
        s = f(s, xs.next)
      f.complete(s)
    }
  }

  implicit val iterableIsEducible = new Educible[Iterable] {
    def educe[X, S]( xs: Iterable[X], f: Reducer[X, S]): S = 
      iteratorIsEducible.educe[X, S](xs.iterator, f)
  }

  implicit val vectorIsEducible = new Educible[Vector] {
    def educe[X, S]( xs: Vector[X], f: Reducer[X, S]): S = 
      iteratorIsEducible.educe[X, S](xs.iterator, f)
  }

  implicit val setIsEducible = new Educible[Set] {
    def educe[X, S]( xs: Set[X], f: Reducer[X, S]): S = 
      iteratorIsEducible.educe[X, S](xs.iterator, f)
  }
}
