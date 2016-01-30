package transducers

trait Syntax { ops: Transducers with Views with Operators =>
  implicit class TransducerOps[A, B]( rhs: Transducer[A, B] ) {

    /**
     * Compose two transducers forming a new transducer.
     */
    def ->:[C]( lhs: Transducer[B, C]): Transducer[A, C] = rhs andThen lhs
  }

  implicit class ReducerOps[A, S]( rhs: Reducer[A, S]) {

    /**
     * Compose reducer with transducer forming a new reducer.
     */
    def ->:[B]( lhs: Transducer[A, B]): Reducer[B, S] = lhs apply rhs
  }

  implicit class EductionOps[G, A]( g: G )( implicit e: Educible[G, A]) {

    /**
     * Educe values and run a reduction.
     */
    def :-/[S]( rhs: Reducer[A, S]): Context[S] = e.educe(g, rhs)

    /**
     * Compose an educible with a transducer forming a View.
     */
    def :->[B]( rhs: Transducer[B, A]): View[B] = view(g, rhs)
  
    /**
     *  Map operator needed by for comprehensions implemented as a View.
     */
    def map[B](f: A => B) = view(g, ops.map(f))

    /**
     *  flatMap operator needed by for comprehensions implemented as a View.
     */
    def flatMap[H, B](f: A => H)(implicit e1: Educible[H, B]) = view(g, ops.flatMap(f))

    /**
     * Filter operator needed by for comprehansions implemented as a View.
     */
    def withFilter(p: A => Boolean) = view(g, ops.filter(p))

    /**
     *  Often used flatMap symbol.
     */
    def >>=[H, B](f: A => H)(implicit e1: Educible[H, B]) = flatMap(f)
  }  
}
