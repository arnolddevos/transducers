package transducers

trait Syntax { this: Transducers =>
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

    /**
     * Run this reducer.
     */
    def >>:[G](lhs: G)(implicit e: Educible[G, A]): Context[S] = e.educe(lhs, rhs)
  }
}
