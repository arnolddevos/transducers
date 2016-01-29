package transducers

trait Views { ops: Transducers with Operators =>

  /**
   * A lazily evaluated data source consisting of a base which is educible and a transducer.
   */
  trait View[A] {
    type Elem
    type Base
    def base: Base
    def isEducible: Educible[Base, Elem]
    def trans: Transducer[A, Elem]
  }

  implicit def viewIsEducible[A] = new Educible[View[A], A] {
    def educe[S](v: View[A], f: Reducer[A, S]): Context[S] =
      transduce(v.base, v.trans, f)(v.isEducible)
  }

  def view[G, A, B](g: G, t: Transducer[A, B])(implicit e: Educible[G, B]): View[A] = new View[A] {
    type Elem = B
    type Base = G
    val base = g
    def isEducible = e
    def trans = t
  }

  def view[G, B](g: G)(implicit e: Educible[G, B]): View[B] = view[G, B, B](g, cat[B])


  /**
   *  Standard operators for Educibles including those needed
   *  by for comprehensions implemented as Views.
   */
  implicit class EductionOps[G, A]( g: G )(implicit e: Educible[G, A]) {
    def map[B](f: A => B) = view(g, ops.map(f))
    def flatMap[H, B](f: A => H)(implicit e1: Educible[H, B]) = view(g, ops.flatMap(f))
    def >>=[H, B](f: A => H)(implicit e1: Educible[H, B]) = flatMap(f)
    def >>[H, B]( k: => H )(implicit e1: Educible[H, B]) = flatMap(_ => k)
    def withFilter(p: A => Boolean) = view(g, ops.filter(p))
  }
}
