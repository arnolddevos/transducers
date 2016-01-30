package transducers

trait Views { ops: Transducers  =>

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


}
