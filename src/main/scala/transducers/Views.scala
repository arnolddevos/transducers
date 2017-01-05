package transducers

trait Views { ops: Transducers with Operators =>

  /**
   * A lazily evaluated data source consisting of a base which is educible and a transducer.
   */
  trait View[A] { self =>
    type Elem
    type Base
    def base: Base
    def isEducible: Educible[Base, Elem]
    def trans: Transducer[A, Elem]

    def transform[B](t: Transducer[B, A]): View[B] = new View[B] {
      type Elem = self.Elem
      type Base = self.Base
      val base = self.base
      val isEducible = self.isEducible
      val trans = self.trans compose t
    }

    def map[B](f: A => B) = transform(ops.map(f))
    def flatMap[H, B](f: A => H)(implicit e1: Educible[H, B]) = transform(ops.flatMap(f))
    def withFilter(p: A => Boolean) = transform(ops.filter(p))
    def >>=[H, B](f: A => H)(implicit e1: Educible[H, B]) = flatMap(f)
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
