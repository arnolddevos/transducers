package transducers

import scala.language.higherKinds

trait Views { this: Transducers =>

  /**
   * A lazily evaluated data source consisting of a base which is educible and a transducer.
   */
  trait View[A] {
    type Elem
    type Base[_]
    def base: Base[Elem]
    def isEducible: Educible[Base]
    def trans: Transducer[A, Elem]
  }

  implicit def viewIsEducible = new Educible[View] {
    def educe[A, S](v: View[A], f: Reducer[A, S]): Context[S] =
      transduce(v.base, v.trans, f)(v.isEducible)
  }

  def view[R[_], A, B](rb: R[B], t: Transducer[A, B])(implicit e: Educible[R]): View[A] = new View[A] {
    type Elem = B
    type Base[X] = R[X]
    val base = rb
    def isEducible = e
    def trans = t
  }

  def view[R[_], B](rb: R[B])(implicit e: Educible[R]): View[B] = view[R, B, B](rb, cat[B])
}
