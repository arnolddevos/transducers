package transducers 

import collection.mutable.{Builder, ListBuffer, ArrayBuilder}
import collection.immutable.VectorBuilder
import reflect.ClassTag

trait Builders { this: Transducers =>

  def builder[X, S](br: => Builder[X, S]): Reducer[X, S] = new Reducer[X, S] {
    type State = Builder[X, S]
    def init = br
    def apply(xs: State, x: X) = inContext { xs += x; xs }
    def isReduced(xs: State) = false
    def complete(xs: State) = xs.result
  }

  def toVector[X] = builder(new VectorBuilder[X])

  def toList[X] = builder(new ListBuffer[X])

  def toArray[X:ClassTag] = builder(ArrayBuilder.make[X])

  def toSet[X]: Reducer[X, Set[X]] = reducer(Set.empty[X])((xs, x) => inContext(xs + x))

  def toVectorSlow[X]: Reducer[X, Vector[X]] = reducer(Vector.empty[X])((xs, x) => inContext(xs :+ x))

  def toListSlow[X]: Reducer[X, List[X]] = new Reducer[X, List[X]] {
    type State = List[X]
    def init = Nil
    def apply(xs: List[X], x: X) = inContext(x :: xs)
    def isReduced(xs: List[X]) = false
    def complete(xs: List[X]) = xs.reverse
  }

  def headOption[X]: Reducer[X, Option[X]] = new Reducer[X, Option[X]] {
    type State = Option[X]
    def init = None
    def apply(xs: Option[X], x: X) = inContext(if(xs.isDefined) xs else Some(x))
    def isReduced(xs: Option[X]) = xs.isDefined
    def complete(xs: Option[X]) = xs
  }
}
