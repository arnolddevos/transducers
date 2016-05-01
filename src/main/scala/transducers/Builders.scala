package transducers

import collection.mutable.{Builder, ListBuffer, ArrayBuilder}
import collection.immutable.VectorBuilder
import reflect.ClassTag

trait Builders { this: Transducers =>

  def buildString[X](s1: String="", s2: String="", s3: String=""): Reducer[X, String] = new Reducer[X, String] {
    class State {
      var builder = new java.lang.StringBuilder(s1)
      var count = 0
    }
    def init = new State
    def apply(s: State, x: X) = inContext {
      if(s.count > 0) s.builder append s2
      s.builder append x.toString
      s.count += 1
      s
    }
    def isReduced(s: State) = false
    def complete(s: State) = {
      s.builder append s3
      s.builder.toString
    }
  }

  def builder[X, S](br: => Builder[X, S]): Reducer[X, S] = new Reducer[X, S] {
    type State = Builder[X, S]
    def init = br
    def apply(xs: State, x: X) = inContext { xs += x }
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
