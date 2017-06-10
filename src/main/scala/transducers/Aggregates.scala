package transducers

trait Aggregates { this: Transducers with ContextIsId =>

  case class AggDouble(sum: Double, squares: Double, max: Double, min: Double, count: Int) {
    def +( x: Double) = AggDouble(
      x + sum,
      x*x + squares,
      x max max,
      x min min,
      1 + count
    )

    def +( rhs: AggDouble) = AggDouble(
      rhs.sum + sum,
      rhs.squares + squares,
      rhs.max + max,
      rhs.min + min,
      count + count
    )

    def isValid = count > 0
  }

  object AggDouble {
    def apply(): AggDouble = apply(0.0,  0.0, Double.MinValue, Double.MaxValue, 0)
    def apply(x: Double): AggDouble = apply(x, x*x, x, x, 1)
  }

  val aggDouble: Reducer[Double, AggDouble] = reducer(AggDouble())(_ + _)

  val count: Reducer[Any, Int] = reducer(0)((c, _) => c + 1)
}
