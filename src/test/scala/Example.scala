object Example extends App {

  import transducers._

  {
    val l = List(1, 2, 3)
    val f = reducer[Int, Int](0)(_ + _)
    val s = educe(l, f) // s == 6
    println(s)
  }
  {
    val l = List(1.0, 2.0, 3.0)
    def f(s: Double, a: Double) = s*0.9 + a*0.1
    val r = reducer(0.0)(f)
    val s = educe(l, r) // s == 
    println(s)
  }
  {
    val l = List(1.0, 2.0, 3.0)
    def f(s: Double, a: Double) = s*0.9 + a*0.1
    val r1 = reducer(0.0)(f)
    val r2 = take(2)(r1)
    val s = educe(l, r2) // s == 
    println(s)
  }
  {
    val l = List(1.0, 2.0, 3.0)
    def f(s: Double, a: Double) = s*0.9 + a*0.1
    val r = reducer(0.0)(f)
    def take2[X] = take[X](2)
    val s = transduce(l, take2[Double], r) // s == 
    println(s)
  }
  {
    val l = List(1.0, 2.0, 3.0)
    def f(s: Double, a: Double) = s*0.9 + a*0.1
    val r = reducer(0.0)(f)
    def take2[X] = take[X](2)
    def square = map((x: Double) => x*x) 
    val s = transduce(l, take2 compose square, r) // s == 
    println(s)
  }
  {
    val l = List("1.0", "2.0", "3.0")
    val r = reducer(0.0)((s, a: Double) => s*0.9 + a*0.1)
    val asDouble = map((s: String) => s.toDouble)
    def take2[X] = take[X](2)
    def square = map((x: Double) => x*x) 
    val s = transduce(l, asDouble compose take2 compose square, r) // s == 
    println(s)
  }
  {
    val l = List("1.0", "2.0", "3.0")
    val r = reducer(0.0)((s, a: Double) => s*0.9 + a*0.1)
    val asDouble = map((s: String) => s.toDouble)
    def take2[X] = take[X](2)
    def square = map((x: Double) => x*x) 
    val s = transduce(l, take2 compose asDouble compose take2 compose square, r) // s == 
    println(s)
  }
}
