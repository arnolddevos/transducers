package object transducers extends Transducers with Views with Operators with Educers {
  def inContext[S](s: S) = s
  def mapContext[S, T](s: S)( f: S => T ) = f(s)
}
