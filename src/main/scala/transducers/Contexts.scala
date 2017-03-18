package transducers

/**
 *  Realise Transducers for the unlifted case where
 *  a reduction step returns the new state directly as a value.
 *  This is the default case, obtained if transducers.api._ is imported.
 */
trait ContextIsId { this: Transducers =>
  type Context[+S] = S
  def inContext[S](s: S) = s
}

/**
 * Context is required to be a monad and one that can nest
 * flatMaps deeply.  That generally implies a trampoline or
 * an asynchronous executor is used for evaluation.
 */
trait ContextIsMonad { this: Transducers =>
  def inContext[S](s: S): Context[S]
  def lazyContext[S](s: => S): Context[S]
  def mapContext[S, T](c: Context[S])( f: S => T ): Context[T]
  def bindContext[S, T](c: Context[S])(f: S => Context[T]): Context[T]

  implicit class ContextBindOp[S](val c: Context[S]) {
    def flatMap[T](f: S => Context[T]) = bindContext(c)(f)
    def >>=[T](f: S => Context[T]) = bindContext(c)(f)
    def map[T](f: S => T): Context[T] = mapContext(c)(f)
  }
}
