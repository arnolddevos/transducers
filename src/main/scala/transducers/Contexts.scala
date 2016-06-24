package transducers

/**
 *  Realise Transducers for the unlifted case where
 *  a reduction step returns the new state directly as a value.
 *  This is the default case, obtained if transducers._ is imported.
 */
trait ContextIsId extends ContextIsMonad { this: Transducers =>
  type Context[+S] = S
  def inContext[S](s: S) = s
  def mapContext[S, T](s: S)( f: S => T ) = f(s)
  def bindContext[S, T](s: S)(f: S => T) = f(s)
}

/**
 * The ability to lift a value into Context is required.
 */
trait ContextIsPure { this: Transducers =>
  def inContext[S](s: S): Context[S]
}

/**
 *  Context is required to be a functor.
 *  Definitions are needed for the operations,
 *  declared here.
 *
 * If there is a Functor typeclass instance for Context
 * (from scalaz or cats perhaps) then mapContext can delegate to it.
 */
trait ContextIsFunctor extends ContextIsPure { this: Transducers =>
  def mapContext[S, T](c: Context[S])( f: S => T ): Context[T]
}

/**
 * Context is required to be a monad and one that can nest
 * flatMaps deeply.  That generally implies a trampoline or
 * an asynchronous executor is used for evaluation.
 *
 * If there is a Monad typeclass instance for Context
 * (from scalaz or cats perhaps) then bindContext can delegate to it.
 */
trait ContextIsMonad extends ContextIsFunctor  { this: Transducers =>
  def bindContext[S, T](c: Context[S])(f: S => Context[T]): Context[T]
}
