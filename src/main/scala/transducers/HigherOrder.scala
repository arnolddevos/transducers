package transducers

trait HigherOrder { this: Transducers with ContextIsId =>

  def integrate[A, S]( f: Reducer[A, S]): Transducer[S, A] = new Transducer[S, A] {
    def apply[T]( h: Reducer[S, T]): Reducer[A, T] = new Reducer[A, T] {
      type State = (f.State, h.State)
      def init = (f.init, h.init)
      def apply(s: State, a: A): State = {
        val s1 = f(s._1, a)
        (s1, h(s._2, f.complete(s._1)))
      }
      def isReduced(s: State) = f.isReduced(s._1) || h.isReduced(s._2)
      def complete(s: State) = h.complete(s._2)
    }
  }

  def spans[A, S]( f: Reducer[A, S]): Transducer[S, A] = new Transducer[S, A] {
    def apply[T]( h: Reducer[S, T]): Reducer[A, T] = new Reducer[A, T] {
      type State = (f.State, h.State)
      def init = (f.init, h.init)
      @annotation.tailrec
      def apply(s: State, a: A): State = {
        if(f.isReduced(s._1)) apply((f.init, h(s._2, f.complete(s._1))), a)
        else (f(s._1, a), s._2)
      }
      def isReduced(s: State) =  h.isReduced(s._2)
      def complete(s: State) = h.complete(s._2)
    }
  }

  sealed trait Reduction[+A, +S]

  object Reduction {
    case class Perfect[+S](s: S) extends Reduction[Nothing, S]
    case class Partial[+A, +S](a: A, s: S) extends Reduction[A, S]
  }

  def takeUntil[A, S](p: A => Boolean)(f: Reducer[A, S]): Reducer[A,  Reduction[A, S]] = {
    new Reducer[A,  Reduction[A, S]] {

      import Reduction._
      type State = Reduction[A, f.State]
      def init = Perfect(f.init)

      def apply(s: State, a: A) = s match {
        case Perfect(fs) => if(p(a)) Partial(a, fs) else Perfect(f(fs, a))
        case _ => s
      }

      def isReduced(s: State) = s match {
        case Perfect(fs) => f.isReduced(fs)
        case _ => true
      }

      def complete(s: State): Reduction[A, S] = s match {
        case Perfect(fs) => Perfect(f.complete(fs))
        case Partial(a, fs) => Partial(a, f.complete(fs))
      }
    }
  }

  def asTransducer[A, S]( f: Reducer[A, S]): Transducer[S, A] = new Transducer[S, A] {
    def apply[T]( h: Reducer[S, T]): Reducer[A, T] = new Reducer[A, T] {
      type State = f.State
      def init = f.init
      def apply(s: State, a: A)= f(s, a)
      def isReduced(s: State) = f.isReduced(s)
      def complete(s: State): T = {
        val h0 = h.init
        val h1 = if(h.isReduced(h0)) h0 else h(h0, f.complete(s))
        h.complete(h1)
      }
    }
  }

  def chain[A, S]( fa: Reducer[A, S], fb: Reducer[A, S]): Transducer[S, A] = new Transducer[S, A] {
    def apply[T]( h: Reducer[S, T]): Reducer[A, T] = new Reducer[A, T] {

      sealed trait State { def sh: h.State }
      case class S1( sh: h.State, sa: fa.State) extends State
      case class S2( sh: h.State, sb: fb.State) extends State
      case class S3( sh: h.State ) extends State

      @annotation.tailrec
      def advance(s: State): State = s match {
        case S1(sh, sa) =>
          if (h.isReduced(sh)) S3(sh)
          else if(fa.isReduced(sa)) advance(S2(h(sh, fa.complete(sa)), fb.init))
          else s
        case S2(sh, sb) =>
          if (h.isReduced(sh)) S3(sh)
          else if(fb.isReduced(sb)) S3(h(sh, fb.complete(sb)))
          else s
        case _ => s
      }

      def init = advance(S1(h.init, fa.init))

      def apply(s: State, a: A)= s match {
        case S1(sh, sa) => advance(S1(sh, fa(sa, a)))
        case S2(sh, sb) => advance(S2(sh, fb(sb, a)))
        case _ => s
      }

      def isReduced(s: State) = s match {
        case S3(_) => true case _ => false
      }

      def complete(s: State): T = h.complete(s.sh)
    }
  }

  def mapReducer[A, S, T](g: S => T): Reducer[A, S] => Reducer[A, T] = {
    f =>  new Reducer[A, T] {
      type State = f.State
      def init = f.init
      def apply(s: State, a: A) = f(s, a)
      def isReduced(s: State) =  f.isReduced(s)
      def complete(s: State) =  g(f.complete(s))
    }
  }

  implicit class ReducerCominators[A, S]( f: Reducer[A, S]) {
    def map[T](g: S => T): Reducer[A, T] =  mapReducer(g)(f)
    def ~(f1: Reducer[A, S]): Transducer[S, A] = chain(f, f1)
  }
}
