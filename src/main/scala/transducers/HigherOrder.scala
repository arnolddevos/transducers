package transducers

trait HigherOrder { this: Transducers with ContextIsId =>

  def spans[A, S]( f: Reducer[A, S]): Transducer[S, A] = new Transducer[S, A] {
    def apply[T]( h: Reducer[S, T]): Reducer[A, T] = new Reducer[A, T] {

      sealed trait State { def hs: h.State }
      case class Ready( hs: h.State) extends State
      case class Running( fs: f.State, hs: h.State) extends State

      def init = Ready(h.init)

      @annotation.tailrec
      def apply(s: State, a: A): State = s match {
        case Ready(hs) => apply(Running(f.init, hs), a)
        case Running(fs, hs) =>
          if(f.isReduced(fs)) apply(Running(f.init, h(hs, f.complete(fs))), a)
          else Running(f(fs, a), hs)
      }
      def isReduced(s: State) =  h.isReduced(s.hs)

      def complete(s: State) = s match {
        case Ready(hs) => h.complete(hs)
        case Running(fs, hs) =>
          if(h.isReduced(hs)) h.complete(hs)
          else h.complete(h(hs, f.complete(fs)))
      }
    }
  }

  def reduceByKey[A, K, V](f: Reducer[A, V])(p: A => K): Reducer[A, Map[K, V]] = new Reducer[A, Map[K, V]] {
    type State = Map[K, f.State]
    def init = Map()
    def apply(ss: State, a: A) = {
      val k = p(a)
      val ms = ss.get(k)
      ms.fold {
        val s = f.init
        ss.updated(k, if(f.isReduced(s)) s else f(s, a))
      } {
        s =>
          if(f.isReduced(s)) ss else ss.updated(k, f(s, a))
      }
    }
    def isReduced(ss: State) = false
    def complete(ss: State) = ss.mapValues(f.complete)
  }

  sealed trait ReductionResult[+A, +S]

  object ReductionResult {
    case class Perfect[+S](s: S) extends ReductionResult[Nothing, S]
    case class Partial[+A, +S](a: A, s: S) extends ReductionResult[A, S]
  }

  def takeUntil[A, S](p: A => Boolean)(f: Reducer[A, S]): Reducer[A,  ReductionResult[A, S]] = {
    new Reducer[A,  ReductionResult[A, S]] {

      import ReductionResult._
      type State = ReductionResult[A, f.State]
      def init = Perfect(f.init)

      def apply(s: State, a: A) = s match {
        case Perfect(fs) => if(p(a)) Partial(a, fs) else Perfect(f(fs, a))
        case _ => s
      }

      def isReduced(s: State) = s match {
        case Perfect(fs) => f.isReduced(fs)
        case _ => true
      }

      def complete(s: State): ReductionResult[A, S] = s match {
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
