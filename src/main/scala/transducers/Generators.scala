package transducers

trait Generators { this: Transducers with SeriesDefs with ContextIsMonad =>

  import Series._
  import Generator._

  trait Generator[+A]  {
    def next: Context[Series[A]]

    def append[B >: A](that: Generator[B]) = Generator(Ops.concat(next, that.next))
    def fold[S](z: S)(f: (S, A) => S) = Ops.fold(next)(z)(f)
    def reduce[S](f: Reducer[A, S]) = Ops.reduce(next)(f)
    def foldp[S](z: S)(f: (S, A) => Context[S]) = Ops.foldp(next)(z)(f)
    def sink = Ops.sink(next)
    def +:[B >: A](b: B) = Generator(Ops.prepend(b, next))
    def map[B](f: A => B) = Generator(Ops.map(next)(f))
    def flatMap[B](f: A => Generator[B]) = Generator(Ops.flatMap(next)(f andThen (_.next)))
    def filter(f: A => Boolean) = Generator(Ops.filter(next)(f))
    def split[B](f: (A, Generator[A]) => Generator[B]) = Generator(Ops.split(next)((a, cs) => f(a, Generator(cs)).next))
  }

  object Generator {
    val empty: Generator[Nothing] = fromSeries(Series())
    def singleton[A](a: A): Generator[A] = fromSeries(Series.singleton(a))
    def fromList[A](as: List[A]) = fromSeries(Series.fromList(as))
    def fromSeries[A](as: Series[A]): Generator[A] = new Generator[A] {
      def next = inContext(as)
    }
    def apply[A](cs: Context[Series[A]]): Generator[A] = new Generator[A] {
      def next = cs
    }

    object Ops {

      type CS[+A] = Context[Series[A]]
      val empty = inContext(Series())

      def split[A, B](cs: CS[A])(f: (A, CS[A]) => CS[B]): CS[B] = {
        cs >>= {
          _ match {
            case NonEmpty(a, cs1) => f(a, cs1)
            case Empty => empty
          }
        }
      }

      def map[A, B](cs: CS[A])(f: A => B): CS[B] =
        cs map { _ map f }

      def flatMap[A, B](cs: CS[A])(f: A => CS[B]): CS[B] = {
        cs >>= {
          _ match {
            case NonEmpty(a, cs1) => concat(f(a), flatMap(cs1)(f))
            case Empty => empty
          }
        }
      }

      def filter[A](cs: CS[A])(f: A => Boolean): CS[A] = {
        cs >>= {
          _ match {
            case NonEmpty(a, cs1) =>
              val cs2 = filter(cs1)(f)
              if(f(a)) prepend(a, cs2) else cs2
            case Empty => empty
          }
        }
      }

      def prepend[A](a: A, cs: CS[A]): CS[A] =
        inContext(Series(a, cs))

      def concat[A](cs1: CS[A], cs2: CS[A]): CS[A] = {
        cs1 >>= { s1 => cs2 map { s2 => s1 append s2 }}
      }

      def fold[A, S](cs: CS[A])(z: S)(f: (S, A) => S): Context[S] = {
        cs >>= {
          _ match {
            case NonEmpty(a, cs1) => fold(cs1)(f(z, a))(f)
            case Empty => inContext(z)
          }
        }
      }

      def foldp[A, S](cs: CS[A])(z: S)(f: (S, A) => Context[S]): Context[S] = {
        cs >>= {
          _ match {
            case NonEmpty(a, cs1) => f(z, a) >>= (foldp(cs1)(_)(f))
            case Empty => inContext(z)
          }
        }
      }

      def reduce[A, S](cs: CS[A])(f: Reducer[A, S]): Context[S] = {
        def loop(g0: CS[A], s: f.State): Context[S] = {
          if(f.isReduced(s)) f.complete(s)
          else
            g0 >>= {
              _ match {
                case NonEmpty(a, cs1) => f(s, a) >>= (loop(cs1, _))
                case Empty => f.complete(s)
              }
            }
        }
        f.init >>= (loop(cs, _))
      }

      def sink[A](cs: CS[A]): (A => Context[Unit]) => Context[Unit] = {
        consume =>
          def loop(cs: CS[A]): Context[Unit] = {
            cs >>= {
              _ match {
                case NonEmpty(a, cs1) => consume(a) >>= (_ => loop(cs1))
                case Empty => inContext(())
              }
            }
          }
          loop(cs)
      }
    }
  }
}

trait SeriesDefs { this: Transducers with ContextIsFunctor =>

  def lazily[A](a: => A): Context[A]

  sealed trait Series[+A] extends Series.Ops[A]

  object Series {

    case class NonEmpty[+A](head: A, tail: Context[Series[A]]) extends Series[A]
    case object Empty extends Series[Nothing]

    trait Ops[+A] { this: Series[A] =>

      def map[B](f: A => B): Series[B] = this match {
        case NonEmpty(a, cs1) => NonEmpty(f(a), cs1 map (_ map f))
        case Empty => Empty
      }

      def extract: Option[A] = this match {
        case NonEmpty(a, _) => Some(a)
        case Empty => None
      }

      def extend[B](f: Series[A] => B): Series[B] =this match {
        case NonEmpty(a, cs1) => NonEmpty(f(this), cs1 map (_ extend f))
        case Empty => Empty
      }


      def +:[B >: A](b: B): Series[B] = NonEmpty(b, inContext(this))

      def append[B >: A](s: Series[B]): Series[B] = this match {
        case NonEmpty(a, cs1) => NonEmpty(a, cs1 map (_ append s))
        case Empty => s
      }
    }

    def apply(): Series[Nothing] = Empty
    def apply[A](a: A, cs: Context[Series[A]]): Series[A] = NonEmpty(a, cs)
    def singleton[A](a: A): Series[A] = NonEmpty(a, inContext(Empty))
    def fromOption[A](o: Option[A]) = o match {
      case Some(a) => singleton(a)
      case None => Empty
    }
    def fromList[A](as: List[A]): Series[A] = as match {
      case a :: as1 => NonEmpty(a, lazily(fromList(as1)))
      case Nil => Empty
    }
  }
}

trait EducerDefs { this: Transducers =>

  trait Educer[-S, +A] { base =>
    type State
    def init(s: S): Context[State]
    def isEduced(s: State): Boolean
    def extract(s: State): A
    def apply(s: State): Context[State]

    def next(s: State): Option[(A, Context[State])] =
      if(isEduced(s)) Some((extract(s), apply(s)))
      else None

    def map[B](f: A => B): Educer[S, B] = new Educer[S, B] {
      type State = base.State
      def init(s: S) = base.init(s)
      def isEduced(s: State) = base.isEduced(s)
      def extract(s: State) = f(base.extract(s))
      def apply(s: State) = base(s)
    }
  }

  def singletonEducer[A]: Educer[A, A] = new Educer[A, A] {
    type State = Option[A]
    def init(a: A) = inContext(Some(a))
    def isEduced(s: State) = ! s.isDefined
    def extract(s: State) = s.get
    def apply(s: State) = inContext(None)
  }

  def optionEducer[A]: Educer[Option[A], A] = new Educer[Option[A], A] {
    type State = Option[A]
    def init(as: Option[A]) = inContext(as)
    def isEduced(s: State) = ! s.isDefined
    def extract(s: State) = s.get
    def apply(s: State) = inContext(None)
  }

  def listEducer[A]: Educer[List[A], A] = new Educer[List[A], A] {
    type State = List[A]
    def init(as: List[A]) = inContext(as)
    def isEduced(s: State) = s.isEmpty
    def extract(s: State) = s.head
    def apply(s: State) = inContext(s.tail)
  }
}

trait SeriesEducerDefs { this: Generators with SeriesDefs with EducerDefs with Transducers =>

  import Series._

  abstract class SeriesEducer[S, A] extends Educer[S, A] {
    type State = Series[A]

    def isEduced(s: State) = s match {
      case NonEmpty(_, _) => false
      case Empty => true
    }

    def extract(s: State) = s match {
      case NonEmpty(a, _) => a
      case Empty => ???
    }

    def apply(s: State) = s match {
      case NonEmpty(_, t) => t
      case Empty => ???
    }
  }

  def seriesEducer[A] = new SeriesEducer[Series[A], A] {
    def init(as: Series[A]) = inContext(as)
  }

  def generatorEducer[A] = new SeriesEducer[Generator[A], A] {
    def init(as: Generator[A]) = as.next
  }
}

trait ComonadDefs { this: EducerDefs with Transducers =>
  import language.higherKinds

  trait Comonad[G[_]] {
    def extract[A](g: G[A]): A
    def map[A, B](g: G[A])(f: A => B): G[B]
    def extend[A, B](g: G[A])(f: G[A] => B): G[B]
  }

}

trait SimpleEduction { this: EducerDefs with Transducers with ContextIsId =>

  def transfer[S, T, A](s: S, g: Educer[S, A], f: Reducer[A, T]): T = {

    @annotation.tailrec
    def loop(sg: g.State, sf: f.State): T = {
      if(g.isEduced(sg) || f.isReduced(sf)) f.complete(sf)
      else loop(g(sg), f(sf, g.extract(sg)))
    }

    loop(g.init(s), f.init)
  }

  def chainLeft[S, A, B](g1: Educer[S, A], g2: Educer[A, B]): Educer[S, B]

  def chainRight[A, B, S](g: Educer[A, B], f: Reducer[B, S]): Reducer[A, S] = new Reducer[A, S] {
    type State = f.State
    def init = f.init
    def isReduced(s: State) = f.isReduced(s)
    def complete(s: State) = f.complete(s)
    def apply(s: State, a: A): State = {

      @annotation.tailrec
      def pump(s: f.State, t: g.State): f.State =
        if(g.isEduced(t) || f.isReduced(s)) s
        else pump(f(s, g.extract(t)), g(t))

      pump(s, g.init(a))
    }
  }

  def flip[A, B](g: Educer[A, B]): Transducer[B, A] = new Transducer[B, A] {
    def apply[S]( f: Reducer[B, S]): Reducer[A, S] = chainRight(g, f)
  }
}
