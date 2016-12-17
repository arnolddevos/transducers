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

      def +:[B >: A](b: B): Series[B] = NonEmpty(b, inContext(this))

      def append[B >: A](s: Series[B]): Series[B] = this match {
        case NonEmpty(a, cs1) => NonEmpty(a, cs1 map (_ append s))
        case Empty => s
      }

      def map[B](f: A => B): Series[B] = this match {
        case NonEmpty(a, cs1) => NonEmpty(f(a), cs1 map (_ map f))
        case Empty => Empty
      }
    }

    def apply(): Series[Nothing] = Empty
    def apply[A](a: A, cs: Context[Series[A]]): Series[A] = NonEmpty(a, cs)
    def singleton[A](a: A): Series[A] = NonEmpty(a, inContext(Empty))
    def fromList[A](as: List[A]): Series[A] = as match {
      case a :: as1 => NonEmpty(a, lazily(fromList(as1)))
      case Nil => Empty
    }
  }
}
