
package transducers

trait Induction { this: Transducers with ContextIsMonad =>

  type Producer[+A] = Context[Series[A]]

  object Producer {
    import Series._

    def apply(): Producer[Nothing] = inContext(Series())
    def apply[A](a: A): Producer[A] = inContext(Series(a))
    def apply[A](a: A, g: Producer[A]): Producer[A] = inContext(Series(a, g))
    def fromList[A](as: List[A]): Producer[A] = inContext(Series.fromList(as))

    def split[A, B](g: Producer[A])(f: (A, Producer[A]) => Producer[B]): Producer[B] = {
      g >>= {
        _ match {
          case NonEmpty(a, g1) => f(a, g1)
          case Empty => Producer()
        }
      }
    }

    def map[A, B](g: Producer[A])(f: A => B): Producer[B] = {
      g map {
        _ match {
          case NonEmpty(a, g1) => NonEmpty(f(a), map(g1)(f))
          case Empty => Empty
        }
      }
    }

    def flatMap[A, B](g: Producer[A])(f: A => Producer[B]): Producer[B] = {
      g >>= {
        _ match {
          case NonEmpty(a, g1) => concat(f(a), flatMap(g1)(f))
          case Empty => Producer()
        }
      }
    }

    def filter[A](g: Producer[A])(f: A => Boolean): Producer[A] = {
      g >>= {
        _ match {
          case NonEmpty(a, g1) =>
            val g2 = filter(g1)(f)
            if(f(a)) Producer(a, g2) else g2
          case Empty => Producer()
        }
      }
    }

    def concat[A](ga: Producer[A], gb: Producer[A]): Producer[A] = {
      ga >>= {
        _ match {
          case NonEmpty(a, ga1) => Producer(a, concat(ga1, gb))
          case Empty => gb
        }
      }
    }
  }

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
    def apply[A](a: A): Series[A] = NonEmpty(a, inContext(Empty))
    def fromOption[A](o: Option[A]) = o match {
      case Some(a) => apply(a)
      case None => apply()
    }
    def fromList[A](as: List[A]): Series[A] = as match {
      case a :: as1 => NonEmpty(a, lazyContext(fromList(as1)))
      case Nil => Empty
    }
  }
}
