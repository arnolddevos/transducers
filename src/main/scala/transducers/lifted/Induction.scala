
package transducers

trait Induction { this: Transducers with ContextIsMonad =>

  type Producer[+A] = Context[Series[A]]

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
      case a :: as1 => NonEmpty(a, lazyContext(fromList(as1)))
      case Nil => Empty
    }
  }
}
