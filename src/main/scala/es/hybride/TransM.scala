package es.hybride

import annotation.tailrec

trait Monad[F[_]] {

  def pure[A](a: A): F[A]
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  def map[A, B](fa: F[A])(f: A => B): F[B] = flatMap(fa)(a => pure(f(a)))
}

object Monad {

  def apply[F[_]](implicit m: Monad[F]): Monad[F] = m

  def flatMap[F[_]: Monad, A, B](fa: F[A])(f: A => F[B]) =
    Monad[F].flatMap[A, B](fa)(f)

  def pure[F[_]: Monad, A](a: A): F[A] = Monad[F].pure[A](a)

  implicit class MonadOps[F[_]: Monad, A](fa: F[A]) {
    def flatMap[B](f: A => F[B]) = Monad[F].flatMap[A, B](fa)(f)
  }

  implicit val optionMonad: Monad[Option] = new Monad[Option] {
    def pure[A](a: A): Option[A] = Option(a)

    def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa match {
      case None    => None
      case Some(a) => f(a)
    }
  }

  implicit val listMonad: Monad[List] = new Monad[List] {

    override def pure[A](a: A): List[A] = List(a)

    override def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] =
      fa match {
        case Nil     => List.empty[B]
        case x :: xs => f(x) ++ flatMap(xs)(f)
      }
  }
}

case class OptionT[F[_]: Monad, A](value: F[Option[A]]) {

  // Applicative is preferred
  def apply(a: A): OptionT[F, A] =
    OptionT(Monad[F].pure(Some(a)))

  def pure(a: A): OptionT[F, A] = apply(a)

  def flatMap[B](f: A => OptionT[F, B]): OptionT[F, B] =
    OptionT(
      Monad[F]
        .flatMap(value)(_.fold(Monad[F].pure[Option[B]](None))(a => f(a).value))
    )

  def map[B](f: A => B): OptionT[F, B] =
    OptionT(Monad[F].map(value)(_.map(f)))
}

object OptionT {
   implicit def optionTMonad[F[_]: Monad, A] = new Monad[({type T[A] = OptionT[F, A]})#T] {
    def pure[A](a: A): OptionT[F, A] = OptionT(Monad[F].pure(Some(a)))

     def flatMap[A, B](fa: OptionT[F, A])(f: A => OptionT[F, B]): OptionT[F, B] =
       fa.flatMap(f)

     override def map[A, B](fa: OptionT[F, A])(f: A => B): OptionT[F, B] =
       fa.map(f)
  }
}

case class ListT[F[_]: Monad, A](value: F[List[A]]) {

  // Applicative is preferred
  def apply(a: A): ListT[F, A] =
    ListT(Monad[F].pure(List(a)))

  def pure(a: A): ListT[F, A] = apply(a)

  def flatMap[B](f: A => ListT[F, B]): ListT[F, B] =
    ListT(
      Monad[F].flatMap(value) {
        _ match {
          case Nil => Monad[F].pure(List.empty[B])
          case lst => lst.map(f).reduce(_ ++ _).value
        }
      }
    )

  def map[B](f: A => B): ListT[F, B] =
    ListT(Monad[F].map(value)(_.map(f)))

  def ++(that: ListT[F, A]) = ListT(
    Monad[F].flatMap(value) { lst => Monad[F].map(that.value)(lst ++ _) }
  )
}

object ListT {
  implicit def listTMonad[F[_]: Monad, A] = new Monad[({type T[A] = ListT[F, A]})#T] {
    override def pure[A](a: A): ListT[F, A] = ListT(Monad[F].pure(List(a)))

    override def flatMap[A, B](fa: ListT[F,A])(f: A => ListT[F,B]): ListT[F,B] =
      fa.flatMap(f)

    override def map[A, B](fa: ListT[F,A])(f: A => B): ListT[F,B] =
      fa.map(f)
  }
}

trait TransM[F[_[_], _]] {
  def lift[G[_]: Monad, A](a: G[A]): F[G, A]
}

object TransM {
  implicit val optionTransM: TransM[OptionT] = new TransM[OptionT] {
    // Functor is peferred
    override def lift[G[_], A](ga: G[A])(implicit G: Monad[G]): OptionT[G, A] =
      OptionT(G.map(ga)(Some(_)))
  }

  implicit val listTransM: TransM[ListT] = new TransM[ListT] {
    // Functor is peferred
    override def lift[G[_], A](ga: G[A])(implicit G: Monad[G]): ListT[G, A] =
      ListT(G.map(ga)(List(_)))
  }
}

trait Trampoline[+A] {
  /*final def runT: A =
    this match {
      case More(k) => k().runT
      case Done(v) => v
    }*/

  // Not in tail position!!!
  /*def flatMap[B](f: A => Trampoline[B]): Trampoline[B] =
    More[B](() => f(runT))*/

  // run the step
  /*final def resume: Either[() => Trampoline[A], A] = this match {
    case Done(v)        => Right(v)
    case More(k)        => Left(k)
    case FlatMap(a, f)  => a match {
      case Done(v)        => f(v).resume
      case More(k)        => Left(() => FlatMap(k(), f))
      case FlatMap(b, g)  => (FlatMap(b, (x: Any) => FlatMap(g(x), f)): Trampoline[A]).resume
    }
   }*/

  // disallow the construction of deeply nested left-associated binds
  @tailrec
  final def resume: Either[() => Trampoline[A], A] = this match {
    case Done(v) => Right(v)
    case More(k) => Left(k)
    case FlatMap(a, f) =>
      a match {
        case Done(v)       => f(v).resume
        case More(k)       => Left(() => k() flatMap f)
        case FlatMap(b, g) => b.flatMap((x: Any) => g(x) flatMap f).resume
      }
  }

  // List(1,2,3).flatMap(i => List(i * 2))
  // FlatMap(FlatMap(b,g), f)
  // FlatMap(b, x => FlatMap(g(x), f))

  // should advance to next step?
  final def runT: A =
    resume match {
      case Right(a) => a
      case Left(k)  => k().runT
    }

  //@tailrec
  final def flatMap[B](f: A => Trampoline[B]): Trampoline[B] =
    /*this match {
      case FlatMap(a, g) => FlatMap(a, (x: Any) => g(x) flatMap f)
      case x             => FlatMap(x, f)
     }*/
    FlatMap(this, f)

  final def map[B](f: A => B): Trampoline[B] =
    flatMap(a => Done(f(a)))

}

case class More[+A](k: () => Trampoline[A]) extends Trampoline[A]
case class Done[+A](result: A) extends Trampoline[A]
private case class FlatMap[A, +B](sub: Trampoline[A], k: A => Trampoline[B])
    extends Trampoline[B]

case class Kleisli[M[_], A, B](run: A => M[B]) {
  def apply(a: A) = run(a)
}
object Kleisli {
  def kleisli[M[_], A, B](f: A => M[B]): Kleisli[M, A, B] =
    Kleisli(f)
}
