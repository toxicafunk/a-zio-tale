package es.hybride

import TransM._

object Main {

  def toOption(i: Int, cond: Int => Boolean): Option[Int] = {
    val opt: Option[Int] = if (cond(i)) Some(i) else None
    opt
  }

  implicit val trampolineMonad = new Monad[Trampoline] {
    override def pure[A](a: A): Trampoline[A] = More(() => Done(a))

    override def flatMap[A, B](fa: Trampoline[A])(f: A => Trampoline[B]): Trampoline[B] =
      fa.flatMap(f)

    override def map[A, B](fa: Trampoline[A])(f: A => B): Trampoline[B] =
      fa.map(f)
  }

  def main(args: Array[String]): Unit = {

    type ListTramp[A] = ListT[Trampoline, A]
    type ListOption[A] = OptionT[ListTramp, A]

    //val lst1: ListTramp[Int] = listTransM.lift(trampolineMonad.pure(1))
    val lst1: ListTramp[Int] = ListT(More(() => Done(List(1))))
    val lsta: ListTramp[Int] = ListT(Monad[Trampoline].pure(List(6)))

    val ra: ListOption[Int] = OptionT(lsta.map(i => Option(i)))
    val r1: ListOption[Int] = OptionT(lst1.map(i => Option(i)))

    val calc: ListOption[Int] => OptionT[ListTramp, Int] = (r: ListOption[Int]) =>
      for {
        l1 <- r
        l2 <- ra
      } yield l1 + l2

    println(calc(r1).value.value.runT.size)

    val l = ListT(Monad[Trampoline].pure((1 to 100000).toList))
    val r = OptionT(l.map(i => Option(i)))
    println(calc(r).value.value.runT.size)

    //type Kleisli[M[_], A, B] = A => M[B]
    import Kleisli._
    val calcT: Kleisli[Trampoline, List[Option[Int]], OptionT[List, Int]] =
      kleisli(r => {
        println("Begin")
        for {
          x <- Monad[Trampoline].pure(r)
          y <- Monad[Trampoline].pure(List(Option(6)))
          s <- Monad[Trampoline].pure{for {
            i  <- x
            j  <- y
            } yield i.flatMap(m => j.map(n => m + n))}
        } yield OptionT(s)
      })
    println(calcT((1 to 100000).toList.map(i => Option(i))).runT.value.size)
  }
}
