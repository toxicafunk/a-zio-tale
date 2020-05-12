package es.hybride

object MainTK {

  def toOption(i: Int, cond: Int => Boolean): Option[Int] = {
    val opt: Option[Int] = if (cond(i)) Some(i) else None
    opt
  }

  import TransM._
  def main(args: Array[String]): Unit = {

    /*val l1 = List(1,2,3,4,5)
    def calcErr(base: Int, threshold: Int): List[Option[Int]] = for {
      i   <- l1
      //opt <- toOption(i, (i => (i + base) <= threshold))
      opt <- Option(i)
    } yield  opt

    println(calcErr(6, 12))*/

    type ListOption[A] = OptionT[List, A]

    val lst1 = List(Option(1), Option(2), Option(3), Option(4), Option(5))
    val lst2 = List(Option(1), Option(2), Option(3), None, Option(5))
    val r1: ListOption[Int] = OptionT(lst1)
    val r2: ListOption[Int] = OptionT(lst2)
    val ra: ListOption[Int] = optionTransM.lift(List(6))
    //val ra: ListOption[Int] = TransM[OptionT].lift(List(6))

    println(r1)
    println(r2)

    val calc: ListOption[Int] => OptionT[List, Int] = (r: ListOption[Int]) =>
      for {
        l1 <- r
        l2 <- ra
      } yield l1 + l2

    println(calc(r1))
    println(calc(r2))

    val l = (1 to 19000).map(Option(_))
      //.map(toOption(_, (i => i % 2 == 0)))

    println(calc(OptionT(l.toList)).value.size)

    //type Kleisli[M[_], A, B] = A => M[B]
    import Kleisli._
    val calcT: Kleisli[Trampoline, ListOption[Int], OptionT[List, Int]] =
      kleisli(r => {
        println("Begin")
        for {
          x <- More(() => Done(r))
          y <- More(() => Done(ra))
          s <- Done(for {
            i  <- x
            j  <- y
            } yield i  + j)
        } yield s
      })
    println(calcT(OptionT(l.toList)).runT.value.size)
  }
}
