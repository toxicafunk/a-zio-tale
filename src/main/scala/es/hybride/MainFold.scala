package es.hybride

object MainFold {

  def toOption(i: Int, cond: Int => Boolean): Option[Int] = {
    val opt: Option[Int] = if (cond(i)) Some(i) else None
    opt
  }

  import TransM._
  def main(args: Array[String]): Unit = {

    type ListOption[A] = OptionT[List, A]

    val l = (1 to 100000)
      .map(toOption(_, (i => i % 2 == 0)))

    val t1 = l.foldLeft(OptionT(List(Option(0))))((acc, c) => {
      val lst = acc.value
      val j = c.fold(0)(_ + lst.head.getOrElse(0))
      OptionT(Option(j) +: lst)
    })
    println(t1.value.filter(_ != Some(0)).length)

    //type Kleisli[M[_], A, B] = A => M[B]
    import Kleisli._
  
    val t2 = l
      .foldLeft[Kleisli[Trampoline, ListOption[Int], ListOption[Int]]](
        kleisli(i => Done(i))
      )((a, e) =>
        kleisli(r =>
          for {
            x <- More(() => a.apply(r))
            y <- Done(r.flatMap(i => OptionT(Option(e.fold(0)(_ + i)) +: x.value)))
          } yield y
        )
      )

    println(t2(OptionT(List(Option(0)))).runT.value.filter(_ != Some(0)).length)
    //println(t2(optionTransM.lift(List(0))).runT)
  }
}
