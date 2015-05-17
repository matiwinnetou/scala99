import scala.annotation.tailrec

object P8 {

  implicit def listToList[A](list: List[A]) = new BetterList(list)

  class BetterList[A](list: List[A]) {

    def dedup: List[A] = if (list.isEmpty) Nil else {

      @tailrec
      def dedup0(list: List[A], acc: List[A]): List[A] = list match {
        case x :: xs => if (list.isEmpty) acc else if (acc.contains(x)) dedup0(xs, acc) else dedup0(xs, x :: acc)
        case _ => acc
      }

      dedup0(list, Nil).reverse
    }
  }

  def main(args: Array[String]) {
    val l = List(1,4,5,9,1)
    val newList: List[Int] = l.dedup
    println(newList)
    println(newList == List(1,4,5,9))
  }

}
