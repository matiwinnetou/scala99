import scala.annotation.tailrec

object P7 {

  implicit def listToList[A](list: List[List[A]]) = new BetterList(list)

  class BetterList[A](list: List[List[A]]) {

    def flat: List[A] = if (list.isEmpty) Nil else {

      @tailrec
      def flatten0(l: List[List[A]], acc: List[A]): List[A] = l match {
        case x :: xs => if (l.isEmpty) acc else flatten0(xs, acc ::: x)
        case _ => acc
      }

      flatten0(list, Nil)
    }
  }

  def main(args: Array[String]) {
    val l = List(List(1), List(4,5), List(), List(9,1))
    val newList: List[Int] = l.flat
    println(newList)
    println(newList == List(1,4,5,9,1))
  }

}
