import scala.annotation.tailrec

object P4 {

  implicit def listToList[A](list: List[A]) = new BetterList(list)

  class BetterList[A](list: List[A]) {

    def mySize: Int = {

      @tailrec
      def mySize0(list: List[A], acc: Int): Int = list match {
        case x :: xs => if (list.isEmpty) acc else mySize0(xs, acc + 1)
        case _ => acc
      }

      mySize0(list, 0)
    }

  }

  def main(args: Array[String]) {
    println(List(1,7,5,2).mySize == 4)
  }

}
