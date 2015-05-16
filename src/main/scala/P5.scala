import scala.annotation.tailrec

object P5 {

  implicit def listToList[A](list: List[A]) = new BetterList(list)

  class BetterList[A](list: List[A]) {

    def rev: List[A] = {

      @tailrec
      def rev0(list: List[A], acc: List[A]): List[A] = list match {
        case x :: xs => if (list.isEmpty) acc else rev0(xs, x :: acc)
        case _ => acc
      }

      rev0(list, Nil)
    }
  }

  def main(args: Array[String]) {
    println(List(1,7,5,2).rev == List(2,5,7,1))
  }

}
