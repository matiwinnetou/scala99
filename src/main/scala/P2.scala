import scala.annotation.tailrec

object P2 {

  implicit def listToList[A](list: List[A]) = new BetterList(list)

  class BetterList[A](targetList: List[A]) {

    @tailrec
    private def nth(targetList: List[A], itemNo: Int = 1): A = targetList match {
      case _ :: _ if targetList.isEmpty => throw new NoSuchElementException
      case x :: _ if targetList.size == itemNo => x
      case _ => nth(targetList.tail, itemNo)
    }

    def penultimate: A = nth(targetList, 2)

    def lastElem: A = nth(targetList)

  }

  def main(args: Array[String]) {
    println(List(1,7,5,2).penultimate == 5)
  }

}
