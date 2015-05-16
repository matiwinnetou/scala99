import scala.annotation.tailrec

object P3 {

  implicit def listToList[A](list: List[A]) = new BetterList(list)

  class BetterList[A](targetList: List[A]) {

    private def index(ind: Int) = Math.abs(ind - targetList.size)

    @tailrec
    private def nth(targetList: List[A], itemNo: Int = targetList.size - 1): A = targetList match {
      case _ :: _ if targetList.isEmpty => throw new NoSuchElementException
      case x :: _ if targetList.size == index(itemNo) => x
      case _ => nth(targetList.tail, itemNo)
    }

    def firstIndex = 0

    def lastIndex = targetList.size - 2

    def first: A = nth(targetList, targetList.firstIndex)

    def penultimate: A = nth(targetList, targetList.lastIndex)

    def lastElem: A = nth(targetList)

  }

  def main(args: Array[String]) {
    println(List(1,7,5,2).penultimate == 5)
    println(List(1,7,5,2).last == 2)
    println(List(1,7,5,2).first == 1)
  }

}
