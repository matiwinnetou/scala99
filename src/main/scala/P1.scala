import scala.annotation.tailrec

//http://aperiodic.net/phil/scala/s-99/
object P1 {

  implicit def listToList[A](list: List[A]) = new BetterList(list)

  class BetterList[A](targetList: List[A]) {

    @tailrec
    private def elemFromEndAtIndex(targetList: List[A], itemNo: Int = 1): A = targetList match {
      case x :: _ if targetList.isEmpty => throw new NoSuchElementException
      case x :: _ if targetList.size == itemNo => x
      case _ => elemFromEndAtIndex(targetList.tail, itemNo)
    }

    def penultimate: A = elemFromEndAtIndex(targetList, 2)

    def lastElem: A = elemFromEndAtIndex(targetList)

  }


  def main(args: Array[String]) {
    println(List(1,7,5,2).last == List(1,7,5,2).lastElem)
  }

}
