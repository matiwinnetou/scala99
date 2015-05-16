import Utils.isEven
import Utils.isOdd

object P6 {

  implicit def listToList[A](list: List[A]) = new BetterList(list)

  class BetterList[A](list: List[A]) {

    def middleIndex: Option[Int] = if (list.size == 1) Some(0)
      else if (isOdd(list.size)) Some(list.size / 2) else None

    def isPalindrome: Boolean = if (list.isEmpty) false
      else if (list.size == 1) true
      else if (isEven(list.size)) false
      else middleIndex.fold(false)(ind => list.splitAt(ind) match { case (leftList, rightList) => leftList == rightList.tail.reverse } )
  }

  def main(args: Array[String]) {
    val l = List(1, 2, 3, 2, 1)
    println(l.middleIndex)
    println(l.isPalindrome == true)
    println(List(1).isPalindrome == true)
    println(List().isPalindrome == false)
    println(List(1,2,3,4).isPalindrome == false)
    println(List(1,2,3,4).isPalindrome == false)
  }

}
