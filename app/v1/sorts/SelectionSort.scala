package v1.sorts

import scala.annotation.tailrec

object SelectionSort extends Sort {
  override protected[sorts] def sort(list: Seq[BigDecimal]): Seq[BigDecimal] = doSort(list)

  @tailrec
  def doSort(unsortedNums: Seq[BigDecimal], sortedNums: Seq[BigDecimal] = Nil): Seq[BigDecimal] = {

    unsortedNums match {
      case Nil => sortedNums
      case num :: Nil => sortedNums :+ num
      case head :: tail =>
        val (lowestNums, remainingNums) = tail.foldLeft((Seq(head), Seq.empty[BigDecimal]))((currState, nextNum) => {

          val (currLowestNums, checkedNums) = currState
          val currLowestNum: BigDecimal = currLowestNums.head

          if (nextNum > currLowestNum) (currLowestNums, checkedNums :+ nextNum)
          else if (nextNum == currLowestNum) (currLowestNums :+ nextNum, checkedNums)
          else (Seq(nextNum), currLowestNums ++ checkedNums)
        })

        doSort(remainingNums, sortedNums ++ lowestNums)
    }
  }
}
