package v1.sorts

import scala.annotation.tailrec

object SelectionSort extends Sort {
  override protected[sorts] def sort(list: Seq[BigDecimal]): Seq[BigDecimal] = {

    //TODO: Compare performance of this method vs fold implementation
    @tailrec
    def findLowest(currLowestNums: Seq[BigDecimal], uncheckedNums: Seq[BigDecimal], checkedNums: Seq[BigDecimal] = Nil): (Seq[BigDecimal], Seq[BigDecimal]) = {
      val currLowestNum: BigDecimal = currLowestNums.head

      def checkNum(num: BigDecimal): (Seq[BigDecimal], Seq[BigDecimal]) =
        if (num > currLowestNum) (currLowestNums, checkedNums :+ num)
        else if (num == currLowestNum) (currLowestNums :+ num, checkedNums)
        else (Seq(num), currLowestNums ++ checkedNums)

      uncheckedNums match {
        case Nil => (currLowestNums, checkedNums)
        case head :: Nil => checkNum(head)
        case head :: tail =>
          val (newLowestNums, newCheckedNums) = checkNum(head)
          findLowest(newLowestNums, tail, newCheckedNums)
      }
    }

    @tailrec
    def doSort(unsortedNums: Seq[BigDecimal], sortedNums: Seq[BigDecimal] = Nil): Seq[BigDecimal] = unsortedNums match {
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

    doSort(list)
  }
}
