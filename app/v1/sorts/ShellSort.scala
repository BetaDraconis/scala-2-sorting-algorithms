package v1.sorts

import scala.annotation.tailrec

object ShellSort extends Sort {
  override protected[sorts] def sort(list: Seq[BigDecimal]): Seq[BigDecimal] = {

    @tailrec
    def generateGaps(gaps: Seq[Int] = Nil, i: Int = 1): Seq[Int] = {
      val nextGap = (2 * i) - 1
      if (list.drop(nextGap).nonEmpty) generateGaps(gaps :+ nextGap, i + 1) else gaps
    }

    @tailrec
    def sortWithGap(nums: Seq[BigDecimal], gap: Int, index: Int = 0): Seq[BigDecimal] = {
      def doCheck(): Seq[BigDecimal] = ??? // (index, index + gap, index + 2gap ....)

      nums.drop(gap + index) match {
        case Nil => nums
        case _ :: Nil => doCheck()
        case _ => sortWithGap(doCheck(), gap, index + 1)
      }
    }

    @tailrec
    def doSort(nums: Seq[BigDecimal], gaps: Seq[Int]): Seq[BigDecimal] = gaps match {
      case Nil => nums
      case Nil :+ reverseHead => sortWithGap(nums, reverseHead)
      case reverseTail :+ reverseHead => doSort(sortWithGap(nums, reverseHead), reverseTail)
    }

    doSort(list, generateGaps())
  }
}
