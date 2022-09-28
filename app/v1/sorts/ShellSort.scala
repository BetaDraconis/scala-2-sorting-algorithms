package v1.sorts

import scala.annotation.tailrec

object ShellSort extends Sort {
  override protected[sorts] def sort(list: Seq[BigDecimal]): Seq[BigDecimal] = {

    @tailrec
    def generateGaps(gaps: Seq[Int] = Nil, i: Int = 1): Seq[Int] = {
      val nextGap = (2 * i) - 1
      if (list.drop(nextGap).nonEmpty) generateGaps(nextGap +: gaps, i + 1) else gaps
    }

    @tailrec
    def doSort(nums: Seq[BigDecimal], gaps: Seq[Int]): Seq[BigDecimal] = {

      @tailrec
      def sortWithGap(nums: Seq[BigDecimal], gap: Int, sublistIndex: Int = 0, currIndex: Int = 0, prevIndexes: Seq[Int] = Nil): Seq[BigDecimal] = {

        val nextSublistIndex: Int = currIndex + gap

        val isSortedWithGap: Boolean = !nums.isDefinedAt(sublistIndex + gap) || sublistIndex == gap
        val isEndOfSublist: Boolean = !nums.isDefinedAt(nextSublistIndex)

        (isSortedWithGap, isEndOfSublist) match {
          case (true, _) => nums
          case (_, true) => sortWithGap(nums, gap, sublistIndex + 1, sublistIndex + 1)
          case (_, _) =>
            val indexesToSwap = (prevIndexes :+ currIndex).dropWhile(i => nums(i) <= nums(nextSublistIndex))
            // this fold should probably be a recursive function instead to avoid returning a enum where only half of the data is actually used.
            val newNums = indexesToSwap.foldRight(nums, nextSublistIndex)((i, state) => (swap(state._1, i, state._2), i))._1
            sortWithGap(newNums, gap, sublistIndex, nextSublistIndex, prevIndexes :+ currIndex)
        }
      }

      gaps match {
        case Nil => nums
        case gap :: Nil => sortWithGap(nums, gap)
        case gap :: tail => doSort(sortWithGap(nums, gap), tail)
      }
    }

    doSort(list, generateGaps())
  }
}
