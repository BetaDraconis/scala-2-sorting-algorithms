package sorts.v1

import sorts.common.Sort
import Sort.swap

import scala.annotation.tailrec

object ShellSort extends Sort[BigDecimal] {
  protected[sorts] def sort(nums: Seq[BigDecimal]): Seq[BigDecimal] = {

    @tailrec
    def generateGaps(gaps: Seq[Int] = Nil, i: Int = 1): Seq[Int] = {
      val nextGap = (2 * i) - 1
      if (nums.drop(nextGap).nonEmpty) generateGaps(nextGap +: gaps, i + 1) else gaps
    }

    @tailrec
    def doSort(nums: Seq[BigDecimal], gaps: Seq[Int]): Seq[BigDecimal] = {

      @tailrec
      def placeItem(nums: Seq[BigDecimal], indexToPlace: Int, indexesToSwapWith: Seq[Int]): Seq[BigDecimal] = indexesToSwapWith match {
        case Nil => nums
        case Nil :+ last => swap(nums, last, indexToPlace)
        case rest :+ last => placeItem(swap(nums, last, indexToPlace), last, rest)
      }

      @tailrec
      def sortWithGap(nums: Seq[BigDecimal], gap: Int, sublistIndex: Int = 0, currIndex: Int = 0, prevIndexes: Seq[Int] = Nil): Seq[BigDecimal] = {
        val nextSublistIndex: Int = currIndex + gap

        val isSortedWithGap: Boolean = nums.drop(sublistIndex + gap).isEmpty || sublistIndex == gap
        val isEndOfSublist: Boolean = nums.drop(nextSublistIndex).isEmpty

        (isSortedWithGap, isEndOfSublist) match {
          case (true, _) => nums
          case (_, true) => sortWithGap(nums, gap, sublistIndex + 1, sublistIndex + 1)
          case (_, _) =>
            val indexesToSwap = (prevIndexes :+ currIndex).dropWhile(i => nums(i) <= nums(nextSublistIndex))
            val newNums = placeItem(nums, nextSublistIndex, indexesToSwap)
            sortWithGap(newNums, gap, sublistIndex, nextSublistIndex, prevIndexes :+ currIndex)
        }
      }

      gaps match {
        case Nil => nums
        case gap :: Nil => sortWithGap(nums, gap)
        case gap :: tail => doSort(sortWithGap(nums, gap), tail)
      }
    }

    doSort(nums, generateGaps())
  }
}
