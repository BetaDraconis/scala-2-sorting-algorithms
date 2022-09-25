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

      //@tailrec
      def sortWithGap(nums: Seq[BigDecimal], gap: Int, startingIndex: Int = 0, currIndex: Int = 0, prevIndexes: Seq[Int] = Nil): Seq[BigDecimal] = {
        /*
          I want to do this in one step (rather than multiple nested recursive functions) following the logic:
            - for each new item in a sublist you can immediately place it rather than storing the index and then
              sorting it later.
            - once you hit the end of a sublist you then move onto the next subList at startingIndex += 1 until either
              no more sublists are possible, or you have created #gap sublists
            - you never need more than #gap sublists. This is because each sublist will contain 1/gap total possible
              items, i.e for a gap of two the first sublist will contain all even numbers, and the second all odd numbers.
         */

        val isSortedWithGap: Boolean =
          startingIndex == gap ||
          (startingIndex == currIndex && !nums.isDefinedAt(startingIndex + gap))

        if (isSortedWithGap) {
          nums
        } else {
          /*
            for each starting index increase currIndex by gap until you hit the end of the subList and then increment
            starting index by 1 until the above Boolean condition indicates that the sort is complete for a gap
           */
          nums
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
