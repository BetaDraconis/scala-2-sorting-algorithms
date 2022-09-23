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
      def sortWithGap(nums: Seq[BigDecimal], gap: Int, startingIndex: Int = 0): Seq[BigDecimal] = {
        val nextIndex = startingIndex + gap

        @tailrec
        def sortInSubList(nums: Seq[BigDecimal],
                          lastValidIndex: Int = nextIndex,
                          subListIndexes: Seq[Int] = Seq(startingIndex, nextIndex)): Seq[BigDecimal] = {

          val nextSubListIndex = lastValidIndex + gap

          @tailrec
          def placeNumInSubList(numToPlace: BigDecimal,
                                indexes: Seq[Int] = subListIndexes,
                                nums: Seq[BigDecimal] = nums): Seq[BigDecimal] = indexes match {
            case Nil => nums
            case _ :+ last if numToPlace >= nums(last) => nums
            case allButLast :+ last => placeNumInSubList(numToPlace, allButLast, swap(nums, last, nextSubListIndex))
          }

          nums.drop(nextSubListIndex) match {
            case Nil => nums
            case numToPlace :: Nil => placeNumInSubList(numToPlace)
            case numToPlace :: _ => sortInSubList(placeNumInSubList(numToPlace), nextSubListIndex, subListIndexes :+ nextSubListIndex)
          }
        }

        val maxRequiredIndex = gap - 1

        (nums.drop(nextIndex), startingIndex) match {
          case (Nil, _) => nums
          case (_ :: Nil, _) => sortInSubList(nums)
          case (_, `maxRequiredIndex`) => sortInSubList(nums)
          case _ => sortWithGap(sortInSubList(nums), gap, startingIndex + 1)
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
