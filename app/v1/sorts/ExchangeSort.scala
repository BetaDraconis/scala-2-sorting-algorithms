package v1.sorts

import scala.annotation.tailrec

object ExchangeSort extends Sort {
  override def sort(list: Seq[BigDecimal]): Seq[BigDecimal] = {

    @tailrec
    def doSort(nums: Seq[BigDecimal], index: Int = 0): Seq[BigDecimal] = {

      @tailrec
      def sortValueAtIndex(nums: Seq[BigDecimal], index: Int = 1): Seq[BigDecimal] = nums.drop(index) match {
          case Nil => nums
          case item :: Nil => if (nums.head > item) swap(nums, index1 = 0, index2 = index) else nums
          case item :: _ => if (nums.head > item) sortValueAtIndex(swap(nums, index1 = 0, index2 = index), index + 1) else sortValueAtIndex(nums, index + 1)
        }

      nums.drop(index) match {
        case Nil => nums
        case _ :: Nil => nums
        case numsToSort => doSort(numsToSort.take(index) ++ sortValueAtIndex(numsToSort), index + 1)
      }
    }

    doSort(list)
  }
}
