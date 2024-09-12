package sorts.v1

import sorts.common.Sort
import Sort.swap

import scala.annotation.tailrec

object ExchangeSort extends Sort[BigDecimal] {
  override def sort(nums: Seq[BigDecimal]): Seq[BigDecimal] = {

    @tailrec
    def doSort(nums: Seq[BigDecimal], index: Int = 0): Seq[BigDecimal] = {

      @tailrec
      def doNextExchange(nums: Seq[BigDecimal], index: Int = 1): Seq[BigDecimal] = nums.drop(index) match {
        case Nil => nums
        case head :: _ if nums.head > head => doNextExchange(swap(nums, 0, index), index + 1)
        case _ => doNextExchange(nums, index + 1)
      }

      nums.drop(index) match {
        case Nil => nums
        case _ :: Nil => nums
        case numsToSort => doSort(nums.take(index) ++ doNextExchange(numsToSort), index + 1)
      }
    }

    doSort(nums)
  }
}
