package v1.sorts

import scala.annotation.tailrec

object RadixSort extends Sort[Integer]{
  override protected[sorts] def sort(list: Seq[Integer]): Seq[Integer] = {
    val maxSf = list.max.toString.toCharArray.length

    @tailrec
    def doSort(nums: Seq[Integer], sf: Int = 0): Seq[Integer] = {
      if (sf == maxSf) {
        nums
      } else {
        val aa = List.fill(10)(List.empty[Integer])
        val newNums = nums.foldLeft(aa)((state, nextNum) => {
          val explodedNum = nextNum.toString.toCharArray
          val sigFigValue = explodedNum.dropRight(sf).lastOption.map(_.toString.toInt).getOrElse(0)
          state.updated(sigFigValue, state(sigFigValue) :+ nextNum)
        }).flatten

        doSort(newNums, sf + 1)
      }
    }

    doSort(list)
  }
}
