/*
 * Copyright 2020 Luke A Jones
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package v1.sorts

import scala.annotation.tailrec

object HeapSort extends Sort {
  def sort(list: Seq[BigDecimal]): Seq[BigDecimal] = {
    @tailrec
    def doSort(unsortedNums: Seq[BigDecimal], sortedNums: Seq[BigDecimal] = Nil): Seq[BigDecimal] = unsortedNums match {
      case Nil => sortedNums
      case num :: Nil => num +: sortedNums
      case _ =>
        val heapedUnsortedNums: Seq[BigDecimal] = heapify(unsortedNums)
        doSort(heapedUnsortedNums.head +: sortedNums, reHeapify(heapedUnsortedNums.tail))
    }

    def heapify(nums: Seq[BigDecimal], index: Int = 0): Seq[BigDecimal] = {
      val parentNode: BigDecimal = nums(index)
      val leftChildNodeExists: Boolean = nums.isDefinedAt((2*index) + 1)
      val rightChildNodeExists: Boolean = nums.isDefinedAt((2*index) + 2)

      (leftChildNodeExists, rightChildNodeExists) match {
        case (false, false) => nums
        case (true, true) => nums

      }
      nums
    }


    def reHeapify(nums: Seq[BigDecimal]): Seq[BigDecimal] = nums

    doSort(list)
  }
}
