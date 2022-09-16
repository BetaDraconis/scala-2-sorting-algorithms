/*
 * Copyright 2022 Luke A Jones
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

object HeapSort extends Sort[BigDecimal] {
  def sort(list: Seq[BigDecimal]): Seq[BigDecimal] = {

    @tailrec
    def siftDown(nums: Seq[BigDecimal], index: Int): Seq[BigDecimal] = {
      val parentNum: BigDecimal = nums(index)

      val leftChildIndex : Int = (2 * index) + 1
      val rightChildIndex : Int = (2 * index) + 2

      (nums.isDefinedAt(leftChildIndex), nums.isDefinedAt(rightChildIndex)) match {
        case (false, _) => nums
        case (true, false) =>
          if (nums(leftChildIndex) > parentNum) swap(nums, index, leftChildIndex) else nums
        case (true, _) =>
          val leftChildNum: BigDecimal = nums(leftChildIndex)
          val rightChildNum: BigDecimal = nums(rightChildIndex)

          if (leftChildNum > parentNum && leftChildNum >= rightChildNum) siftDown(swap(nums, index, leftChildIndex), leftChildIndex)
          else if (rightChildNum > parentNum && rightChildNum >= leftChildNum) siftDown(swap(nums, index, rightChildIndex), rightChildIndex)
          else nums
      }
    }

    @tailrec
    def heapify(nums: Seq[BigDecimal], index: Int): Seq[BigDecimal] = {
      if (index == -1) nums
      else {
        heapify(siftDown(nums, index), index - 1)
      }
    }

    def reHeapify(nums: Seq[BigDecimal]): Seq[BigDecimal] =
      siftDown(nums = nums.takeRight(1) ++ nums.dropRight(1), index = 0)

    @tailrec
    def doSort(heapedNums: Seq[BigDecimal], sortedNums: Seq[BigDecimal] = Nil): Seq[BigDecimal] = heapedNums match {
      case Nil => sortedNums
      case num :: Nil => num +: sortedNums
      case _ => doSort(reHeapify(heapedNums.drop(1)), heapedNums.head +: sortedNums)
    }

    val startNodeIndex = math.floor((list.length - 1) / 2).toInt
    val heapedNums: Seq[BigDecimal] = heapify(list, startNodeIndex)
    doSort(heapedNums)
  }
}
