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
        doSort(reHeapify(heapedUnsortedNums.tail), heapedUnsortedNums.head +: sortedNums)
    }

    def heapify(nums: Seq[BigDecimal], index: Int = 0): Seq[BigDecimal] = {
      val parentNode: BigDecimal = nums(index)

      val leftChildIndex: Int = (2 * index) + 1
      val rightChildIndex: Int = (2 * index) + 2
      val leftChildNodeExists: Boolean = nums.isDefinedAt(leftChildIndex)
      val rightChildNodeExists: Boolean = nums.isDefinedAt(rightChildIndex)

      def sortParentWithChildAtIndex(childIndex: Int): Seq[BigDecimal] =
        if (nums(childIndex) > parentNode) swap(nums, index, childIndex) else nums

      (leftChildNodeExists, rightChildNodeExists) match {
        case (false, false) => nums // the bottom of the heap
        case (true, false) => sortParentWithChildAtIndex(leftChildIndex)
        case (false, true) => sortParentWithChildAtIndex(rightChildIndex)
        case (true, true) =>
          val a = heapify(nums, leftChildIndex)
          val b = heapify(nums, rightChildIndex)
          val c = nums.zipWithIndex

          val difs = c.diff(a) ++ c.diff(b) // could be improved by checking only diffs in child heap indexes

          val updatedNums: Seq[BigDecimal] = c.map{ (aa) =>
            val as = difs.indexWhere(_._2 == aa._2)
            if (as == -1) aa._1 else difs(as)._1
          }

          val newLeftChild = updatedNums(leftChildIndex)
          val newRightChild = updatedNums(rightChildIndex)

          if (newLeftChild > parentNode && newLeftChild > newRightChild) swap(updatedNums, index, leftChildIndex)
          else if (newRightChild > parentNode && newRightChild > newLeftChild) swap(updatedNums, index, rightChildIndex)
          else updatedNums
      }
    }

    def reHeapify(nums: Seq[BigDecimal]): Seq[BigDecimal] = {
      heapify(nums.takeRight(1) ++ nums.dropRight(1))
      // you only actually have to re-heap the side of the heap with the highest top-level value (index = 1 or 2)
    }

    doSort(list)
  }
}
