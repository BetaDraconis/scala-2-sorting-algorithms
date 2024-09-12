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
package sorts.v1

import sorts.common.Sort
import Sort.swap

import scala.annotation.tailrec

object LomutoQuickSort extends Sort[BigDecimal] {
  protected[sorts] def sort(nums: Seq[BigDecimal]): Seq[BigDecimal] = doSort(nums)

  // Quick sort using Lomuto partition
  private def doSort(nums: Seq[BigDecimal]): Seq[BigDecimal] = nums match {
    case Nil => Nil
    case Nil :+ _ => nums
    case _ =>
      val pivot: BigDecimal = nums.last
      val split: (Seq[BigDecimal], Seq[BigDecimal]) = nums.dropRight(1).partition(x => x <= pivot)
      (doSort(split._1) :+ pivot) ++ doSort(split._2)
  }
}

object HoareQuickSort extends Sort[BigDecimal] {
  protected[sorts] def sort(nums: Seq[BigDecimal]): Seq[BigDecimal] = doSort(nums, nums.length)

  // Hoare partition
  private def doSort(nums: Seq[BigDecimal], numsLength: Int): Seq[BigDecimal] = nums match {
    case Nil => Nil
    case Nil :+ _ => nums
    case _ =>

      @tailrec
      def partition(nums: Seq[BigDecimal], low: Int = 0, high: Int = numsLength - 1): (Seq[BigDecimal], Int) = {
        val initialPivot: BigDecimal = nums.last
        val i = nums.indexWhere(x => x >= initialPivot, low)
        val j = nums.lastIndexWhere(x => x <= initialPivot, high)

        if (i >= j) (nums, j) else partition(swap(nums, i, j), i + 1, j)
      }

      val (newNums, pivotIndex) = partition(nums)
      doSort(newNums.take(pivotIndex), numsLength) :+ newNums(pivotIndex)
  }
}
