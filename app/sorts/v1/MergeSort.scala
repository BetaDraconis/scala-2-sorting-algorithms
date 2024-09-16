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

import scala.annotation.tailrec

object MergeSort extends SortV1[BigDecimal] {
  protected[sorts] def sort(nums: Seq[BigDecimal]): Seq[BigDecimal] = doSort(nums)

  def doSort(nums: Seq[BigDecimal]): Seq[BigDecimal] = {
    @tailrec
    def split(nums: Seq[BigDecimal],
              leftNums: Option[Seq[BigDecimal]] = None,
              rightNums: Option[Seq[BigDecimal]] = None,
              middleNums: Option[BigDecimal] = None): Seq[Seq[BigDecimal]] =
      nums match {
      case Nil => Seq(leftNums, rightNums).flatten
      case num :: Nil => Seq(leftNums, Some(Seq(num)), rightNums).flatten
      case _ =>
        val newNums: Seq[BigDecimal] = nums.drop(1).dropRight(1)
        val nextLeftNum: Seq[BigDecimal] = nums.take(1)
        val nextRightNum: Seq[BigDecimal] = nums.takeRight(1)
        val newLeftNums: Seq[BigDecimal] = leftNums.fold(nextLeftNum)(nums => nums ++ nextLeftNum)
        val newRightNums: Seq[BigDecimal] = rightNums.fold(nextRightNum)(nums => nextRightNum ++ nums)
        split(newNums, Some(newLeftNums), Some(newRightNums), None)
    }

    @tailrec
    def combine(leftNums: Seq[BigDecimal], rightNums: Seq[BigDecimal], sortedNums: Seq[BigDecimal] = Nil): Seq[BigDecimal] = {
      (leftNums, rightNums) match {
        case (Nil, Nil) => sortedNums
        case (_, Nil) => sortedNums ++ leftNums
        case (Nil, _) => sortedNums ++ rightNums
        case (_, _) =>
          val rightHead: BigDecimal = rightNums.head
          val insertionPoint = leftNums.indexWhere(item => rightHead <= item)

          if (insertionPoint == -1) sortedNums ++ leftNums ++ rightNums
          else {
            val newSortedNums: Seq[BigDecimal] = sortedNums ++ leftNums.take(insertionPoint) :+ rightHead
            combine(leftNums.drop(insertionPoint), rightNums.tail, newSortedNums)
          }
      }
    }

    nums match {
      case Nil => Nil
      case num :: Nil => nums
      case _ => split(nums).map(doSort).fold(Seq.empty[BigDecimal])((leftNums, rightNums) => combine(leftNums, rightNums))
    }
  }
}
