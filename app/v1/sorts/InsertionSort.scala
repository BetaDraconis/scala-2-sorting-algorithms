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

object InsertionSort extends Sort {
  def sort(nums: Seq[BigDecimal]): Seq[BigDecimal] = doSort(nums)

  @tailrec
  private def doSort(unsortedNums: Seq[BigDecimal],
                     sortedNums: Seq[BigDecimal] = Seq.empty): Seq[BigDecimal] = (unsortedNums, sortedNums) match {
    case (Nil, _) => sortedNums
    case (_, Nil) => doSort(unsortedNums.tail, unsortedNums.take(1))
    case _ =>
      val key = unsortedNums.head
      val splitNums = sortedNums.partition(num => num <= key)

      val newSortedNums = (splitNums._1 :+ key) ++ splitNums._2
      doSort(unsortedNums.tail, newSortedNums)
  }
}
