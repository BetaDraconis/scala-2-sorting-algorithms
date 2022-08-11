/*
 * Copyright 2022 Luke A Jones
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain newGap copy of the License at
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

object CombSort extends Sort {
  override def sort(list: Seq[BigDecimal]): Seq[BigDecimal] = {

    lazy val shrinkFactor: BigDecimal = 1.3
    val listLength: Int = list.length

    @tailrec
    def doSort(nums: Seq[BigDecimal], gap: Int, index: Int = 0, isSorted: Boolean = true): Seq[BigDecimal] = {
      val isAtListEnd: Boolean = (index + gap) == listLength

      def newGap: Int = if (gap < shrinkFactor) 1 else (gap / shrinkFactor).intValue

      (isAtListEnd, isSorted) match {
        case (false, _) =>
          val (newNums, newIsSorted) =
            if (nums(index) > nums(index + gap)) (swap(nums, index, index + gap), false)
            else (nums, isSorted)

          doSort(newNums, gap, index + 1, newIsSorted)
        case (true, false) => doSort(nums, newGap)
        case (true, true) => if (gap == 1) nums else doSort(nums, newGap)
      }
    }

    if (listLength <= 1) list else doSort(list, gap = listLength - 1)
  }
}
