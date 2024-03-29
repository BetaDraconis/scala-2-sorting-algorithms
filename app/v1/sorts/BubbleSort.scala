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

object BubbleSort extends Sort[BigDecimal] {
  protected[sorts] def sort(nums: Seq[BigDecimal]): Seq[BigDecimal] = {
    val listLength = nums.length
    if (listLength > 0) doSort(nums, maxIndex = nums.length - 1) else nums
  }

  @tailrec
  private def doSort(list: Seq[BigDecimal], currIndex: Int = 0, runCount: Int = 0, maxIndex: Int): Seq[BigDecimal] = (currIndex, runCount) match {
    case (`maxIndex`, 0) => list
    case (`maxIndex`, _) => doSort(list, maxIndex = maxIndex)
    case (_, _) if list(currIndex) > list(currIndex + 1) => doSort(swap(list, currIndex, currIndex +1), currIndex + 1, runCount +1, maxIndex)
    case (_, _) => doSort(list, currIndex +1, runCount, maxIndex)
  }
}