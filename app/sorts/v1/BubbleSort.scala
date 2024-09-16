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
import scala.annotation.tailrec

object BubbleSort extends Sort[BigDecimal] {
  protected[sorts] def sort(nums: Seq[BigDecimal]): Seq[BigDecimal] = {

    @tailrec
    def doSort(uncheckedNums: Seq[BigDecimal], runHasSwaps: Boolean = false, checkedNums: Seq[BigDecimal] = Nil): Seq[BigDecimal] = uncheckedNums match {
      case Nil => if (runHasSwaps) doSort(checkedNums) else checkedNums
      case head :: Nil => if (runHasSwaps) doSort(checkedNums :+ head) else checkedNums :+ head
      case num1 :: num2 :: tail if num2 < num1 => doSort(num1 +: tail, runHasSwaps = true, checkedNums :+ num2)
      case num1 :: num2 :: tail => doSort(num2 +: tail, runHasSwaps, checkedNums :+ num1)
    }

    doSort(nums)
  }

}