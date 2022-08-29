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

object CountingSort extends Sort[BigInt] {
  def sort(list: Seq[BigInt]): Seq[BigInt] = {
    val maxNumOpt: Option[BigInt] = list.headOption.map(
      head => list.tail.fold(head)((num1, num2) => if (num1 >= num2) num1 else num2)
    )

    @tailrec
    def doCount(nums: Seq[BigInt], counts: Seq[BigInt]): Seq[BigInt] = {
      def addToCount(num: BigInt): Seq[BigInt] = counts

      nums match {
        case Nil => counts
        case head :: Nil => addToCount(head)
        case head :: tail => doCount(tail, addToCount(head))
      }
    }

    val counts: Seq[BigInt] = doCount(list, maxNumOpt.fold(list)(maxNum => Seq.fill(maxNum.intValue + 1)(0)))

    ???
  }
}
