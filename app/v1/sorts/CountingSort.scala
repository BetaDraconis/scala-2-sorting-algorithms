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

object CountingSort extends Sort[Integer] {
  def sort(nums: Seq[Integer]): Seq[Integer] = {

    @tailrec
    def doCount(nums: Seq[Integer], counts: List[Integer]): List[Integer] = {
      def addToCount(num: Integer): List[Integer] = counts.updated(index = num, elem = counts(num) + 1)

      nums match {
        case Nil => counts
        case head :: Nil => addToCount(head)
        case head :: tail => doCount(tail, addToCount(head))
      }
    }

    nums match {
      case Nil => nums
      case _ :: Nil => nums
      case head :: tail =>

        val maxNum = tail.fold(head)((num1, num2) => if (num1 >= num2) num1 else num2)
        val counts: List[Integer] = doCount(nums, List.fill(maxNum + 1)(0))

        val sortedNums = counts.foldLeft((0, Seq.empty[Integer]))(
          (state, count) => {
            lazy val (num, currNums) = state
            val nextNum = num + 1
            if (count == 0) (nextNum, currNums) else (nextNum, currNums ++ Seq.fill(count)(num))
          }
        )

        sortedNums._2
    }
  }

}
