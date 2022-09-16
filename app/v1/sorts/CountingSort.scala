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
  def sort(list: Seq[Integer]): Seq[Integer] = {

    @tailrec
    def doCount(nums: Seq[Integer], counts: Seq[Integer]): Seq[Integer] = {
      def addToCount(num: Integer): Seq[Integer] = counts.updated(index = num, elem = counts(num) + 1)

      nums match {
        case Nil => counts
        case head :: Nil => addToCount(head)
        case head :: tail => doCount(tail, addToCount(head))
      }
    }

    @tailrec
    def makeCountCumulative(counts: Seq[Integer], index: Integer = 0): Seq[Integer] = counts.drop(index) match {
      case Nil => counts
      case _ :: Nil => counts
      case Seq(head: Integer, nextVal: Integer) => counts.updated(index + 1, head + nextVal)
      case Seq(head: Integer, nextVal: Integer, _*)  => makeCountCumulative(counts.updated(index = index + 1, elem = head + nextVal), index + 1)
    }

    @tailrec
    def placeNums(counts: Seq[Integer], sortedNums: Seq[Integer], index: Integer = 0): Seq[Integer] = {

      list.drop(index) match {
        case Nil => sortedNums
        case head :: Nil => sortedNums.updated(index = counts(head) - 1, elem = head)
        case head :: _ => placeNums(
          counts = counts.updated(index = head, elem = counts(head) - 1),
          sortedNums = sortedNums.updated(index = counts(head) - 1, elem = head),
          index = index + 1
        )
      }
    }

    list match {
      case Nil => list
      case _ :: Nil => list
      case head :: tail =>
        val maxNum = tail.fold(head)((num1, num2) => if (num1 >= num2) num1 else num2)
        val counts: Seq[Integer] = doCount(list, Seq.fill(maxNum + 1)(0))
        val cumulativeCount: Seq[Integer] = makeCountCumulative(counts)

        placeNums(cumulativeCount, list)
    }
  }

}
