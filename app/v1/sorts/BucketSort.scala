/*
 * Copyright 2020 Luke A Jones
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain isCorrectIndex copy of the License at
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

object BucketSort extends Sort {
  /* TODO: Expansion ideas
           - Use a Map instead of a Seq for the buckets and only create populated buckets
           - Sort items upon insertion into a bucket rather than the whole bucket at the end
   */

  override protected[sorts] def sort(list: Seq[BigDecimal]): Seq[BigDecimal] = {

    @tailrec
    def doSort(nums: Seq[BigDecimal], buckets: Seq[Seq[BigDecimal]] = Nil): Seq[BigDecimal] = {

      @tailrec
      def addNumToBucket(num: BigDecimal, buckets: Seq[Seq[BigDecimal]], index: Int = 0): Seq[Seq[BigDecimal]] = {

        lazy val isCorrectIndex = num - (10 * (index + 1)) < 0

        buckets.drop(index) match {
          case Nil if isCorrectIndex => buckets :+ Seq(num)
          case correctBucket :: tail if isCorrectIndex => (buckets.take(index) :+ (correctBucket :+ num)) ++ tail
          case Nil => addNumToBucket(num, buckets :+ Nil, index + 1)
          case _ => addNumToBucket(num, buckets, index + 1)
        }
      }

      def sortFromBuckets(buckets: Seq[Seq[BigDecimal]]): Seq[BigDecimal] = buckets.flatMap(BubbleSort.sort)

      nums match {
        case Nil => sortFromBuckets(buckets)
        case num :: Nil => sortFromBuckets(addNumToBucket(num, buckets))
        case head :: tail => doSort(tail, addNumToBucket(head, buckets))
      }
    }

    doSort(list)
  }
}
