/*
 * Copyright 2023 Luke A Jones
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

package sorts.v2

import sorts.v2.SortV2.{minMaxLength, placeItemInSortedList}

import scala.annotation.tailrec
import scala.collection.immutable.{AbstractSeq, LinearSeq}
import scala.math.Ordered.orderingToOrdered

object BucketSortV2 /*extends SortV2*/ {
//  def sort[T](items: Seq[T])(implicit ordering: Ordering[T]): Seq[T] = {
//
//    @tailrec
//    def sortItemIntoBucket(item: T,
//                           uncheckedBuckets: Seq[Seq[T]],
//                           min: T,
//                           interval: T,
//                           checkedBuckets: Seq[Seq[T]] = Nil,
//                           index: Int = 0)
//                          (implicit ordering: Ordering[T]): Seq[Seq[T]] =
//      uncheckedBuckets match {
//        case head :: Nil => (placeItemInSortedList(item, head) +: checkedBuckets).reverse
//        case head :: tail => if (item <= ((index + 1) * interval)) {
//          checkedBuckets.reverse +: (placeItemInSortedList(item, head) +: tail)
//        } else {
//          sortItemIntoBucket(item, tail, min, interval, head +: checkedBuckets, index + 1)
//        }
//        case Nil => Nil
//      }
//
//    @tailrec
//    def doSort(items: Seq[T], buckets: Seq[Seq[T]], min: T, interval: T): Seq[T] =
//      items match {
//      case head :: Nil => sortItemIntoBucket(head, buckets, min, interval).flatten
//      case head :: tail => doSort(tail, sortItemIntoBucket(head, buckets, min, interval), min, interval)
//      case Nil => buckets.flatten
//    }
//
//    minMaxLength(items) match {
//      case Some((min, max, length)) => doSort(items, Seq.fill[Seq[T]](length)(Seq.empty[T]), min, (max - min)/length)
//      case None => Nil
//    }
//  }
}
