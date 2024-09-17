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

import sorts.v2.SortV2.placeItemInSortedList

import scala.annotation.tailrec

object InsertionSortV2 extends SortV2 {
  def sort[T](items: Seq[T])(implicit ordering: Ordering[T]): Seq[T] = {
    @tailrec
    def doSort(unsortedItems: Seq[T], sortedItems: Seq[T] = Nil): Seq[T] = unsortedItems match {
      case head :: (tail@_ :: _) => doSort(tail, placeItemInSortedList(head, sortedItems))
      case head :: _ => placeItemInSortedList(head, sortedItems)
      case _=> sortedItems
    }

    doSort(items)
  }
}
