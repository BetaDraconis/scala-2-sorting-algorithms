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

import sorts.v2.SortV2.{isOrderedPair, prependAndReverseResult}

import scala.annotation.tailrec

object BubbleSortV2 extends SortV2 {
  override def sort[T](items: Seq[T])(implicit ordering: Ordering[T]): Seq[T] = {

    @tailrec
    def doSort(items: Seq[T],
               checkedItems: Seq[T] = Nil,
               hasSwaps: Boolean = false)
              (implicit ordering: Ordering[T]): Seq[T] = items match {
      case head :: (tail@secondItem :: secondTail) =>
        if (isOrderedPair(head, secondItem)) doSort(tail, head +: checkedItems, hasSwaps)
        else doSort(head +: secondTail, secondItem +: checkedItems, hasSwaps = true)
      case head :: Nil =>
        val runResult = prependAndReverseResult(head, checkedItems)
        if (hasSwaps) doSort(runResult) else runResult
      case _ => Nil
    }

    doSort(items)
  }

  /*
  This implementation uses foldLeft to iterate over each item in the list to complete a 'run', and then uses
  tail recursion to start a new 'run' if swaps have been made during the fold comparison. I suspect this will be less
  efficient than the other implementation when handling long lists due to foldLeft being based upon a while loop
  instead of tail recursion which Scala is well optimised to handle
   */
  @tailrec
  def sortWithFold[T](items: Seq[T])(implicit ordering: Ordering[T]): Seq[T] = items match {
    case head :: tail =>
      val (nonLargestItems, largestItem, hasSwaps) = tail.foldLeft((Seq.empty[T], head, false))((state, nextItem) => {
        val (checkedItems, checkItem, hasSwaps) = state
        if (isOrderedPair(checkItem, nextItem)) (checkItem +: checkedItems, nextItem, hasSwaps)
        else (nextItem +: checkedItems, checkItem, true)
      })

      val runResult = (largestItem +: nonLargestItems).reverse
      if (hasSwaps) sortWithFold(runResult) else runResult
    case _ => items
  }

}
