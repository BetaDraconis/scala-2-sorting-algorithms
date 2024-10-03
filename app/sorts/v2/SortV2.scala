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

import sorts.common.Sort

import scala.annotation.tailrec
import scala.math.Ordered.orderingToOrdered

trait SortV2 extends Sort {
  def sort[T](items: Seq[T])(implicit ordering: Ordering[T]): Seq[T]
}

object SortV2 {
  def isOrderedPair[T](firstItem: T, secondItem: T)(implicit ordering: Ordering[T]): Boolean =
    firstItem <= secondItem

  def prependAndReverseResult[T](itemToPrepend: T, listToPrependTo: Seq[T]): Seq[T] =
    (itemToPrepend +: listToPrependTo).reverse

  @tailrec
  def prependMultipleAndReverseResult[T](itemsToPrepend: Seq[T], listToPrependTo: Seq[T]): Seq[T] = itemsToPrepend match {
    case head :: (tail@_::_) => prependMultipleAndReverseResult(tail, head +: listToPrependTo)
    case head :: Nil => prependAndReverseResult(head, listToPrependTo)
    case Nil => listToPrependTo.reverse
  }

  @tailrec
  def placeItemInSortedList[T](itemToPlace: T,
                               uncheckedSortedItems: Seq[T],
                               checkedSortedItems: Seq[T] = Nil)
                              (implicit ordering: Ordering[T]): Seq[T] =
    uncheckedSortedItems match {
      case head :: (tail@_ :: _) =>
        if (isOrderedPair(itemToPlace, head)) prependMultipleAndReverseResult(Seq(itemToPlace, head), checkedSortedItems) ++ tail
        else placeItemInSortedList(itemToPlace, tail, head +: checkedSortedItems)
      case head :: Nil =>
        if (isOrderedPair(itemToPlace, head)) prependMultipleAndReverseResult(Seq(itemToPlace, head), checkedSortedItems)
        else prependMultipleAndReverseResult(Seq(head, itemToPlace), checkedSortedItems)
      case Nil => prependAndReverseResult(itemToPlace, checkedSortedItems)
    }

  @tailrec
  def listLength[T](items: Seq[T], length: Int = 0): Int = items match {
    case _ :: tail => listLength(tail, length + 1)
    case _ => length
  }

  def minMaxLength[T](items: Seq[T])(implicit ordering: Ordering[T]): Option[(T, T, Int)] = items match {
    case Nil => None
    case head :: Nil => Some((head, head, 1))
    case head :: tail => Some(
      tail.foldLeft((head, head, 1))((state, nextItem) =>
        if (nextItem < state._1) (nextItem, state._2, state._3 + 1)
        else if (nextItem > state._2) (state._1, nextItem, state._3 + 1)
        else (state._1, state._2, state._3 + 1)
      )
    )
  }

  @tailrec
  def extractMaxItem[T](items: Seq[T],
                        maxValue: T,
                        leftItems: Seq[T] = Nil,
                        rightItems: Seq[T] = Nil,
                        persistOrder: Boolean = false)
                       (implicit ordering: Ordering[T]): (Seq[T], T) = items match {
    case head :: (tail@_ :: _) =>
      if (isOrderedPair(head, maxValue)) {
        val newLeftItems = if (persistOrder) leftItems else head +: leftItems
        val newRightItems = if (persistOrder) head +: rightItems else rightItems
        extractMaxItem(tail, maxValue, newLeftItems, newRightItems, persistOrder)
      } else {
        val newLeftItems = if (persistOrder) rightItems ++ (maxValue +: leftItems) else maxValue +: leftItems
        extractMaxItem(tail, head, newLeftItems, persistOrder = persistOrder)
      }
    case head :: _ =>
      if (isOrderedPair(head, maxValue)) {
        val listWithoutMax = if (persistOrder) (rightItems ++ (head +: leftItems)).reverse else head +: leftItems
        (listWithoutMax, maxValue)
      }
      else {
        val listWithoutMax = if (persistOrder) ((maxValue +: rightItems) ++ leftItems).reverse else maxValue +: leftItems
        (listWithoutMax, head)
      }
    case _ => (items, maxValue)
  }
}
