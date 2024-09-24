package sorts.v2

import sorts.v2.SortV2.{isOrderedPair, listLength}

import scala.annotation.tailrec

object CombSortV2 extends SortV2 {

  @tailrec
  def canTakeItems[T](items: Seq[T],
                      gap: Int,
                      timesItemTaken: Int = 1,
                      takenItems: Seq[T] = Nil): Option[(T, Seq[T], Seq[T])] = items match {
    case head :: (tail@_ :: _) =>
      if (timesItemTaken == gap) Some(head, takenItems.reverse, tail)
      else canTakeItems(tail, gap, timesItemTaken + 1, head +: takenItems)
    case head :: Nil => if (timesItemTaken == gap) Some(head, takenItems.reverse, Nil) else None
    case Nil => None
  }

  def newGap(gap: Int): Int = {
    val newGap = gap / 1.3
    if (newGap < 1) 1 else newGap.toInt
  }

  def sort[T](items: Seq[T])(implicit ordering: Ordering[T]): Seq[T] = {
    // This reduces down to Bubble Sort once the gap reduces to 1.
    @tailrec
    def doSort(items: Seq[T],
               checkedItems: Seq[T] = Nil,
               hasSwaps: Boolean = false,
               combGap: Int = 1)(implicit ordering: Ordering[T]): Seq[T] = items match {
      case _ :: _ :: _ if combGap == 1 => BubbleSortV2.sort(items)
      case head :: (tail@_::_) => canTakeItems(tail, combGap) match {
        case Some((gapValue, beforeGap, afterGap)) =>
          if (isOrderedPair(head, gapValue)) doSort(tail, head +: checkedItems, hasSwaps, combGap)
          else doSort(beforeGap ++ (head +: afterGap), gapValue +: checkedItems, hasSwaps = true, combGap)
        case None => doSort(checkedItems.reverse ++ items, combGap = newGap(combGap))
      }
      case _ => items
    }

    doSort(items, combGap = listLength(items))
  }
}
