package sorts.v2

import sorts.v2.SortV2.extractMaxItem

import scala.annotation.tailrec

object SelectionSortV2 extends SortV2 {
  override def sort[T](items: Seq[T])(implicit ordering: Ordering[T]): Seq[T] = {
    @tailrec
    def doSort(items: Seq[T], sortedItems: Seq[T] = Nil): Seq[T] = items match {
      case head :: (tail@ _ :: _) =>
        val (newItems, maxItem) = extractMaxItem(tail, head)
        doSort(newItems, maxItem +: sortedItems)
      case head :: Nil => head +: sortedItems
      case _ => Nil
    }

    doSort(items)
  }

}
