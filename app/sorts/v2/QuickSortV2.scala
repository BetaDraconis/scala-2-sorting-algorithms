package sorts.v2

import sorts.v2.QuickSortV2.pickPivot

trait QuickSortV2 extends SortV2 {

  //TODO: Implement option for median of median pivoting to increase efficiency of QuickSort
  def partitionMethod[T](items: Seq[T],
                         pivotValue: T,
                         lowerPartition: Seq[T] = Nil,
                         upperPartition: Seq[T] = Nil)(implicit ordering: Ordering[T]): (Seq[T], Seq[T])

  def sort[T](items: Seq[T])(implicit ordering: Ordering[T]): Seq[T] = {
    def doSort(items: Seq[T]): Seq[T] = items match {
      case _ :: (_@_ :: _) =>
        val (pivot, itemsWithoutPivot) = pickPivot(items)
        val (lowerPartition, upperPartition) = partitionMethod(itemsWithoutPivot, pivot)
        doSort(lowerPartition) ++ (pivot +: doSort(upperPartition))
      case _ :: Nil => items
      case _ => items
    }

    doSort(items)
  }

}

object QuickSortV2 {
  // This is only defined for non-empty lists of items
  def lastValuePivot[T]: Seq[T] => (T, Seq[T]) = (items: Seq[T]) => (items.last, items.dropRight(1))

  def pickPivot[T](items: Seq[T], pivotApproach: Seq[T] => (T, Seq[T]) = lastValuePivot[T]): (T, Seq[T]) =
    pivotApproach(items)
}
