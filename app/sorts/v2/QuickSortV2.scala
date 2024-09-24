package sorts.v2

import sorts.v2.SortV2.pickPivot

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
