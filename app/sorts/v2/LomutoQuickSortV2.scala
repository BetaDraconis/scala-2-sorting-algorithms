package sorts.v2

import scala.annotation.tailrec
import scala.math.Ordered.orderingToOrdered

object LomutoQuickSortV2 extends QuickSort {
  @tailrec
  def partitionMethod[T](items: Seq[T],
                         pivotValue: T,
                         lowerPartition: Seq[T] = Nil,
                         upperPartition: Seq[T] = Nil)
                        (implicit ordering: Ordering[T]): (Seq[T], Seq[T]) = items match {
    case head :: (tail@_ :: _) =>
      if (head <= pivotValue) partitionMethod(tail, pivotValue, head +: lowerPartition, upperPartition)
      else partitionMethod(tail, pivotValue, lowerPartition, head +: upperPartition)
    case head :: Nil =>
      if (head <= pivotValue) (head +: lowerPartition, upperPartition)
      else (lowerPartition, head +: upperPartition)
    case _ => (lowerPartition, upperPartition)
  }
}
