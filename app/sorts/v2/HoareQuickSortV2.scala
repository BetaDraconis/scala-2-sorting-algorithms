package sorts.v2

import scala.annotation.tailrec
import scala.math.Ordered.orderingToOrdered

object HoareQuickSortV2 extends QuickSort {
  @tailrec
  def partitionMethod[T](items: Seq[T],
                         pivot: T,
                         lowerPartition: Seq[T] = Nil,
                         upperPartition: Seq[T] = Nil)
                        (implicit ordering: Ordering[T]): (Seq[T], Seq[T]) = items match {
    case head :: (tail@middle :+ last) =>
      (head < pivot, last >= pivot) match {
        case (true, true) => partitionMethod(middle, pivot, head +: lowerPartition, last +: upperPartition)
        case (true, _) => partitionMethod(tail, pivot, head +: lowerPartition, upperPartition)
        case (_, true) => partitionMethod(head +: middle, pivot, lowerPartition, last +: upperPartition)
        case (_, _) => partitionMethod(middle, pivot, last +: lowerPartition, head +: upperPartition)
      }
    case head :: Nil =>
      if (head <= pivot) (head +: lowerPartition, upperPartition)
      else (lowerPartition, head +: upperPartition)
    case _ => (lowerPartition, upperPartition)
  }
}
