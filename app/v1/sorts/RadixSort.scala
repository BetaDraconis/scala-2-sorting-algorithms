package v1.sorts

import scala.annotation.tailrec

object RadixSort extends Sort[Integer] {
  override protected[sorts] def sort(list: Seq[Integer]): Seq[Integer] = {

    @tailrec
    def doSort(unsortedNums: Seq[Integer], sf: Int = 1, sortedNums: List[Integer] = Nil): Seq[Integer] = unsortedNums match {
      case Nil => sortedNums
      case last :: Nil => sortedNums :+ last
      case _ =>
        val emptyRadixBuckets = List.fill(11)(List.empty[Integer])

        val bucketedNums = unsortedNums.foldLeft(emptyRadixBuckets)((currBuckets, nextNum) => {
          val explodedNextNum = nextNum.toString.toCharArray
          val sfValOpt = explodedNextNum.dropRight(sf - 1).lastOption
          val nextBucketIndex = sfValOpt.map(_.toString.toInt).getOrElse(10)
          val newRadixBucket: List[Integer] = currBuckets.apply(nextBucketIndex) :+ nextNum
          currBuckets.updated(nextBucketIndex, newRadixBucket)
        })

        val newSortedNums = sortedNums ++ bucketedNums.last
        val newUnsortedNums = bucketedNums.dropRight(1).flatten
        doSort(newUnsortedNums, sf + 1, newSortedNums)
    }

    doSort(list)
  }
}
