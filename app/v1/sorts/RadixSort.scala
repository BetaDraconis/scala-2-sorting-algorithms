package v1.sorts

import scala.annotation.tailrec

object RadixSort extends Sort[Integer]{
  override protected[sorts] def sort(list: Seq[Integer]): Seq[Integer] = {

    @tailrec
    def doSort(unsortedNums: Seq[Integer], sf: Int = 1, sortedNums: List[Integer] = Nil): Seq[Integer] = unsortedNums match {
        case Nil => sortedNums
        case last :: Nil => sortedNums :+ last
        case _ =>
          val emptyRadixBuckets = List.fill(10)(List.empty[Integer])

          val (bucketedRemainingNums, newSortedNums) = unsortedNums.foldLeft((emptyRadixBuckets, sortedNums))(
            (state, nextNum) => {
              val (currBucketedNums, currSortedNums) = state

              val explodedNextNum = nextNum.toString.toCharArray
              val sfRadixCharOpt = explodedNextNum.dropRight(sf - 1).lastOption

              sfRadixCharOpt.fold((currBucketedNums, currSortedNums :+ nextNum))(sfRadixChar => {
                val sfRadixInt: Int = sfRadixChar.toString.toInt
                val newRadixBucket: List[Integer] = currBucketedNums.apply(sfRadixInt) :+ nextNum
                val newBucketedNums = currBucketedNums.updated(sfRadixInt, newRadixBucket)
                (newBucketedNums, currSortedNums)
              })
            }
          )

          doSort(bucketedRemainingNums.flatten, sf + 1, newSortedNums)
      }

   doSort(list)
  }
}
