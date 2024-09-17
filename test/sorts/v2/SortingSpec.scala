/*
 * Copyright 2022 Luke A Jones
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

import org.scalacheck.Gen
import support.UnitSpec

import scala.concurrent.duration.Duration

trait SortingSpec extends UnitSpec {

  final def sortingSmokeTest(sortingAlgorithm: SortV2): Unit = {

    def baseSortTest[T](given: String, toSort: Seq[T], sorted: Seq[T])(implicit ordering: Ordering[T]): Unit =
      s"given $given" should {
        "return a sorted list" in {
          sortingAlgorithm.sort(toSort) shouldBe sorted
        }
      }

    def testSortForRandomList[T](typeString: String, randomList: List[T])(implicit ordering: Ordering[T]): Unit =
      s"given a random list of type: $typeString" should {
        "return a sorted list" in {
          sortingAlgorithm.sort(randomList) shouldBe randomList.sorted
        }
      }

    s"${sortingAlgorithm.getClass.getSimpleName.dropRight(1)}" when {
      baseSortTest(
        given = "an empty list",
        toSort = List.empty[BigDecimal],
        sorted = List.empty[BigDecimal]
      )

      baseSortTest(
        given = "a single item list",
        toSort = List[BigDecimal](3.14),
        sorted = List[BigDecimal](3.14)
      )

      baseSortTest(
        given = "a list with multiple items",
        toSort = List[BigDecimal](3, 7, 12, 2.31, 43, 65, 5, 5),
        sorted = List[BigDecimal](2.31, 3, 5, 5, 7, 12, 43, 65)
      )

      // Character types
      testSortForRandomList("String", Gen.listOf[String](Gen.asciiPrintableStr).sample.get)
      testSortForRandomList("Char", Gen.listOf[Char](Gen.asciiChar).sample.get)

      // Decimal number types
      testSortForRandomList("Double", Gen.listOf[Double](Gen.double).sample.get)
      testSortForRandomList("Float", Gen.listOf[Float](Gen.double.map(_.toFloat)).sample.get)
      testSortForRandomList("BigDecimal", Gen.listOf[BigDecimal](Gen.double.map(BigDecimal.decimal)).sample.get)

      // Integer number types
      testSortForRandomList("Byte", Gen.listOf[Byte](Gen.long.map(_.toByte)).sample.get)
      testSortForRandomList("Short", Gen.listOf[Short](Gen.long.map(_.toShort)).sample.get)
      testSortForRandomList("Int", Gen.listOf[Int](Gen.long.map(_.toInt)).sample.get)
      testSortForRandomList("Long", Gen.listOf[Long](Gen.long).sample.get)
      testSortForRandomList("BigInt", Gen.listOf[BigInt](Gen.long.map(BigInt.long2bigInt)).sample.get)

      // Other types
      testSortForRandomList("Duration", Gen.listOf[Duration](Gen.duration).sample.get)
    }
  }
}
