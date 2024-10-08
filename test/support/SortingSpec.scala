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

package support

import sorts.common.Sort

trait SortingSpec extends UnitSpec {

  // TODO: Test this with and without filtered sort to catch any potential issues wrt empty sequences being supplied
  // TODO: Update this to support non BigDecimal types
  final def sortingSmokeTest(sortingAlgorithm: Sort[BigDecimal]): Unit = {
    s"${sortingAlgorithm.getClass.getSimpleName.dropRight(1)}" when {
      "given an empty list" should {
        "return an empty list" in {
          val emptyList: List[BigDecimal] = List.empty[BigDecimal]
          sortingAlgorithm.filteredSort(emptyList) shouldBe emptyList
        }
      }

      "given a single item list" should {
        "return a single item list" in {
          val singleItemList: List[BigDecimal] = List(3.14)
          sortingAlgorithm.filteredSort(singleItemList) shouldBe singleItemList
        }
      }

      "given a list with many items" should {
        "return a sorted list" in {
          val unsortedList: List[BigDecimal] = List(3, 7, 12, 2.31, 43, 65, 5, 5)
          val sortedList: List[BigDecimal] = List(2.31, 3, 5, 5, 7, 12, 43, 65)
          sortingAlgorithm.filteredSort(unsortedList) shouldBe sortedList
        }
      }
    }
  }

  final def sortingSmokeTestInt(sortingAlgorithm: Sort[Integer]): Unit = {
    s"${sortingAlgorithm.getClass.getSimpleName.dropRight(1)}" when {
      "given an empty list" should {
        "return an empty list" in {
          val emptyList: List[Integer] = List.empty[Integer]
          sortingAlgorithm.filteredSort(emptyList) shouldBe emptyList
        }
      }

      "given a single item list" should {
        "return a single item list" in {
          val singleItemList: List[Integer] = List(3)
          sortingAlgorithm.filteredSort(singleItemList) shouldBe singleItemList
        }
      }

      "given a list with many items" should {
        "return a sorted list" in {
          val unsortedList: List[Integer] = List(3, 7, 12, 2, 43, 65, 5, 5)
          val sortedList: List[Integer] = List(2, 3, 5, 5, 7, 12, 43, 65)
          sortingAlgorithm.filteredSort(unsortedList) shouldBe sortedList
        }
      }
    }
  }
}
