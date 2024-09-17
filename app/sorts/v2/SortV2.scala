/*
 * Copyright 2023 Luke A Jones
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

import sorts.common.Sort

import scala.math.Ordered.orderingToOrdered

trait SortV2 extends Sort {
  def sort[T](nums: Seq[T])(implicit ordering: Ordering[T]): Seq[T]
}

object SortV2 {
  def isOrderedPair[T](firstItem: T, secondItem: T)(implicit ordering: Ordering[T]): Boolean = firstItem <= secondItem
}
