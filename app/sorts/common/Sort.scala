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

package sorts.common

trait Sort[NumType <: Number] {
  protected[sorts] def sort(nums: Seq[NumType]): Seq[NumType]

  def filteredSort(nums: Seq[NumType]): Seq[NumType] = nums match {
    case Nil => Nil
    case _ :: Nil => nums
    case _ => sort(nums)
  }
}

object Sort {
  def swap[NumType <: Number](nums: Seq[NumType], index1: Int, index2: Int): Seq[NumType] = {
    if (index1 == index2) nums
    else nums.slice(0, index1) ++ Seq(nums(index2)) ++nums.slice(index1 + 1, index2) ++ Seq(nums(index1)) ++ nums.drop(index2 + 1)
  }
}
