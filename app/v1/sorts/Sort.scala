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

package v1.sorts

trait Sort[NumType <: Number] {
  protected[sorts] def sort(list: Seq[NumType]): Seq[NumType]

  def filteredSort(list: Seq[NumType]): Seq[NumType] = list match {
    case Nil => Nil
    case _ :: Nil => list
    case _ => sort(list)
  }

  protected[sorts] def swap(list: Seq[NumType], index1: Int, index2: Int): Seq[NumType] =
    list.slice(0, index1) ++ Seq(list(index2)) ++ list.slice(index1 + 1, index2) ++ Seq(list(index1)) ++ list.drop(index2 + 1)
}
