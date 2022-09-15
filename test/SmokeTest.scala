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

import support.UnitSpec
import v1.sorts.CountingSort

class SmokeTest extends UnitSpec {

  "this test" when {
    "run" should {
      "pass" in {
        "test" shouldBe "test"
      }
    }
  }

  CountingSort.sort(Seq(1,2,3))
}
