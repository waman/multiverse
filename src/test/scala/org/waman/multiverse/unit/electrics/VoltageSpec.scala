package org.waman.multiverse.unit.electrics

import org.waman.multiverse.MultiverseCustomSpec
import org.waman.multiverse.unit.electrics.VoltageUnits._

class VoltageSpec extends MultiverseCustomSpec {

  "Unit" - {

    "A voltage unit should have the proper dimension" in {
      // SetUp
      import org.waman.multiverse.DimensionSymbol._
      // Exercise
      val sut = V.dimension
      // Verify
      sut should contain theSameElementsAs Map(M -> 1, L -> 2, T -> -3, I -> -1)
      sut(J) should be (0)
    }
  }
}
