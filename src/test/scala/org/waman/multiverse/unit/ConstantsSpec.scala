package org.waman.multiverse.unit

import org.waman.multiverse.MultiverseCustomSpec
import org.waman.multiverse.unit.basic.AreaUnitObjects
import spire.math.Real

class ConstantsSpec extends MultiverseCustomSpec {

  "[SOURCE GENERATION]" - {

    "Constants.Pi should have the collect value" in {
      // SetUp
      val expected = Real.pi
      // Exercise
      val sut = Constants.Pi
      // Verify
      sut should equal(expected)
    }

    "Constants.FootSquared should have the same value as the interval of foot squared " in {
      // SetUp
      val expected = AreaUnitObjects.foot_squared.interval
      // Exercise
      val sut = Constants.FootSquared
      // Verify
      sut should be (expected)
    }
  }
}
