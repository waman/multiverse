package waman.multiverse.unit

import waman.multiverse.MultiverseCustomSpec
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
  }
}
