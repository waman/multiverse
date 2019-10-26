package org.waman.multiverse.unit.luminous

import org.waman.multiverse.MultiverseCustomSpec
import org.waman.multiverse.unit.basic.AreaUnits._
import org.waman.multiverse.unit.luminous.LuminanceUnitObjects.foot_lambert
import spire.implicits._
import spire.math.Real

class LuminanceSpec extends MultiverseCustomSpec {

  "[SOURCE GENERATION] " - {

    "The unit value of foot lambert should be collect value" in {
      // {"name":"foot lambert", "symbols":"fLb", "interval":"candela / (Pi * square_foot)"}
      // SetUp
      val expected = 1 / (Real.pi * ft2.interval)
      // Exercise
      val sut = foot_lambert
      // Verify
      sut.interval should equal (expected)
    }
  }
}
