package org.waman.multiverse.unit.defs.photometry

import org.waman.multiverse.MultiverseCustomSpec
import org.waman.multiverse.unit.defs.AreaUnits._
import org.waman.multiverse.unit.defs.photometry.LuminanceUnitObjects.foot_lambert
import spire.implicits._
import spire.math.Real

class LuminanceSpec extends MultiverseCustomSpec {

  "The unit value of foot lambert should be collect value" in {
    // {"name":"foot lambert", "symbols":"fLb",
    //   "interval":"1/Constants.Pi", "baseUnit":"LuminousIntensity.candela / Area.square_foot"}
    // SetUp
    val expected = 1 / (Real.pi * ft2.interval)
    // Exercise
    val sut = foot_lambert
    // Verify
    sut.interval should equal (expected)
  }
}
