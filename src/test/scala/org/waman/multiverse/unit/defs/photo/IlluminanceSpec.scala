package org.waman.multiverse.unit.defs.photo

import org.waman.multiverse.MultiverseCustomSpec
import org.waman.multiverse.unit.defs.AreaUnits._
import org.waman.multiverse.unit.defs.photo.IlluminanceUnitObjects.foot_candle
import spire.implicits._

class IlluminanceSpec extends MultiverseCustomSpec {

  "The unit value of foot candle should be collect value" in {
    // {"name":"foot candle", "symbol":"fc", "baseUnit":"LuminousFlux.lumen / Area.square_foot"}
    // SetUp
    val expected = 1 / ft2.interval
    // Exercise
    val sut = foot_candle
    // Verify
    sut.interval should equal (expected)
  }
}
