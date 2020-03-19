package org.waman.multiverse.unit.photometry

import org.waman.multiverse.MultiverseCustomSpec
import org.waman.multiverse.unit.basic.AreaUnits._
import org.waman.multiverse.unit.photometry.IlluminanceUnitObjects.foot_candle
import spire.implicits._

class IlluminanceSpec extends MultiverseCustomSpec {

  "[SOURCE GENERATION] " - {

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
}
