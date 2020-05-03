package waman.multiverse.unit.photometry

import waman.multiverse.MultiverseCustomSpec
import waman.multiverse.unit.basic.AreaUnits._
import waman.multiverse.unit.photometry.IlluminanceUnitObjects.foot_candle
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
