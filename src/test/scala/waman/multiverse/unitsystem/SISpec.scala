package waman.multiverse.unitsystem

import waman.multiverse.MultiverseCustomSpec
import waman.multiverse.implicits._
import waman.multiverse.unit.BasicUnits._

class SISpec extends MultiverseCustomSpec{

  "Length object is implicitly converted to Double value in metre unit" in {
    import waman.multiverse.unitsystem.SI._
    // Exercise
    val sut: Double = 1.0(km)
    // Verify
    sut should equal (%%%%(1000.0))
  }
}
