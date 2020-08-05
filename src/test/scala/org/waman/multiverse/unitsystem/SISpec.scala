package org.waman.multiverse.unitsystem

import org.waman.multiverse.MultiverseCustomSpec
import org.waman.multiverse.implicits._
import org.waman.multiverse.unit.BasicUnits._

class SISpec extends MultiverseCustomSpec{

  "Length object is implicitly converted to Double value in metre unit" in {
    import SI._
    // Exercise
    val sut: Double = 1.0(km)
    // Verify
    sut should equal (%%%%(1000.0))
  }
}
