package org.waman.multiverse

import org.waman.multiverse.implicits._
import org.waman.multiverse.predef.LengthUnits._

class MKSUnitSystemSpec extends MultiverseCustomSpec{

  "Length object is implicitly converted to Double value in metre unit" in {
    import org.waman.multiverse.MKSUnitSystem._
    // Exercise
    val sut: Double = 1.0(km)
    // Verify
    sut should equal (%%%%(1000.0))
  }
}
