package org.waman.multiverse

import org.waman.multiverse.predef.LengthUnits._

class PhysicalUnitSpec extends MultiverseCustomSpec{

  "Order" - {

    "(m) should be less than (km)" in {
      // Exercise
      val result = m < km
      // Verify
      result should be (true)
    }

    "(m) should not be less than (mm)" in {
      // Exercise
      val result = m < mm
      // Verify
      result should be (false)
    }
  }
}
