package org.waman.multiverse.unit.electric

import org.waman.multiverse.MultiverseCustomSpec
import org.waman.multiverse.unit.Constants
import org.waman.multiverse.unit.electric.VoltageUnitObjects.statvolt

class VoltageSpec extends MultiverseCustomSpec {

  "[SOURCE GENERATION]" - {

    "The unit value of stat should be collect value (the interval string is a quotient of constant and number)" in {
      // Exercise
      val sut = statvolt.interval
      // Verify
      sut should equal (Constants.SpeedOfLight / 1e6)
    }
  }
}
