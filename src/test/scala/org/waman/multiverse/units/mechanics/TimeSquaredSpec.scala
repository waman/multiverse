package org.waman.multiverse.units.mechanics

import org.waman.multiverse.MultiverseCustomSpec
import org.waman.multiverse.predef.MechanicsUnits._

class TimeSquaredSpec extends MultiverseCustomSpec {

  "symbol property of TimeSquared should return the proper string" in {
    // Exercise
    val sut = s2.symbol
    // Verify
    sut should equal("s²")
  }
}
