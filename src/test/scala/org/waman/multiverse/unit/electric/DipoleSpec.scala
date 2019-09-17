package org.waman.multiverse.unit.electric

import spire.implicits._

import org.waman.multiverse.MultiverseCustomSpec
import org.waman.multiverse.unit.electric.ChargeUnitObjects.statcoulomb
import org.waman.multiverse.unit.electric.DipoleUnitObjects.debye

class DipoleSpec extends MultiverseCustomSpec {

  "The unit value of stat should be collect value (code generation is not supported)" in {
    // Exercise
    val sut = debye.interval
    // Verify
    sut should equal (r"1e-20" * statcoulomb.interval)
  }
}
