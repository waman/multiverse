package org.waman.multiverse.unit.electric

import org.waman.multiverse.MultiverseCustomSpec
import org.waman.multiverse.implicits._
import org.waman.multiverse.unit.electric.ConductanceUnits._
import org.waman.multiverse.unit.electric.ResistanceUnits._

class ConductanceSpec extends MultiverseCustomSpec {

  "[SOURCE GENERATION] " - {

    "Conductance should be implicitly converted to Resistance and vice versa (reciprocal convertible)" in {
      // Exercise
      val conversions =
        Table(
          ("sut", "expected"),
          (3.0(S)(ohm), 1.0 / 3.0),
          (3.0(mS)(mohm), 1.0e6 / 3.0),
          (3.0(ohm)(S), 1.0 / 3.0),
          (3.0(kohm)(kS), 1.0e-6 / 3.0)
        )
      forAll(conversions){ (sut: Double, expected: Double) =>
        // Verify
        sut should equal (%%%%(expected))
      }
    }
  }
}
