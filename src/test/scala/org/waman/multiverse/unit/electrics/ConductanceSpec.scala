package org.waman.multiverse.unit.electrics

import org.waman.multiverse.MultiverseCustomSpec
import org.waman.multiverse.implicits._
import org.waman.multiverse.unit.electrics.ConductanceUnits._
import org.waman.multiverse.unit.electrics.ResistanceUnits._

class ConductanceSpec extends MultiverseCustomSpec {

  "[SOURCE GENERATION] " - {

    "Conductance should be implicitly converted to Resistance and vice versa (reciprocal convertible)" in {
      // Exercise
      val conversions =
        Table(
          ("sut", "expected"),
          (3.0(S).toResistance(ohm), 1.0 / 3.0),
          (3.0(mS).toResistance(mohm), 1.0e6 / 3.0),
          (3.0(ohm).toConductance(S), 1.0 / 3.0),
          (3.0(kohm).toConductance(kS), 1.0e-6 / 3.0)
        )
      forAll(conversions){ (sut: Double, expected: Double) =>
        // Verify
        sut should equal (%%%%(expected))
      }
    }
  }
}
