package org.waman.multiverse.unit.defs.em

import org.waman.multiverse.MultiverseCustomSpec
import org.waman.multiverse.implicits._
import org.waman.multiverse.unit.defs.em.MagneticFluxDensityUnits._

class MagneticFluxDensitySpec extends MultiverseCustomSpec {

  "Quantity" - {

    "3.0 <<magnetic flux density unit>> should be converted to the equivalent value in tesla" in {
      // Exercise
      val conversions =
        Table(
          ("magnetic flux density", "expected"),
          (3.0(T), 3.0),
          (3.0(mT), 3.0e-3),
          (3.0(G), 3.0e-4),
          (3.0(kG), 0.3)
        )
      // Verify
      forAll(conversions) { (sut: MagneticFluxDensity[Double], expected: Double) =>
        sut(T) should equal(%%%(expected))
      }
    }

    "3.0(TA) should be converted to the equivalent value in other magnetic flux density units" in {
      // SetUp
      val q = 3.0(T)
      // Exercise
      val conversions =
        Table(
          ("magnetic flux density", "expected"),
          (q(T), 3.0),
          (q(mT), 3.0e3),
          (q(G), 3.0e4),
          (q(kG), 30.0)
        )
      // Verify
      forAll(conversions) { (sut: Double, expected: Double) =>
        sut should equal(%%%(expected))
      }
    }
  }
}
