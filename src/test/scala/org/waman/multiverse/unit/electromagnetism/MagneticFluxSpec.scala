package org.waman.multiverse.unit.electromagnetism

import org.waman.multiverse.MultiverseCustomSpec
import org.waman.multiverse.implicits._
import org.waman.multiverse.unit.electromagnetism.MagneticFluxUnits._

class MagneticFluxSpec extends MultiverseCustomSpec {

  "Quantity" - {

    "3.0 <<magnetic flux unit>> should be converted to the equivalent value in weber" in {
      // Exercise
      val conversions =
        Table(
          ("magnetic flux", "expected"),
          (3.0(Wb), 3.0),
          (3.0(mWb), 3.0e-3),
          (3.0(Mx), 3.0e-8),
          (3.0(kMx), 3.0e-5)
        )
      // Verify
      forAll(conversions) { (sut: MagneticFlux[Double], expected: Double) =>
        sut(Wb) should equal(%%%(expected))
      }
    }

    "3.0(Wb) should be converted to the equivalent value in other magnetic flux units" in {
      // SetUp
      val q = 3.0(Wb)
      // Exercise
      val conversions =
        Table(
          ("magnetic flux", "expected"),
          (q(Wb), 3.0),
          (q(mWb), 3.0e3),
          (q(Mx), 3.0e8),
          (q(kMx), 3.0e5)
        )
      // Verify
      forAll(conversions) { (sut: Double, expected: Double) =>
        sut should equal(%%%(expected))
      }
    }
  }
}
