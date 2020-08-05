package org.waman.multiverse.unit.fluid

import org.waman.multiverse.MultiverseCustomSpec
import org.waman.multiverse.implicits._
import org.waman.multiverse.unit.fluid.PressureUnits._

class PressureSpec extends MultiverseCustomSpec {

  "Quantity" - {

    "3.0 <<pressure unit>> should be converted to the equivalent value in Pa" in {
      // Exercise
      val conversions =
        Table(
          ("pressure", "expected"),
          (3.0(at), 3.0*98066.5),
          (3.0(psi) , 3.0*6.894757293e3)
        )
      // Verify
      forAll(conversions){ (sut: Pressure[Double], expected: Double) =>
        sut(Pa) should equal (%%%%(expected))
      }
    }

    "3.0(Pa) should be converted to the equivalent value in other pressure units" in {
      // SetUp
      val q = 3.0 (Pa)
      // Exercise
      val conversions =
        Table(
          ("pressure", "expected"),
          (q(at), 3.0/98066.5),
          (q(psi) , 3.0/6.894757293e3)
        )
      // Verify
      forAll(conversions){ (sut: Double, expected: Double) =>
        sut should equal (%%%%(expected))
      }
    }
  }
}