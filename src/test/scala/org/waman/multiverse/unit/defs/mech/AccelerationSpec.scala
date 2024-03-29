package org.waman.multiverse.unit.defs.mech

import org.waman.multiverse.MultiverseCustomSpec
import org.waman.multiverse.implicits._
import org.waman.multiverse.unit.custom.BasicUnits._
import org.waman.multiverse.unit.custom.MechanicalUnits._

class AccelerationSpec extends MultiverseCustomSpec {

  "Unit" - {

    "The symbol property of AccelerationUnit should return the proper string" in {
      // Exercise
      val conversions =
        Table(
          ("acceleration", "expected"),
          (m/s2, "m/s²"),
          (km/s2, "km/s²"),
          (m/(s*s), "m/s²"),
          (m/(s*ms), "m/(s*ms)"),
          (m/s/s, "m/s/s"),
          (M/s, "M/s")
        )
      // Verify
      forAll(conversions){ (sut: AccelerationUnit, expected: String) =>
        sut.symbol should equal (expected)
      }
    }

    "Dimension of a quotient of units should equal the dimension of the corresponding quotient unit" in {
      // SetUp
      val expected = AccelerationUnit.dimension
      // Exercise
      val sut = (m/s2).dimension
      // Verify
      sut should contain theSameElementsAs expected
    }
  }

  "Quantity" - {

    "3.0 <<acceleration unit>> should be converted to the equivalent value in m/s²" in {
      // Exercise
      val conversions =
        Table(
          ("acceleration", "expected"),
          (3.0(mm/s2), 3e-3),
          (3.0(m/s2) , 3.0),
          (3.0(km/s2), 3e3)
        )
      // Verify
      forAll(conversions){ (sut: Acceleration[Double], expected: Double) =>
        sut(m/s2) should equal (%%%%(expected))
      }
    }

    "3.0(m/s2) should be converted to the equivalent value in other acceleration units" in {
      // SetUp
      val q = 3.0 (m/s2)
      // Exercise
      val conversions =
        Table(
          ("acceleration", "expected"),
          (q(mm/s2), 3e3),
          (q(m/s2) , 3.0),
          (q(km/s2), 3e-3)
        )
      // Verify
      forAll(conversions){ (sut: Double, expected: Double) =>
        sut should equal (%%%%(expected))
      }
    }
  }
}
