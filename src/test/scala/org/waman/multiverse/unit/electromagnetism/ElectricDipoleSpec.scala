package org.waman.multiverse.unit.electromagnetism

import org.waman.multiverse.MultiverseCustomSpec
import org.waman.multiverse.implicits._
import org.waman.multiverse.unit.basic.LengthUnits._
import org.waman.multiverse.unit.electromagnetism.ElectricChargeUnits._
import org.waman.multiverse.unit.electromagnetism.ElectricDipoleUnits._

class ElectricDipoleSpec extends MultiverseCustomSpec {

  "Unit" - {

    "Dimension of a product of units should equal the dimension of the corresponding product unit" in {
      // SetUp
      val expected = ElectricDipoleUnit.dimension
      // Exercise
      val sut = (C*m).dimension
      // Verify
      sut should contain theSameElementsAs expected
    }
  }

  "Quantity" - {

    "Multipliation between quantities should work well" in {
      // SetUp
      val q = 6(C)
      val x = 3(m)
      val expected = 18(C*m)
      // Exercise
      val sut = q*x
      // Verify
      sut should equal (expected)
    }

    "3.0 <<dipole unit>> should be converted to the equivalent value in C*m" in {
      // Exercise
      val conversions =
        Table(
          ("dipole", "expected"),
          (3.0(C*m), 3.0),
          (3.0(D), 3.0 * 3.33564095e-30),
          (3.0(ea0), 3.0 * 8.47835281e-30)
        )
      // Verify
      forAll(conversions) { (sut: ElectricDipole[Double], expected: Double) =>
        sut(C*m) should equal(%%%(expected))
      }
    }

    "3.0(C*m) should be converted to the equivalent value in other dipole units" in {
      // SetUp
      val q = 3.0(C*m)
      // Exercise
      val conversions =
        Table(
          ("dipole", "expected"),
          (q(C*m), 3.0),
          (q(D), 3.0 / 3.33564095e-30),
          (q(ea0), 3.0 / 8.47835281e-30)
        )
      // Verify
      forAll(conversions) { (sut: Double, expected: Double) =>
        sut should equal(%%%(expected))
      }
    }
  }
}
