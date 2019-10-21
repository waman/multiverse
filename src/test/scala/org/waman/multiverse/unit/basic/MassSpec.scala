package org.waman.multiverse.unit.basic

import org.waman.multiverse.MultiverseCustomSpec
import org.waman.multiverse.implicits._
import org.waman.multiverse.unit.BasicUnits._

class MassSpec extends MultiverseCustomSpec {

  "Quantity" - {

    "3.0 <<mass unit>> should be converted to the equivalent value in kilogram" in {
      // Exercise
      val conversions =
        Table(
          ("mass", "expected"),
          (3.0(kg) , 3.0),
          (3.0(g), 3e-3),
          (3.0(mg), 3e-6),
          (3.0(t), 3e3)
        )
      // Verify
      forAll(conversions){ (sut: Mass[Double], expected: Double) =>
        sut(kg) should equal (%%%%(expected))
      }
    }

    "3.0(kg) should be converted to the equivalent value in other mass units" in {
      // SetUp
      val q = 3.0 (kg)
      // Exercise
      val conversions =
        Table(
          ("mass", "expected"),
          (q(kg) , 3.0),
          (q(g), 3e3),
          (q(mg), 3e6),
          (q(t), 3e-3)
        )
      // Verify
      forAll(conversions){ (sut: Double, expected: Double) =>
        sut should equal (%%%%(expected))
      }
    }
  }
}
