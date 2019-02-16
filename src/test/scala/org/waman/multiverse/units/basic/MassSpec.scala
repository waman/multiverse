package org.waman.multiverse.units.basic

import org.waman.multiverse.MultiverseCustomSpec
import org.waman.multiverse.implicits._
import org.waman.multiverse.predef.BasicUnits._

class MassSpec extends MultiverseCustomSpec {

  "3.0 <<mass unit>> should be converted to the equivalent value in metre" in {
    // Exercise
    val conversions =
      Table(
        ("mass", "expected"),
        (3.0(g), 3e-3),
        (3.0(kg) , 3.0),
        (3.0(t), 3e3)
      )
    // Verify
    forAll(conversions){ (sut: Mass[Double], expected: Double) =>
      sut(kg) should equal (%%%%(expected))
    }
  }

  "3.0(m) should be converted to the equivalent value in other mass units" in {
    // SetUp
    val q = 3.0 (kg)
    // Exercise
    val conversions =
      Table(
        ("mass", "expected"),
        (q(g), 3e3),
        (q(kg) , 3.0),
        (q(t), 3e-3)
      )
    // Verify
    forAll(conversions){ (sut: Double, expected: Double) =>
      sut should equal (%%%%(expected))
    }
  }
}