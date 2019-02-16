package org.waman.multiverse.units.mechanics

import org.waman.multiverse.MultiverseCustomSpec
import org.waman.multiverse.implicits._
import org.waman.multiverse.predef.BasicUnits._

class AccelerationSpec extends MultiverseCustomSpec {

  "symbol property of AccelerationUnit should return the proper string" in {
    // Exercise
    val conversions =
      Table(
        ("acceleration", "expected"),
        (m/s^2, "m/s^2"),
        (km/s^2, "km/s^2"),
        (m/ms^2, "m/ms^2"),
        (m/s/ms, "m/s/ms"),
        (M/s, "M/s")
      )
    // Verify
    forAll(conversions){ (sut: AccelerationUnit, expected: String) =>
      sut.symbol should equal (expected)
    }
  }

  "3.0 <<acceleration unit>> should be converted to the equivalent value in m/s^2" in {
    // Exercise
    val conversions =
      Table(
        ("acceleration", "expected"),
        (3.0(mm/s^2), 3e-3),
        (3.0(m/s^2) , 3.0),
        (3.0(km/s^2), 3e3),
        (3.0(m/ms^2), 3e6)
      )
    // Verify
    forAll(conversions){ (sut: Acceleration[Double], expected: Double) =>
      sut(m/s^2) should equal (%%%%(expected))
    }
  }

  "3.0(m/s^2) should be converted to the equivalent value in other acceleration units" in {
    // SetUp
    val q = 3.0 (m/s^2)
    // Exercise
    val conversions =
      Table(
        ("acceleration", "expected"),
        (q(mm/s^2), 3e3),
        (q(m/s^2) , 3.0),
        (q(km/s^2), 3e-3),
        (q(m/ms^2), 3e-6)
      )
    // Verify
    forAll(conversions){ (sut: Double, expected: Double) =>
      sut should equal (%%%%(expected))
    }
  }
}
