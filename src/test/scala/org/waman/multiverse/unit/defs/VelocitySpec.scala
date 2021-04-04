package org.waman.multiverse.unit.defs

import org.waman.multiverse.MultiverseCustomSpec
import org.waman.multiverse.implicits._
import org.waman.multiverse.unit.defs.LengthUnits._
import org.waman.multiverse.unit.defs.TimeUnits._
import org.waman.multiverse.unit.defs.VelocityUnits._

class VelocitySpec extends MultiverseCustomSpec {

  "Quantity" - {

    "3.0 <<velocity unit>> should be converted to the equivalent value in m/s" in {
      // Exercise
      val conversions =
        Table(
          ("velocity", "expected"),
          (3.0(mm/s), 3e-3),
          (3.0(m/s) , 3.0),
          (3.0(km/h), 3e3/3600.0),
          (3.0(kn), 3.0*1852.0/3600.0)
        )
      // Verify
      forAll(conversions){ (sut: Velocity[Double], expected: Double) =>
        sut(m/s) should equal (%%%%(expected))
      }
    }

    "3.0(m/s) should be converted to the equivalent value in other velocity units" in {
      // SetUp
      val q = 3.0 (m/s)
      // Exercise
      val conversions =
        Table(
          ("velocity", "expected"),
          (q(mm/s), 3e3),
          (q(m/s) , 3.0),
          (q(km/h), 3e-3*3600.0),
          (q(kn), 3.0*3600.0/1852.0)
        )
      // Verify
      forAll(conversions){ (sut: Double, expected: Double) =>
        sut should equal (%%%%(expected))
      }
    }

    "Division between quantities should work well" in {
      // SetUp
      val x = 6(km)
      val t = 5(min)
      val expected = 72(km/h)
      // Exercise
      val sut = x / t
      // Verify
      sut should equal (expected)
    }
  }
}