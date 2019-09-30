package org.waman.multiverse.unit.basic

import org.waman.multiverse.MultiverseCustomSpec
import org.waman.multiverse.implicits._
import org.waman.multiverse.unit.BasicUnits._

class VelocitySpec extends MultiverseCustomSpec {

  "3.0 <<velocity unit>> should be converted to the equivalent value in m/s" in {
    // Exercise
    val conversions =
      Table(
        ("velocity", "expected"),
        (3.0(mm/s), 3e-3),
        (3.0(m/s) , 3.0),
        (3.0(km/h), 3e3/3600.0)
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
        (q(km/h), 3e-3*3600.0)
      )
    // Verify
    forAll(conversions){ (sut: Double, expected: Double) =>
      sut should equal (%%%%(expected))
    }
  }

  "getUnits method of VelocityUnits should return Seq(c, M)" in {
    // Exercise
    val sut = VelocityUnits.getUnits
    // Verify
    sut should contain theSameElementsInOrderAs Seq(c, M)
  }
}