package org.waman.multiverse.unit.basic

import org.waman.multiverse.MultiverseCustomSpec
import org.waman.multiverse.implicits._
import org.waman.multiverse.unit.basic.LengthUnits._
import org.waman.multiverse.unit.basic.TimeUnits._
import org.waman.multiverse.unit.basic.VelocityUnits._

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

  "[SOURCE GENERATION] " - {

    "Quotient unit 'km/s' should have combinated aliases" in {
      // SetUp
      val expected = Seq("Km/s", "Km/sec", "km/sec")
      // Exercise
      val sut = km/s
      // Verify
      sut.aliases should contain theSameElementsAs expected
      sut.symbol should be ("km/s")
    }

    "The getSIUnit method return the composite (quotient) unit m/s" in {
      // SetUp
      val expected = m/s
      // Exercise
      val sut = VelocityUnit.getSIUnit
      // Verify
      sut should be (expected)
    }

    "The getUnits method of VelocityUnits should return Seq(c, M)" in {
      // Exercise
      val sut = VelocityUnit.getUnits
      // Verify
      sut should contain theSameElementsInOrderAs Seq(c, M, kt, kine)
    }
  }
}