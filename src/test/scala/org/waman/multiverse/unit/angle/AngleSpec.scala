package org.waman.multiverse.unit.angle

import org.waman.multiverse.MultiverseCustomSpec
import org.waman.multiverse.implicits._
import org.waman.multiverse.unit.angle.AngleUnits._

class AngleSpec extends MultiverseCustomSpec {

  "Unit" - {

    "The degree unit should return the proper string (in the case that the interval is an irrational value π/180)" in {
      // SetUp
      val expected = "degree (°) [1(°) = 0.0174532925...(rad)] aliases: [deg] dim: -"
      // Exercise
      val sut = deg.toString
      // Verify
      sut should be (expected)
    }

    "The dimension of an angle unit should be dimensionless" in {
      // SetUp
      import org.waman.multiverse.DimensionSymbol._
      // Exercise
      val sut = deg.dimension
      // Verify
      sut should be (empty)
      sut(L) should be (0)
    }
  }

  "Quantity" - {

    "An angle quantity with degree unit should return the proper string" in {
      // SetUp
      val expected = "20.0(°)"
      // Exercise
      val sut = 20.0 (deg)
      // Verify
      sut.toString should be (expected)
    }

    "3.0 <<angle unit>> should be converted to the equivalent value in radian" in {
      // Exercise
      val conversions =
        Table(
          ("angle", "expected"),
          (3.0(rad), 3.0),
          (3.0(deg) , 3.0 * Math.PI / 180),
          (3.0(as), 3.0 * Math.PI / 180 / 3600)
        )
      // Verify
      forAll(conversions){ (sut: Angle[Double], expected: Double) =>
        sut(rad) should equal (%%%%(expected))
      }
    }

    "3.0(m) should be converted to the equivalent value in other angle units" in {
      // SetUp
      val q = 3.0 (rad)
      // Exercise
      val conversions =
        Table(
          ("angle", "expected"),
          (q(rad), 3.0),
          (q(deg) , 3.0 * 180 / Math.PI),
          (q(as), 3.0 * 3600 * 180 / Math.PI)
        )
      // Verify
      forAll(conversions){ (sut: Double, expected: Double) =>
        sut should equal (%%%%(expected))
      }
    }
  }
}
