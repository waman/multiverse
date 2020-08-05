package org.waman.multiverse

import org.waman.multiverse.unit.BasicUnits._
import org.waman.multiverse.unit.basic.LengthUnits.a0
import org.waman.multiverse.unit.MechanicalUnits._
import org.waman.multiverse.unit.basic.VolumeUnitObjects

class LinearUnitSpec extends MultiverseCustomSpec{

  "Equality" - {

    "(mm/ms) should NOT equal (m/s)" in {
      // Exercise
      val sut = mm/ms == m/s
      // Verify
      sut should be (false)
    }
  }

  "Order" - {

    "(mm/ms) should DO equal (m/s) with the compare method" in {
      // Exercise
      val sut = (mm/ms).compare(m/s)
      // Verify
      sut should be (0)
    }

    "(m) should be less than (km)" in {
      // Exercise
      val sut = m < km
      // Verify
      sut should be (true)
    }

    "(m) should not be less than (mm)" in {
      // Exercise
      val sut = m < mm
      // Verify
      sut should be (false)
    }
  }

  "toString method" - {

    "toString method should return the proper string" in {
      val conversions =
        Table(
          ("length unit", "expected"),
          (mm, "millimetre (mm) [1(mm) = 0.001(m)], dim: L"),
          (m , "metre (m), dim: L"),  // SI unit
          (km, "kilometre (km) [1(km) = 1,000(m)], aliases: [Km], dim: L"),
          (km/s, "kilometre per second (km/s) [1(km/s) = 1,000(m/s)], aliases: [Km/s, km/sec, Km/sec], dim: LT⁻¹"),  // quotient unit
          (W/(cm2*Hz), "watt per square centimetre times heltz (W/(cm²*Hz)) [1(W/(cm²*Hz)) = 10,000(W/(m²*Hz))], aliases: [W/(cm2*Hz)], dim: MT⁻²"), // LiteralComposite
          (a0, "atomic unit of length (a0) [1(a0) ≈ 0.00000000005291772109217(m)], dim: L")  // NotExact unit
        )

      forAll(conversions){ (unit: PhysicalUnit[_], expected: String) =>
        // Exercise
        val sut = unit.toString
        //Verify
        sut should equal (expected)
      }
    }
  }

  "Equivalence" - {

    "(mm/ms) should be equivalent to (m/s) unlike the equal method" in {
      // Exercise
      val sut = (mm / ms).isEquivalentTo(m / s)
      // Verify
      sut should be(true)
    }

    "isEquivalentTo method should return true if two units have the same dimension and interval" in {
      import org.waman.multiverse.unit.basic.LengthUnitObjects._
      // SetUp
      val conversions =
        Table(
          ("unit", "expected"),
          (micron, true),
          (metre, false),
          (ms, false)
        )
      forAll(conversions) { (unit: LinearUnit[_], expected: Boolean) =>
        // Exercise
        val sut = micrometre.isEquivalentTo(unit)
        // Verify
        sut should equal(expected)
      }
    }
  }

  "Description" in {
    // Exercise
    val sut = VolumeUnitObjects.barrel
    // Verify
    sut.description.length should not equal 0
    sut.description should equal ("oil barrel")

    VolumeUnitObjects.`barrel(US_fl)` should not be a [Description]
    VolumeUnitObjects.millilitre should not be a [Description]
  }
}
