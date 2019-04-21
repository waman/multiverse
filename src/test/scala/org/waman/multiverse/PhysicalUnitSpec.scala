package org.waman.multiverse

import org.waman.multiverse.predef.BasicUnits._
import org.waman.multiverse.predef.basic.LengthUnits.a_0

class PhysicalUnitSpec extends MultiverseCustomSpec{

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

      "(km) should return a string like ''kilometre (km) [1(km) = 1000(m)]" in {
        val conversions =
          Table(
            ("length unit", "expected"),
            (mm, "millimetre (mm) [1(mm) = 1/1000(m)]"),
            (m , "metre (m)"),  // SI unit
            (km, "kilometre (km) [1(km) = 1000(m)]"),
            (km/s, "kilometre per second (km/s) [1(km/s) = 1000(m/s)]"),  // quotient unit
            (a_0, "atomic unit of length (a_0) [1(a_0) ≈ 5291772109217/100000000000000000000000(m)]")  // quotient unit
          )

        forAll(conversions){ (unit: PhysicalUnit[_], expected: String) =>
          // Exercise
          val sut = unit.toString
          //Verify
          sut should equal (expected)
        }
      }
  }
}
