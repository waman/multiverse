package org.waman.multiverse

import org.waman.multiverse.units.LengthUnit
import org.waman.multiverse.predef.LengthUnits._

class PhysicalUnitSpec extends MultiverseCustomSpec{

  "Order" - {

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

      "(m) should return 'm'" in {
        val conversions =
          Table(
            ("length unit", "expected"),
            (mm, "millimetre (mm)"),
            (m , "metre (m)"),
            (km, "kilometre (km)")
          )

        forAll(conversions){ (unit: LengthUnit, expected: String) =>
          // Exercise
          val sut = unit.toString
          //Verify
          sut should equal (expected)
        }
      }
  }
}
