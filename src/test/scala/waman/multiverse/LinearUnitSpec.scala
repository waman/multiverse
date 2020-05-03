package waman.multiverse

import waman.multiverse.unit.basic.LengthUnits._
import waman.multiverse.unit.basic.TimeUnits._
import waman.multiverse.unit.basic.VolumeUnits._

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

    "(km) should return a string like ''kilometre (km) [1(km) = 1000(m)] ..." in {
      val conversions =
        Table(
          ("length unit", "expected"),
          (mm, "millimetre (mm) [1(mm) = 0.001(m)], dim: L"),
          (m , "metre (m), dim: L"),  // SI unit
          (km, "kilometre (km) [1(km) = 1,000(m)], aliases: [Km], dim: L"),
          (km/s, "kilometre per second (km/s) [1(km/s) = 1,000(m/s)], aliases: [Km/s, km/sec, Km/sec], dim: LT⁻¹"),  // quotient unit
          (a_0, "atomic unit of length (a_0) [1(a_0) ≈ 0.00000000005291772109217(m)], dim: L")  // NotExact unit
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
      val sut = (mm/ms).isEquivalentTo(m/s)
      // Verify
      sut should be (true)
    }

    "isEquivalentTo method should return true if two units have the same dimension and interval" in {
      import waman.multiverse.unit.basic.LengthUnitObjects._
      // SetUp
      val conversions =
      Table(
        ("unit", "expected"),
        (micron, true),
        (metre, false),
        (ms, false)
      )
      forAll(conversions){ (unit: LinearUnit[_], expected: Boolean) =>
        // Exercise
        val sut = micrometre.isEquivalentTo(unit)
        // Verify
        sut should equal (expected)
      }
    }

    "Quotient unit instantiated by / method of LinearUnit should be equivalent to the ordinal quotient unit" in {
      // SetUp
      val v = m/s
      // Exercise
      val unit: LinearUnit[_] = m
      val sut = unit/s
      // Verify
      sut.isInstanceOf[QuotientUnit[_, _]] should be (true)
      sut.isEquivalentTo(v) should be (true)
    }
  }

  "* operator should return the proper product unit" in {
    // Exercise
    val sut = km*h
    // Verify
    sut.name should equal ("kilometre times hour")
    sut.symbol should equal ("km*h")
    sut.getSIUnit should equal (m*s)
  }

  "/ operator should return the proper quotient unit" in {
    import waman.multiverse.unit.basic.VolumeUnits.m3
    // Exercise
    val sut = km/L
    // Verify
    sut.name should equal ("kilometre per litre")
    sut.symbol should equal ("km/L")
    sut.getSIUnit should equal (m/m3)
  }
}
