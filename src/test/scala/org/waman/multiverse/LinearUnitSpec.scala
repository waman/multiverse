package org.waman.multiverse

import org.waman.multiverse.unit.custom.BasicUnits._
import org.waman.multiverse.unit.custom.MechanicalUnits._
import org.waman.multiverse.unit.defs.LengthUnits.a0
      import org.waman.multiverse.unit.defs.LengthUnitObjects.micron

class LinearUnitSpec extends MultiverseCustomSpec{

  "Equality" - {

    "== operator should return the proper value" in {
      val conversions =
        Table(
          ("eval", "expected"),
          (m == m, true),
          (μm == micron, false),
          (m/s == m/s, true),
          (mm/ms == m/s, false)
        )

      forAll(conversions){ (eval: Boolean, expected: Boolean) =>
        //Verify
        assert(eval == expected)
      }
    }

    "hashCode() method should return the proper value" in {
      val conversions =
        Table(
          ("eval", "expected"),
          (m.hashCode == m.hashCode, true),
          (μm.hashCode == micron.hashCode, false),
          ((m/s).hashCode == (m/s).hashCode, true),
          ((mm/ms).hashCode == (m/s).hashCode, false)
        )

      forAll(conversions){ (eval: Boolean, expected: Boolean) =>
        //Verify
        assert(eval == expected)
      }
    }
  }

  "Order" - {

    "Methods of Comparable should return the proper value" in {
      val conversions =
        Table(
          ("eval", "expected"),
          ((mm/ms).compare(m/s) == 0, true),
          (m < km, true),
          (m < mm, false)
        )

      forAll(conversions){ (eval: Boolean, expected: Boolean) =>
        //Verify
        assert(eval == expected)
      }
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
      val sut = (mm/ms) ~= (m/s)
      // Verify
      sut should be(true)
    }

    "isEquivalentTo method should return true if two units have the same dimension and interval" in {
      // SetUp
      val conversions =
        Table(
          ("unit", "expected"),
          (micron, true),
          (m, false),
          (ms, false)
        )
      forAll(conversions) { (unit: LinearUnit[_], expected: Boolean) =>
        // Exercise
        val sut = μm ~= unit
        // Verify
        sut should equal(expected)
      }
    }
  }

  "Description" in {
    import org.waman.multiverse.unit.defs.VolumeUnitObjects
    // Exercise
    val sut = VolumeUnitObjects.barrel
    // Verify
    sut.description.length should not equal 0
    sut.description should equal ("oil barrel")

    VolumeUnitObjects.`barrel(US_fl)` should not be a [Description]
    VolumeUnitObjects.millilitre should not be a [Description]
  }

  "Unit multiplication: kg*(m/s2) = N" in {
    import org.waman.multiverse.unit.custom.MechanicalUnits._
    // Exercise
    val sut = kg * (m/s2)
    // Verify
    (sut ~= N) should be (true)
  }

  "Unit multiplication (reversed): (m/s2) * kg result in a typeless unit" in {
    import org.waman.multiverse.unit.custom.MechanicalUnits._
    import org.waman.multiverse.typeless._
    // Exercise
    val sut = (m/s2) * kg
    // Verify
    sut should be (a [TypelessLinearUnit])
  }

  "Unit division: m/s = mm/ms" in {
    // Exercise
    val sut = m/s
    // Verify
    sut.isEquivalentTo(mm/ms) should be (true)
  }
}
