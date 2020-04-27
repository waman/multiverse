package org.waman.multiverse.unit.basic

import org.waman.multiverse.MultiverseCustomSpec
import org.waman.multiverse.implicits._
import org.waman.multiverse.unit.basic.LengthUnits._
import org.waman.multiverse.unit.basic.AreaUnits._
import org.waman.multiverse.unit.basic.LengthAttributes._
import org.waman.multiverse.unit.basic.VolumeUnits._
import spire.implicits._

class VolumeSpec extends MultiverseCustomSpec {

  "Unit" - {

    "All of m3, m*m*m, and s.cubic should return equivalent objects" in {
      // Exercise
      val conversions =
        Table(
          ("first", "second", "therd"),
          (m3 , m.cubic, m*m*m),
          (mm3, mm.cubic, mm*mm*mm)
        )
      // Verify
      forAll(conversions){ (first: VolumeUnit, second: VolumeUnit, third: VolumeUnit) =>
        first should equal(second)
        first should equal (third)
      }
    }

    "The name property of volume unit object should return the proper string" in {
      // Exercise
      val conversions =
        Table(
          ("volume unit", "expected"),
          (m3 , "cubic metre"),
          (m.cubic , "cubic metre"),
          (mm3, "cubic millimetre"),
          (mm*mm*mm, "cubic millimetre"),
          (m2*mm, "square metre times millimetre"),

          (gal, "gallon"),
          (gal(US_fl), "gallon(US_fl)"),
          (gal(US_dry), "gallon(US_dry)")
        )
      // Verify
      forAll(conversions){ (sut: VolumeUnit, expected: String) =>
        sut.name should equal (expected)
      }
    }

    "The symbol property of volume unit object should return the proper string" in {
      // Exercise
      val conversions =
        Table(
          ("volume unit", "expected"),
          (m3 , "m³"),
          (m.cubic , "m³"),
          (m.cubic, "m³"),
          (mm*mm*mm, "mm³"),
          (m*mm*nm, "m*mm*nm"),

          (gal, "gal"),
          (gal(US_fl), "gal(US_fl)"),
          (gal(US_dry), "gal(US_dry)")
        )
      // Verify
      forAll(conversions){ (sut: VolumeUnit, expected: String) =>
        sut.symbol should equal (expected)
      }
    }

    "The aliases property of volume unit object should return the proper Seq of String" in {
      // Exercise
      val conversions =
        Table(
          ("volume unit", "expected"),
          (m3 , Seq("m3")),
          (m.cubic, Seq("m.cubic")),
          (mm*mm*mm, Seq("mm.cubic")),
          (m*mm*nm, Nil),

          (gal, Nil),
          (gal(US_fl), Seq("US_gal")),
          (gal(US_dry), Nil)
        )
      // Verify
      forAll(conversions){ (sut: VolumeUnit, expected: Seq[String]) =>
        sut.aliases should contain theSameElementsAs expected
      }
    }

    "Dimension of a cubic of a length unit should equal the dimension of volume unit" in {
      // SetUp
      val expected = VolumeUnit.dimension
      // Exercise
      val sut = ft.cubic.dimension
      // Verify
      sut should contain theSameElementsAs expected
    }
  }

  "Quantity" - {
    val cu_in = 2.54e-2**3

    "3.0 <<volume unit>> should be converted to the equivalent value in metre cubic" in {
      // Exercise
      val conversions =
        Table(
          ("volume", "expected"),
          (3.0(m3) , 3.0),
          (3.0(m*m*m), 3.0),
          (3.0(m.cubic), 3.0),
          (3.0(mm*mm*mm), 3.0*1e-9),
          (3.0(mm.cubic), 3.0*1e-9),
          (3.0(m*mm*nm), 3.0*1e-12),

          (3.0(gal), 3.0*231.0*cu_in),
          (3.0(gal(US_fl)), 3.0*231.0*cu_in),
          (3.0(gal(US_dry)), 3.0*1.0/8.0*2150.42*cu_in)
        )
      // Verify
      forAll(conversions){ (sut: Volume[Double], expected: Double) =>
        sut(m3) should equal (%%%%(expected))
      }
    }

    "3.0(m3) should be converted to the equivalent value in other volume units" in {
      // SetUp
      val q = 3.0 (m3)
      // Exercise
      val conversions =
        Table(
          ("volume", "expected"),
          (q(m3) , 3.0),
          (q(m*m*m), 3.0),
          (q(mm*mm*mm), 3.0*1e9),
          (q(m*mm*nm), 3.0*1e12),

          (q(gal), 3.0/(231.0*cu_in)),
          (q(gal(US_fl)), 3.0/(231.0*cu_in)),
          (q(gal(US_dry)), 3.0/(1.0/8.0*2150.42*cu_in))
        )
      // Verify
      forAll(conversions){ (sut: Double, expected: Double) =>
        sut should equal (%%%%(expected))
      }
    }

    "The cubic method of Length should return the proper Volume object" in {
      // SetUp
      val expected = 27(mm3)
      val x = 3(mm)
      // Exercise
      val sut = x.cubic
      // Verify
      sut should equal (expected)
    }
  }
}
