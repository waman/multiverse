package org.waman.multiverse.unit.defs

import spire.implicits._

import org.waman.multiverse.MultiverseCustomSpec
import org.waman.multiverse.implicits._
import org.waman.multiverse.unit.defs.LengthUnits._
import org.waman.multiverse.unit.defs.AreaUnits._
import org.waman.multiverse.unit.defs.MetricAttributes._

class AreaSpec extends MultiverseCustomSpec {

  "Unit" - {

    "All of m2, m*m, and s.square should return equivalent objects" in {
      // Exercise
      val conversions =
        Table(
          ("first", "second", "therd"),
          (m2 , m.squared, m*m),
          (mm2, mm.squared, mm*mm)
        )
      // Verify
      forAll(conversions){ (first: AreaUnit, second: AreaUnit, third: AreaUnit) =>
        first should equal(second)
        first should equal (third)
      }
    }

    "The name property of area unit object should return the proper string" in {
      // Exercise
      val conversions =
        Table(
          ("area unit", "expected"),
          (m2 , "square metre"),
          (m.squared, "square metre"),
          (mm2 , "square millimetre"),
          (mm.squared, "square millimetre"),
          (m*mm, "metre times millimetre"),

          (ft2, "square foot"),
          (ft2(US), "square foot(US)")
        )
      // Verify
      forAll(conversions){ (sut: AreaUnit, expected: String) =>
        sut.name should equal (expected)
      }
    }

    "The symbol property of area unit object should return the proper string" in {
      // Exercise
      val conversions =
        Table(
          ("area unit", "expected"),
          (m2 , "m²"),
          (m.squared, "m²"),
          (mm*mm, "mm²"),
          (m*mm, "m*mm"),

          (ft2, "ft²"),
          (ft2(US), "ft(US)²")
        )
      // Verify
      forAll(conversions){ (sut: AreaUnit, expected: String) =>
        sut.symbol should equal (expected)
      }
    }

    "The aliases property of area unit object should return the proper Seq of String" in {
      // Exercise
      val conversions =
        Table(
          ("area unit", "expected"),
          (m2 , Seq("m2")),
          (m.squared, Seq("m.squared")),
          (mm*mm, Seq("mm.squared")),
          (m*mm, Nil),

          (ft2, Seq("ft2", "sq_ft")),
          (ft2(US), Seq("ft2(US)", "sq_ft(US)"))
        )
      // Verify
      forAll(conversions){ (sut: AreaUnit, expected: Seq[String]) =>
        sut.aliases should contain theSameElementsAs expected
      }
    }

    "The baseUnit property of square-length area unit should return the proper LengthUnit" in {
      // Exercise
      val sut = in2.baseUnit
      // Verify
      sut.name should be ("inch")
    }

    "Dimension of a square of a length unit should equal the dimension of area unit" in {
      // SetUp
      val expected = AreaUnit.dimension
      // Exercise
      val sut = ft.squared.dimension
      // Verify
      sut should contain theSameElementsAs expected
    }
  }

  "Quantity" - {

    "3.0 <<area unit>> should be converted to the equivalent value in metre squared" in {
      // Exercise
      val conversions =
        Table(
          ("area", "expected"),
          (3.0(m2) , 3.0),
          (3.0(m*m), 3.0),
          (3.0(m.squared), 3.0),
          (3.0(mm*mm), 3.0e-6),
          (3.0(mm.squared), 3.0e-6),
          (3.0(m*mm), 3.0e-3),

          (3.0(ft2), 3.0*((12.0*2.54*0.01)**2)),
          (3.0(ft2(US)), 3.0*((1200.0/3937)**2))
        )
      // Verify
      forAll(conversions){ (sut: Area[Double], expected: Double) =>
        sut(m2) should equal (%%%%(expected))
      }
    }

    "3.0(m2) should be converted to the equivalent value in other area units" in {
      // SetUp
      val q = 3.0 (m2)
      // Exercise
      val conversions =
        Table(
          ("area", "expected"),
          (q(m2) , 3.0),
          (q(m*m), 3.0),
          (q(mm*mm), 3.0e6),
          (q(m*mm), 3.0e3),

          (q(ft2), 3.0/((12.0*2.54*0.01)**2)),
          (q(ft2(US)), 3.0/((1200.0/3937)**2))
        )
      // Verify
      forAll(conversions){ (sut: Double, expected: Double) =>
        sut should equal (%%%%(expected))
      }
    }

    "The square method of Length should return the proper Area object" in {
      // SetUp
      val expected = 9(mm2)
      val x = 3(mm)
      // Exercise
      val sut = x.squared
      // Verify
      sut should equal (expected)
    }
  }
}
