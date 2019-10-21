package org.waman.multiverse.unit.basic

import org.waman.multiverse.MultiverseCustomSpec
import org.waman.multiverse.implicits._
import org.waman.multiverse.unit.basic.LengthUnits._
import org.waman.multiverse.unit.basic.AreaUnits._

class AreaSpec extends MultiverseCustomSpec {

  "Unit" - {

    "All of m2, m*m, and s.square should return equivalent objects" in {
      m2 should equal (m*m)
      m2 should equal (m.square)

      mm*mm should equal (mm.square)
    }

    "The name property of area unit object should return the proper string" in {
      // Exercise
      val conversions =
        Table(
          ("area unit", "expected"),
          (m2 , "metre squared"),
          (mm*mm, "millimetre squared"),
          (m*mm, "metre times millimetre")
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
          (mm*mm, "mm²"),
          (m*mm, "m*mm")
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
          (m2 , Seq("m2", "m.square", "m*m")),
          (mm*mm, Seq("mm.square", "mm*mm")),
          (m*mm, Nil)
        )
      // Verify
      forAll(conversions){ (sut: AreaUnit, expected: Seq[String]) =>
        sut.aliases should contain theSameElementsAs expected
      }
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
          (3.0(m.square), 3.0),
          (3.0(mm*mm), 3.0*0.000001),
          (3.0(mm.square), 3.0*0.000001),
          (3.0(m*mm), 3.0*0.001)
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
          (q(mm*mm), 3.0*1000000.0),
          (q(m*mm), 3.0*1000.0)
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
      val sut = x.square
      // Verify
      sut should equal (expected)
    }
  }

  "[SOURCE GENERATION]" - {

    "metre_squared should have the proper symbol and some aliases" in {
      // SetUp
      val expected = m.square
      // Exercise
      val conversions =
        Table(
          "area unit",
          AreaUnitObjects.metre_squared,
          m2,
          `m²`
        )
      // Verify
      forAll(conversions){ sut: AreaUnit =>
        sut should equal (expected)
      }
    }

    "micrometre_squared should have the proper symbol and some aliases" in {
      // SetUp
      val expected = mcm.square
      // Exercise
      val conversions =
        Table(
          "area unit",
          AreaUnitObjects.micrometre_squared,
          μm2,
          mcm2,
          `μm²`,
          `mcm²`
        )
      // Verify
      forAll(conversions){ sut: AreaUnit =>
        sut should equal (expected)
      }
    }
  }
}
