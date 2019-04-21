package org.waman.multiverse.units.basic

import org.waman.multiverse.MultiverseCustomSpec
import org.waman.multiverse.implicits._
import org.waman.multiverse.predef.BasicUnits._

class LengthSpec extends MultiverseCustomSpec {

  "3.0 <<length unit>> should be converted to the equivalent value in metre" in {
    // Exercise
    val conversions =
      Table(
        ("length", "expected"),
        (3.0(mm), 3e-3),
        (3.0(m) , 3.0),
        (3.0(km), 3e3)
      )
    // Verify
    forAll(conversions){ (sut: Length[Double], expected: Double) =>
      sut(m) should equal (%%%%(expected))
    }
  }

  "3.0(m) should be converted to the equivalent value in other length units" in {
    // SetUp
    val q = 3.0 (m)
    // Exercise
    val conversions =
      Table(
        ("length", "expected"),
        (q(mm), 3e3),
        (q(m) , 3.0),
        (q(km), 3e-3)
      )
    // Verify
    forAll(conversions){ (sut: Double, expected: Double) =>
      sut should equal (%%%%(expected))
    }
  }

  "Contextful units" - {
    import org.waman.multiverse
    import multiverse.predef.basic.LengthUnits.xu
    import multiverse.predef.basic.XUnitContexts._

    "symbol property should return the proper string" in {
      // Exercise
      val conversions =
        Table(
          ("length", "expected"),
          (xu, "xu"),
          (xu(CuKα1) , "xu(CuKα1)"),
          (xu(MoKα1), "xu(MoKα1)")
        )
      // Verify
      forAll(conversions){ (sut: LengthUnit, expected: String) =>
        sut.symbol should equal (expected)
      }
    }

    "3.0 <<length unit>> should be converted to the equivalent value in metre" in {
      // Exercise
      val conversions =
        Table(
          ("length", "expected"),
          (3.0(xu), 3.0*1.0021e-13),
          (3.0(xu(CuKα1)) , 3.0*1.0020769928e-13),
          (3.0(xu(MoKα1)), 3.0*1.0020995553e-13)
        )
      // Verify
      forAll(conversions){ (sut: Length[Double], expected: Double) =>
        sut(m) should equal (%%%%(expected))
      }
    }
  }
}
