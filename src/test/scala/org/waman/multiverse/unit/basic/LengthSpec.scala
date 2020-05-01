package org.waman.multiverse.unit.basic

import org.waman.multiverse.MultiverseCustomSpec
import org.waman.multiverse.implicits._
import org.waman.multiverse.unit.basic.LengthUnits._
import org.waman.multiverse.unit.basic.MetricAttributes._

class LengthSpec extends MultiverseCustomSpec {

  "Unit" - {

    "Units with attributes" - {

      "The name property should return the proper string" in {
        // Exercise
        val conversions =
          Table(
            ("length", "expected"),
            (xu, "xunit"),
            (xu(CuKα1) , "xunit(CuKα1)"),
            (xu(MoKα1), "xunit(MoKα1)"),
            (ft, "foot"),
            (ft(US), "foot(US)")
          )
        // Verify
        forAll(conversions){ (sut: LengthUnit, expected: String) =>
          sut.name should equal (expected)
        }
      }

      "The symbol property should return the proper string" in {
        // Exercise
        val conversions =
          Table(
            ("length", "expected"),
            (xu, "xu"),
            (xu(CuKα1), "xu(CuKα1)"),
            (xu(MoKα1), "xu(MoKα1)"),
            (ft, "ft"),
            (ft(US), "ft(US)")
          )
        // Verify
        forAll(conversions) { (sut: LengthUnit, expected: String) =>
          sut.symbol should equal(expected)
        }
      }
    }

    "A length unit should have the proper dimension" in {
      // SetUp
      import org.waman.multiverse.DimensionSymbol._
      // Exercise
      val sut = m.dimension
      // Verify
      sut should contain theSameElementsAs Map(L -> 1)
      sut(L) should be (1)
      sut(T) should be (0)
    }
  }

  "Quantity" - {

    "3.0 <<length unit>> should be converted to the equivalent value in metre" in {
      // Exercise
      val conversions =
        Table(
          ("length", "expected"),
          (3.0(mm), 3e-3),
          (3.0(m) , 3.0),
          (3.0(km), 3e3),

          (3.0(xu), 3.0*1.0021e-13),
          (3.0(xu(CuKα1)) , 3.0*1.0020769928e-13),
          (3.0(xu(MoKα1)), 3.0*1.0020995553e-13),
          (3.0(ft), 3.0*12*2.54*0.01),
          (3.0(ft(US)), 3.0*1200.0/3937.0)
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
          (q(km), 3e-3),

          (q(xu), 3.0/1.0021e-13),
          (q(xu(CuKα1)) , 3.0/1.0020769928e-13),
          (q(xu(MoKα1)), 3.0/1.0020995553e-13),
          (q(ft), 3.0/(12*2.54*0.01)),
          (q(ft(US)), 3.0/(1200.0/3937.0))
        )
      // Verify
      forAll(conversions){ (sut: Double, expected: Double) =>
        sut should equal (%%%%(expected))
      }
    }
  }
}
