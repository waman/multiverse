package org.waman.multiverse.luminous

import org.scalatest.prop.PropertyChecks
import org.waman.multiverse.AbstractQuantityAndUnitSpec
import org.waman.multiverse.UnitSystem._

import scala.language.postfixOps

/**
  * Expected values are from
  * <a href="https://en.wikipedia.org/wiki/Conversion_of_units">Conversion of units</a>.
  */
class LuminanceSpec
  extends AbstractQuantityAndUnitSpec[LuminanceUnit]
    with PropertyChecks{

  override protected val getUnitClass = classOf[LuminanceUnit]

  "Predefined luminance units" - {
    "3.0 <<luminance unit>> should be converted to the equivalent value in candera per square metre" in {
      __Exercise__
      val conversions =
        Table(
          ("luminances", "expected"),
          (Seq(3.0.cd/m2, 3.0 cd/m2, 3.0 (cd/m2)), 3.0),
          (Seq(3.0.sb , 3.0 sb , 3.0 (sb)) , 3.0 * 1e4),
          (Seq(3.0.Lb , 3.0 Lb , 3.0 (Lb)) , 3.0 * 3183.0988628),
          (Seq(3.0.asb, 3.0 asb, 3.0 (asb)), 3.0 / Math.PI),
          (Seq(3.0.sk , 3.0 sk , 3.0 (sk)) , 3.0 * 1e-3 / Math.PI),
          (Seq(3.0.bril , 3.0 bril , 3.0 (bril)) , 3.0 * 1e-7 / Math.PI),
          (Seq(3.0.fLb, 3.0 fLb, 3.0 (fLb)), 3.0 * 3.4262590996)
        )
      __Verify__
      forAll(conversions) { (suts: Seq[Luminance[Double]], expected: Double) =>
        suts.foreach { sut =>
          (sut cd / m2) should equal(%%%%(expected))
        }
      }
    }

    "3.0 cd/m2 should be converted to the equivalent value in other luminance units" in {
      __SetUp__
      val q = 3.0 (cd / m2)
      __Exercise__
      val conversions =
        Table(
          ("luminances", "expected"),
          (Seq(q.cd/m2, q cd/m2, q (cd/m2)), 3.0),
          (Seq(q.sb  , q sb  , q (sb))  , 3.0 / 1e4),
          (Seq(q.Lb  , q Lb  , q (Lb))  , 3.0 / 3183.0988628),
          (Seq(q.asb, q asb  , q (asb)) , 3.0 * Math.PI),
          (Seq(q.sk  , q sk  , q (sk))  , 3.0 * 1e3 * Math.PI),
          (Seq(q.bril, q bril, q (bril)), 3.0 * 1e7* Math.PI),
          (Seq(q.fLb , q fLb , q (fLb)) , 3.0 / 3.4262590996)
        )
      __Verify__
      forAll(conversions) { (suts: Seq[Double], expected: Double) =>
        suts.foreach { sut =>
          sut should equal(%%%%(expected))
        }
      }
    }
  }

  "Quotient luminance unit" - {

    "Luminance unit of cd/in2 should equal 1550.0031 cd/m2" in {
      __Exercise__
      val sut = cd/in2
      __Verify__
      sut.unitValueInSIUnit.toDouble should equal (%%%%(1550.0031))
    }

    "3.0 lm/in2 should equal 3.0 * 1550.0031 lx" in {
      __Exercise__
      val conversions =
        Table(
          ("illuminance", "expected"),
          (3.0.cd/in2, 3.0 * 1550.0031),
          (3.0 cd/in2, 3.0 * 1550.0031),
          (3.0 (cd/in2), 3.0 * 1550.0031)
        )
      __Verify__
      forAll(conversions){ (sut: Luminance[Double], expected: Double) =>
        sut.cd/m2 should equal (%%%%(expected))
      }
    }

    "3.0 cd/m2 should equal 3.0 / 1550.0031 cd/in2" in {
      __SetUp__
      val q = 3.0 (cd/m2)
      val expected = 3.0 / 1550.0031
      __Exercise__
      val conversions =
        Table(
          ("Luminance", "expected"),
          (q.cd/in2, expected),
          (q cd/in2, expected),
          (q (cd/in2), expected)
        )
      __Verify__
      forAll(conversions){ (sut: Double, expected: Double) =>
        sut should equal (%%%%(expected))
      }
    }
  }
}
