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

//  "UnitSystem#getSupportedUnits method should return supported units of kinematic viscosity" in {
//    __SetUp__
//    __Exercise__
//    val result = UnitSystem.getSupportedUnits(classOf[LuminanceUnit])
//    __Verify__
//    result should contain (Stokes)
//  }

//  "LuminanceUnit should" - {
//
//    "return a luminance value of 1 km/h in metre per second by 'inMetrePerSecond' property" in {
//      __SetUp__
//      val vf = L/minute
//      __Verify__
//      vf.unitInCubicMetrePerSecond should equal (r"1e-3" / r"60")
//    }
//
//    "be evaluated as equal even if a different object" in {
//      __Verify__
//      (m3/s) should equal (m3/s)
//      (m3/s).hashCode should equal ((m3/s).hashCode)
//    }
//  }

  "3.0 <<luminance unit>> should be converted to the equivalent value in candera per square metre" in {
    __Exercise__
    val conversions =
      Table(
        ("luminances", "expected"),
        (Seq(3.0.cd/m2, 3.0 cd/m2, 3.0 (cd/m2)), 3.0),
        (Seq(3.0.sb   , 3.0 sb   , 3.0 (sb))   , 3.0 * 1e4)
      )
    __Verify__
    forAll(conversions){ (suts: Seq[Luminance[Double]], expected: Double) =>
      suts.foreach{ sut =>
        (sut cd/m2) should equal (%%%%(expected))
      }
    }
  }

  "3.0 cd/m2 should be converted to the equivalent value in other luminance units" in {
    __SetUp__
    val q = 3.0 (cd/m2)
    __Exercise__
    val conversions =
      Table(
        ("luminances", "expected"),
        (Seq(q.cd/m2, q cd/m2, q (cd/m2)), 3.0),
        (Seq(q.sb   , q sb   , q (sb))   , 3.0 / 1e4)
      )
    __Verify__
    forAll(conversions){ (suts: Seq[Double], expected: Double) =>
      suts.foreach{ sut =>
        sut should equal (%%%%(expected))
      }
    }
  }
}
