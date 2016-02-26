package org.waman.multiverse.fluid

import org.scalatest.prop.PropertyChecks
import org.waman.multiverse.AbstractQuantityAndUnitSpec
import org.waman.multiverse.UnitSystem._

import scala.language.postfixOps

/**
  * Expected values are from
  * <a href="https://en.wikipedia.org/wiki/Conversion_of_units">Conversion of units</a>.
  */
class PressureSpec
  extends AbstractQuantityAndUnitSpec[PressureUnit]
    with PropertyChecks{

  override protected val getUnitClass = classOf[PressureUnit]

//  "PressureUnit should" - {
//
//    "return a pressure value of 1 km/h in metre per second by 'inMetrePerSecond' property" in {
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

  "Tests where converting from some units to m3/s like 3.0 LPM (litre per minute) => 3.0 * 1e-3 / 60.0 m3/s" in {
    __Exercise__
    val conversions =
      Table(
        ("pressures", "expected"),
        (Seq(3.0.Pa, 3.0 Pa, 3.0 (Pa)), 3.0)
      )
    __Verify__
    forAll(conversions){ (suts: Seq[Pressure[Double]], expected: Double) =>
      suts.foreach{ sut =>
        (sut Pa) should equal (%%%%(expected))
      }
    }
  }

  "Tests where converting metre unit to other units like 3.0 m => 3000.0 mm" in {
    __SetUp__
    val value = 3.0 (Pa)
    __Exercise__
    val conversions =
      Table(
        ("pressures", "expected"),
        (Seq(value.Pa, value Pa, value (Pa)), 3.0)
      )
    __Verify__
    forAll(conversions){ (suts: Seq[Double], expected: Double) =>
      suts.foreach{ sut =>
        sut should equal (%%%%(expected))
      }
    }
  }
}
