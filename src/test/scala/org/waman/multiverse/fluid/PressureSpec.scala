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

  "3.0 <<pressure unit>> should be converted to the equivalent value in Pascal" in {
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

  "3.0 Pa should be converted to the equivalent value in other pressure units" in {
    __SetUp__
    val q = 3.0 (Pa)
    __Exercise__
    val conversions =
      Table(
        ("pressures", "expected"),
        (Seq(q.Pa, q Pa, q (Pa)), 3.0)
      )
    __Verify__
    forAll(conversions){ (suts: Seq[Double], expected: Double) =>
      suts.foreach{ sut =>
        sut should equal (%%%%(expected))
      }
    }
  }
}
