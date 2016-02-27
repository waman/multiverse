package org.waman.multiverse.fluid

import org.scalatest.prop.PropertyChecks
import org.waman.multiverse.AbstractQuantityAndUnitSpec
import org.waman.multiverse.UnitSystem._

import scala.language.postfixOps

/**
  * Expected values are from
  * <a href="https://en.wikipedia.org/wiki/Conversion_of_units">Conversion of units</a>.
  */
class DynamicViscositySpec
  extends AbstractQuantityAndUnitSpec[DynamicViscosityUnit]
    with PropertyChecks{

  override protected val getUnitClass = classOf[DynamicViscosityUnit]

//  "DynamicViscosityUnit should" - {
//
//    "return a dynamicViscosity value of 1 km/h in metre per second by 'inMetrePerSecond' property" in {
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

  "3.0 <<dynamic viscosity unit>> should be converted to the equivalent value in Pascal second" in {
    __Exercise__
    val conversions =
      Table(
        ("dynamic viscosities", "expected"),
        (Seq(3.0.Pa*s, 3.0 Pa*s, 3.0 (Pa*s)), 3.0),
        (Seq(3.0.P , 3.0 P , 3.0 (P)) , 3.0 * 0.1)
      )
    __Verify__
    forAll(conversions){ (suts: Seq[DynamicViscosity[Double]], expected: Double) =>
      suts.foreach{ sut =>
        (sut Pa*s) should equal (%%%%(expected))
      }
    }
  }

  "3.0 Pa*s should be converted to the equivalent value in other dynamic viscosity units" in {
    __SetUp__
    val q = 3.0 (Pa*s)
    __Exercise__
    val conversions =
      Table(
        ("dynamic viscosities", "expected"),
        (Seq(q.Pa*s, q Pa*s, q (Pa*s)), 3.0),
        (Seq(q.P   , q P   , q (P))   , 3.0 / 0.1)
      )
    __Verify__
    forAll(conversions){ (suts: Seq[Double], expected: Double) =>
      suts.foreach{ sut =>
        sut should equal (%%%%(expected))
      }
    }
  }
}
