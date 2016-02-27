package org.waman.multiverse.fluid

import org.scalatest.prop.PropertyChecks
import org.waman.multiverse.AbstractQuantityAndUnitSpec
import org.waman.multiverse.UnitSystem._

import scala.language.postfixOps

/**
  * Expected values are from
  * <a href="https://en.wikipedia.org/wiki/Conversion_of_units">Conversion of units</a>.
  */
class KinematicViscositySpec
  extends AbstractQuantityAndUnitSpec[KinematicViscosityUnit]
    with PropertyChecks{

  override protected val getUnitClass = classOf[KinematicViscosityUnit]

//  "KinematicViscosityUnit should" - {
//
//    "return a kinematicViscosity value of 1 km/h in metre per second by 'inMetrePerSecond' property" in {
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

  "3.0 <<kinematic viscosity unit>> should be converted to the equivalent value in square metre per second" in {
    __Exercise__
    val conversions =
      Table(
        ("kinematic viscosities", "expected"),
        (Seq(3.0.m2/s, 3.0 m2/s, 3.0 (m2/s)), 3.0),
        (Seq(3.0.St  , 3.0 St  , 3.0 (St))  , 3.0 * 1e-4)
      )
    __Verify__
    forAll(conversions){ (suts: Seq[KinematicViscosity[Double]], expected: Double) =>
      suts.foreach{ sut =>
        (sut m2/s) should equal (%%%%(expected))
      }
    }
  }

  "3.0 m2/s should be converted to the equivalent value in other kinematic viscosity units" in {
    __SetUp__
    val q = 3.0 (m2/s)
    __Exercise__
    val conversions =
      Table(
        ("kinematic viscosities", "expected"),
        (Seq(q.m2/s, q m2/s, q (m2/s)), 3.0),
        (Seq(q.St  , q St  , q (St))  , 3.0 / 1e-4)
      )
    __Verify__
    forAll(conversions){ (suts: Seq[Double], expected: Double) =>
      suts.foreach{ sut =>
        sut should equal (%%%%(expected))
      }
    }
  }
}
