package org.waman.multiverse.mechanics

import org.scalatest.prop.PropertyChecks
import org.waman.multiverse.AbstractQuantityAndUnitSpec
import org.waman.multiverse.UnitSystem._

import scala.language.postfixOps

/**
  * Expected values are from
  * <a href="https://en.wikipedia.org/wiki/Conversion_of_units">Conversion of units</a>.
  */
class AccelerationSpec
  extends AbstractQuantityAndUnitSpec[AccelerationUnit]
    with PropertyChecks{

  override protected val getUnitClass = classOf[AccelerationUnit]

  "3.0 <<acceleration unit>> should be converted to the equivalent value in metre per second squared" in {
    __Exercise__
    val conversions =
      Table(
        ("accelerations", "expected"),
        (Seq(3.0.m/s2, 3.0 m/s2, 3.0 (m/s2)), 3.0),
        (Seq(3.0.g0, 3.0 g0, 3.0 (g0)), 3.0 * 9.80665)
      )
    __Verify__
    forAll(conversions){ (suts: Seq[Acceleration[Double]], expected: Double) =>
      suts.foreach{ sut =>
        (sut m/s2) should equal (%%%%(expected))
      }
    }
  }

  "3.0 m/s2 should be converted to the equivalent value in other acceleration units" in {
    __SetUp__
    val q = 3.0 (m/s2)
    __Exercise__
    val conversions =
      Table(
        ("accelerations", "expected"),
        (Seq(q.m/s2, q m/s2, q (m/s2)), 3.0),
        (Seq(q.g0 , q g0 , q (g0)) , 3.0 / 9.80665)
      )
    __Verify__
    forAll(conversions){ (suts: Seq[Double], expected: Double) =>
      suts.foreach{ sut =>
        sut should equal (%%%%(expected))
      }
    }
  }
}
