package org.waman.multiverse.mechanics

import org.scalatest.prop.PropertyChecks
import org.waman.multiverse.AbstractQuantityAndUnitSpec
import org.waman.multiverse.UnitSystem._
import spire.implicits._

import scala.language.postfixOps

/**
  * Expected values are from
  * <a href="https://en.wikipedia.org/wiki/Conversion_of_units">Conversion of units</a>.
  */
class VelocitySpec
  extends AbstractQuantityAndUnitSpec[VelocityUnit]
    with PropertyChecks{

  override protected val getUnitClass = classOf[VelocityUnit]

  "VelocityUnit should" - {

    "return a velocity value of 1 km/h in metre per second by 'inMetrePerSecond' property" in {
      __SetUp__
      val vu = km/h
      __Verify__
      vu.unitValueInSIUnit should equal (r"1000" / r"3600")
    }

    "be evaluated as equal even if different objects" in {
      __Verify__
      (km/h) should equal (km/h)
      (km/h).hashCode should equal ((km/h).hashCode)
    }
  }

  "Predefined velocity units" - {

    "3.0 <<velocity unit>> should be converted to the equivalent value in metre per second" in {
      __Exercise__
      val conversions =
        Table(
          ("velocities", "expected"),
          (Seq(3.0.m / s, 3.0 m / s, 3.0 (m / s)), 3.0),
          (Seq(3.0.km / h, 3.0 km / h, 3.0 (km / h)), 3.0 * 1000.0 / 3600.0),
          (Seq(3.0.c, 3.0 c, 3.0 (c)), 3.0 * 299792458),
          (Seq(3.0.km / minute, 3.0 km / minute, 3.0 (km / minute)), 3.0 * 1000.0 / 60.0),
          (Seq(3.0.cm / minute, 3.0 cm / minute, 3.0 (cm / minute)), 3.0 * 0.01 / 60.0)
        )
      __Verify__
      forAll(conversions) { (suts: Seq[Velocity[Double]], expected: Double) =>
        suts.foreach { sut =>
          (sut m / s) should equal(%%%%(expected))
        }
      }
    }

    "3.0 m/s should be converted to the equivalent value in other velocity units" in {
      __SetUp__
      val q = 3.0 (m / s)
      __Exercise__
      val conversions =
        Table(
          ("velocities", "expected"),
          (Seq(q.m / s, q m / s, q(m / s)), 3.0),
          (Seq(q.km / h, q km / h, q(km / h)), 3.0 * 3600.0 / 1000.0),
          (Seq(q.c, q c, q(c)), 3.0 / 299792458),
          (Seq(q.km / minute, q km / minute, q(km / minute)), 3.0 * 60.0 / 1000.0),
          (Seq(q.cm / minute, q cm / minute, q(cm / minute)), 3.0 * 60.0 / 0.01)
        )
      __Verify__
      forAll(conversions) { (suts: Seq[Double], expected: Double) =>
        suts.foreach { sut =>
          sut should equal(%%%%(expected))
        }
      }
    }
  }
}
