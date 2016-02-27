package org.waman.multiverse.angle

import org.scalatest.prop.PropertyChecks
import org.waman.multiverse.AbstractQuantityAndUnitSpec
import org.waman.multiverse.UnitSystem._
import spire.implicits._
import spire.math.Real

import scala.language.postfixOps

/**
  * Expected values are from
  * <a href="https://en.wikipedia.org/wiki/Conversion_of_units">Conversion of units</a>.
  */
class AngularVelocitySpec
  extends AbstractQuantityAndUnitSpec[AngularVelocityUnit]
    with PropertyChecks{

  override protected val getUnitClass = classOf[AngularVelocityUnit]

  "AngularVelocityUnit should" - {

    "return an angular velocity value of 1 deg/h in rad per second by 'inRadianPerSecond' property" in {
      __SetUp__
      val avu = deg / h
      __Verify__
      avu.unitInRadianPerSecond should equal(Real.pi / r"180" / r"3600")
    }

    "be evaluated as equal even if different objects" in {
      __Verify__
      (deg / h) should equal(deg / h)
      (deg / h).hashCode should equal((deg / h).hashCode)
    }
  }

  "AngularVelocity object should be converted to a frequency by toFrequency method" in {
    __Exercise__
    val conversions =
      Table(
        ("angular velocities", "expected"),
        (3.0 rad/s, 3.0 / (2.0 * Math.PI)),
        (3.0 rpm, 3.0 / 60.0)
      )
    __Verify__
    forAll(conversions){ (av: AngularVelocity[Double], expected: Double) =>
      av.toFrequency.Hz should equal (%%%%(expected))
    }
  }

  "Predefined angular velocity units" - {

    "3.0 <<angular velocity unit>> should be converted to the equivalent value in metre per second" in {
      __Exercise__
      val conversions =
        Table(
          ("angular velocities", "expected"),
          (Seq(3.0.rad / s, 3.0 rad / s, 3.0 (rad / s)), 3.0),
          (Seq(3.0.deg / s, 3.0 deg / s, 3.0 (deg / s)), 3.0 * Math.PI / 180.0),
          (Seq(3.0.rpm, 3.0 rpm, 3.0 (rpm)), 3.0 * 2.0 * Math.PI / 60.0),
          (Seq(3.0.cps, 3.0 cps, 3.0 (cps)), 3.0 * 2.0 * Math.PI),
          (Seq(3.0.deg / minute, 3.0 deg / minute, 3.0 (deg / minute)), 3.0 * Math.PI / (180.0 * 60.0))
        )
      __Verify__
      forAll(conversions) { (suts: Seq[AngularVelocity[Double]], expected: Double) =>
        suts.foreach { sut =>
          (sut rad / s) should equal(%(expected))
        }
      }
    }

    "3.0 m/s should be converted to the equivalent value in other velocity units" in {
      __SetUp__
      val q = 3.0 (rad / s)
      __Exercise__
      val conversions =
        Table(
          ("angular velocity", "expected"),
          (Seq(q.rad / s, q rad / s, q(rad / s)), 3.0),
          (Seq(q.deg / s, q deg / s, q(deg / s)), 3.0 * 180.0 / Math.PI),
          (Seq(q.rpm, q rpm, q(rpm)), 3.0 * 60.0 / (2.0 * Math.PI)),
          (Seq(q.cps, q cps, q(cps)), 3.0 / (2.0 * Math.PI)),
          (Seq(q.deg / minute, q deg / minute, q(deg / minute)), 3.0 * 180.0 * 60.0 / Math.PI)
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
