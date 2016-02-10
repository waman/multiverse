package org.waman.multiverse

import org.scalatest.prop.PropertyChecks
import org.waman.multiverse.UnitSystem._
import spire.implicits._
import spire.math.Real

import scala.language.postfixOps

/**
  * Expected values are from
  * <a href="https://en.wikipedia.org/wiki/Conversion_of_units">Conversion of units</a>.
  */
class AngularVelocitySpec extends MultiverseCustomSpec with PropertyChecks{

  "UnitSystem#getSupportedUnits method should return supported units of angular velocity" in {
    __SetUp__
    import AngularVelocityUnit._
    __Exercise__
    val result = UnitSystem.getSupportedUnits(classOf[AngularVelocityUnit])
    __Verify__
    result should contain allOf (
      RevolutionPerMinute,
      CyclePerSecond)
  }

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
    val conversions =
      Table(
        ("angularVelocity", "expected"),
        (3.0 rad/s, 3.0 / (2.0 * Math.PI)),
        (3.0 rpm, 3.0 / 60.0)
      )

    forAll(conversions){ (av: AngularVelocity[Double], expected: Double) =>
      av.toFrequency.Hz should equal (%(expected))
    }
  }

  "Tests where converting from some units to rad/s like 3.0 deg/s => 3.0 * 2 PI / 360 rad/s" in {
    val conversions =
      Table(
        ("angularVelocity", "expected"),
        (Seq(3.0.rad/s  , 3.0 rad/s     , 3.0 (rad/s))  , 3.0),
        (Seq(3.0.deg/s  , 3.0 deg/s     , 3.0 (deg/s))  , 3.0 * Math.PI / 180.0),
        (Seq(3.0.rpm    , 3.0 rpm       , 3.0 (rpm))    , 3.0 * 2.0 * Math.PI / 60.0),
        (Seq(3.0.cps    , 3.0 cps       , 3.0 (cps))    , 3.0 * 2.0 * Math.PI),
        (Seq(3.0.deg/minute, 3.0 deg/minute, 3.0 (deg/minute)), 3.0 * Math.PI / 180.0 / 60.0)
      )

    forAll(conversions){ (avs: Seq[AngularVelocity[Double]], expected: Double) =>
      avs.foreach{ av =>
        (av rad/s) should equal (%(expected))
      }
    }
  }

  "Tests where converting metre unit to other units like 3.0 rad/s => 3.0 * 180.0 / PI deg/s" in {
    val value = 3.0 rad/s

    val conversions =
      Table(
        ("angularVelocity", "expected"),
        (Seq(value.rad/s  , value rad/s     , value (rad/s))  , 3.0),
        (Seq(value.deg/s  , value deg/s     , value (deg/s))  , 3.0 * 180.0 / Math.PI),
        (Seq(value.rpm    , value rpm       , value (rpm))    , 3.0 * 60.0 / (2.0 * Math.PI) ),
        (Seq(value.cps    , value cps       , value (cps))    , 3.0 / (2.0 * Math.PI)),
        (Seq(value.deg/minute, value deg/minute, value (deg/minute)), 3.0 * 180.0 * 60.0 / Math.PI)
      )

    forAll(conversions){ (as: Seq[Double], expected: Double) =>
      as.foreach{ a =>
        a should equal (%(expected))
      }
    }
  }
}
