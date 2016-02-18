package org.waman.multiverse.mechanics

import org.scalatest.prop.PropertyChecks
import org.waman.multiverse.MultiverseCustomSpec
import org.waman.multiverse.UnitSystem._

import scala.language.postfixOps

/**
  * Expected values are from
  * <a href="https://en.wikipedia.org/wiki/Conversion_of_units">Conversion of units</a>.
  */
class AccelerationSpec extends MultiverseCustomSpec with PropertyChecks{

  //  "UnitSystem#getSupportedUnits method should return supported units of acceleration" in {
  //    __SetUp__
  //    import AccelerationUnit._
  //    __Exercise__
  //    val result = UnitSystem.getSupportedUnits(classOf[AccelerationUnit])
  //    __Verify__
  //    result should contain ()
  //  }

  "Tests where converting from some units to m/s2 like 3.0 g0 => 3.0 * 9.80665 m/s2" in {
    val conversions =
      Table(
        ("acceleration", "expected"),
        (Seq(3.0.m/s2, 3.0 m/s2, 3.0 (m/s2)), 3.0),
        (Seq(3.0.g0, 3.0 g0, 3.0 (g0)), 3.0 * 9.80665)
      )

    forAll(conversions){ (accels: Seq[Acceleration[Double]], expected: Double) =>
      accels.foreach{ accel =>
        (accel m/s2) should equal (%(expected))
      }
    }
  }

  "Tests where converting a acceleration unit to other units like 3.0 m/s2 => 3.0 / 9.80665 g0" in {
    val value = 3.0 m/s2

    val conversions =
      Table(
        ("acceleration", "expected"),
        (Seq(value.m/s2, value m/s2, value (m/s2)), 3.0),
        (Seq(value.g0 , value g0 , value (g0)) , 3.0 / 9.80665)
      )

    forAll(conversions){ (accels: Seq[Double], expected: Double) =>
      accels.foreach{ accel =>
        accel should equal (%(expected))
      }
    }
  }
}
