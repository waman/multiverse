package org.waman.multiverse

import org.scalatest.prop.PropertyChecks
import org.waman.multiverse.UnitSystem._
import scala.language.postfixOps

class AngularVelocitySpec extends MultiverseCustomSpec with PropertyChecks with MKSUnitSystem{

  "Tests where converting from some units to rad/s like 3.0 deg/s => 3.0 * 2 PI / 360 rad/s" in {
    val conversions =
      Table(
        ("length", "expected"),
        (Seq(3.0.`rad/s`, 3.0 `rad/s`, 3.0 (`rad/s`), 3.0.rad/s, 3.0 rad/s, 3.0 (rad/s)) , 3.0),
        (Seq(3.0.`deg/s`, 3.0 `deg/s`, 3.0 (`deg/s`), 3.0.deg/s, 3.0 deg/s, 3.0 (deg/s)), 3.0*Math.PI/180.0),
        (Seq(3.0.deg/min, 3.0 deg/minute, 3.0 (deg/min)), 3.0*Math.PI/180.0/60.0)
      )

    forAll(conversions){ (avs: Seq[AngularVelocity[Double]], expected: Double) =>
      avs.foreach{ av =>
        (av `rad/s`) should equal (%(expected))
      }
    }
  }

  val threeRpS = 3.0.`rad/s`

  "Tests where converting metre unit to other units like 3.0 rad/s => 3.0 * 180.0 / PI deg/s" in {
    val conversions =
      Table(
        ("length", "expected"),
        (Seq(threeRpS.`rad/s`, threeRpS `rad/s`, threeRpS (`rad/s`), threeRpS.rad/s, threeRpS rad/s, threeRpS (rad/s)), 3.0),
        (Seq(threeRpS.`deg/s`, threeRpS `deg/s`, threeRpS (`deg/s`), threeRpS.deg/s, threeRpS deg/s, threeRpS (deg/s)), 3.0*180.0/Math.PI),
        (Seq(threeRpS.deg/min, threeRpS deg/minute, threeRpS (deg/min)), 3.0*180.0/Math.PI*60.0)
      )

    forAll(conversions){ (as: Seq[Double], expected: Double) =>
      as.foreach{ a =>
        a should equal (%(expected))
      }
    }
  }
}
