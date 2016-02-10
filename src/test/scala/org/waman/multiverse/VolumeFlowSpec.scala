package org.waman.multiverse

import org.scalatest.prop.PropertyChecks
import org.waman.multiverse.UnitSystem._
import spire.implicits._

import scala.language.postfixOps

/**
  * Expected values are from
  * <a href="https://en.wikipedia.org/wiki/Conversion_of_units">Conversion of units</a>.
  */
class VolumeFlowSpec extends MultiverseCustomSpec with PropertyChecks{

  "UnitSystem#getSupportedUnits method should return supported units of volume flow" in {
    __SetUp__
    import VolumeFlowUnit._
    __Exercise__
    val result = UnitSystem.getSupportedUnits(classOf[VolumeFlowUnit])
    __Verify__
    result should contain (LitrePerMinute)
  }

  "VolumeFlowUnit should" - {

    "return a volumeFlow value of 1 km/h in metre per second by 'inMetrePerSecond' property" in {
      __SetUp__
      val vf = L/minute
      __Verify__
      vf.unitInCubicMetrePerSecond should equal (r"1e-3" / r"60")
    }

    "be evaluated as equal even if a different object" in {
      __Verify__
      (m3/s) should equal (m3/s)
      (m3/s).hashCode should equal ((m3/s).hashCode)
    }
  }

  "Tests where converting from some units to m3/s like 3.0 LPM (litre per minute) => 3.0 * 1e-3 / 60.0 m3/s" in {
    val conversions =
      Table(
        ("volumeFlow", "expected"),
        (Seq(3.0.m3/s, 3.0 m3/s, 3.0 (m3/s)), 3.0),
        (Seq(3.0.LPM , 3.0 LPM , 3.0 (LPM)) , 3.0 * 1e-3 / 60.0)
      )

    forAll(conversions){ (vfs: Seq[VolumeFlow[Double]], expected: Double) =>
      vfs.foreach{ vf =>
        (vf m3/s) should equal (%(expected))
      }
    }
  }

  "Tests where converting metre unit to other units like 3.0 m => 3000.0 mm" in {
    val value = 3.0 m3/s

    val conversions =
      Table(
        ("volumeFlow", "expected"),
        (Seq(value.m3/s, value m3/s, value (m3/s)), 3.0),
        (Seq(value.LPM , value LPM , value (LPM)) , 3.0 * 60.0 / 1e-3)
      )

    forAll(conversions){ (vfs: Seq[Double], expected: Double) =>
      vfs.foreach{ vf =>
        vf should equal (%(expected))
      }
    }
  }
}
