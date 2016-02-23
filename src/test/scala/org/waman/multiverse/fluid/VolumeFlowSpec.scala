package org.waman.multiverse.fluid

import org.scalatest.prop.PropertyChecks
import org.waman.multiverse.UnitSystem._
import org.waman.multiverse.{MultiverseCustomSpec, UnitSystem}
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
    __Exercise__
    val conversions =
      Table(
        ("volume flows", "expected"),
        (Seq(3.0.m3/s, 3.0 m3/s, 3.0 (m3/s)), 3.0),
        (Seq(3.0.LPM , 3.0 LPM , 3.0 (LPM)) , 3.0 * 1e-3 / 60.0)
      )
    __Verify__
    forAll(conversions){ (suts: Seq[VolumeFlow[Double]], expected: Double) =>
      suts.foreach{ sut =>
        (sut m3/s) should equal (%%%%(expected))
      }
    }
  }

  "Tests where converting metre unit to other units like 3.0 m => 3000.0 mm" in {
    __SetUp__
    val value = 3.0 (m3/s)
    __Exercise__
    val conversions =
      Table(
        ("volume flows", "expected"),
        (Seq(value.m3/s, value m3/s, value (m3/s)), 3.0),
        (Seq(value.LPM , value LPM , value (LPM)) , 3.0 * 60.0 / 1e-3)
      )
    __Verify__
    forAll(conversions){ (suts: Seq[Double], expected: Double) =>
      suts.foreach{ sut =>
        sut should equal (%%%%(expected))
      }
    }
  }
}
