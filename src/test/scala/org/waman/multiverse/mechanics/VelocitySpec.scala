package org.waman.multiverse.mechanics

import org.scalatest.prop.PropertyChecks
import org.waman.multiverse.UnitSystem._
import org.waman.multiverse.{MultiverseCustomSpec, UnitSystem}
import spire.implicits._

import scala.language.postfixOps

/**
  * Expected values are from
  * <a href="https://en.wikipedia.org/wiki/Conversion_of_units">Conversion of units</a>.
  */
class VelocitySpec extends MultiverseCustomSpec with PropertyChecks{

  "UnitSystem#getSupportedUnits method should return supported units of velocity" in {
    __SetUp__
    import VelocityUnit._
    __Exercise__
    val result = UnitSystem.getSupportedUnits(classOf[VelocityUnit])
    __Verify__
    result should contain (SpeedOfLight)
  }

  "VelocityUnit should" - {

    "return a velocity value of 1 km/h in metre per second by 'inMetrePerSecond' property" in {
      __SetUp__
      val vu = km/h
      __Verify__
      vu.unitInMetrePerSecond should equal (r"1000" / r"3600")
    }

    "be evaluated as equal even if different objects" in {
      __Verify__
      (km/h) should equal (km/h)
      (km/h).hashCode should equal ((km/h).hashCode)
    }
  }

  "Tests where converting from some units to m/s like 3.0 km/h => 3.0 * 1000.0/3600.0 m/s" in {
    val conversions =
      Table(
        ("velocity", "expected"),
        (Seq(3.0.m/s   , 3.0 m/s      , 3.0 (m/s))   , 3.0),
        (Seq(3.0.km/h  , 3.0 km/h     , 3.0 (km/h))  , 3.0 * 1000.0 / 3600.0),
        (Seq(3.0.c     , 3.0 c        , 3.0 (c))     , 3.0 * 299792458),
        (Seq(3.0.km/minute, 3.0 km/minute, 3.0 (km/minute)), 3.0 * 1000.0 / 60.0),
        (Seq(3.0.cm/minute, 3.0 cm/minute, 3.0 (cm/minute)), 3.0 * 0.01 / 60.0)
      )

    forAll(conversions){ (vs: Seq[Velocity[Double]], expected: Double) =>
      vs.foreach{ v =>
        (v m/s) should equal (%(expected))
      }
    }
  }

  "Tests where converting metre unit to other units like 3.0 m => 3000.0 mm" in {
    val value = 3.0 m/s

    val conversions =
      Table(
        ("velocity", "expected"),
        (Seq(value.m/s   , value m/s      , value (m/s))   , 3.0),
        (Seq(value.km/h  , value km/h     , value (km/h))  , 3.0 * 3600.0 / 1000.0),
        (Seq(value.c     , value c        , value (c))     , 3.0 / 299792458),
        (Seq(value.km/minute, value km/minute, value (km/minute)), 3.0 * 60.0 / 1000.0),
        (Seq(value.cm/minute, value cm/minute, value (cm/minute)), 3.0 * 60.0 / 0.01)
      )

    forAll(conversions){ (vs: Seq[Double], expected: Double) =>
      vs.foreach{ v =>
        v should equal (%(expected))
      }
    }
  }
}
