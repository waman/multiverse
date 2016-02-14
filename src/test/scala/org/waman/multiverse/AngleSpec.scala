package org.waman.multiverse

import org.scalatest.prop.PropertyChecks
import org.waman.multiverse.UnitSystem._

import scala.language.postfixOps

/**
  * Expected values are from
  * <a href="https://en.wikipedia.org/wiki/Conversion_of_units">Conversion of units</a>.
  */
class AngleSpec extends MultiverseCustomSpec with PropertyChecks{

  "UnitSystem#getSupportedUnits method should return supported units of angle" in {
    __SetUp__
    import AngleUnit._
    __Exercise__
    val result = UnitSystem.getSupportedUnits(classOf[AngleUnit])
    __Verify__
    result should contain theSameElementsAs Seq(
      Radian,
      Degree,
      Gradian,
      Turn
    )
  }

  "Tests where converting from some units to radian like 3.0 deg => 3.0 * 2 PI / 360 rad" in {
    val conversions =
      Table(
        ("angle", "expected"),
        (Seq(3.0.rad, 3.0 rad, 3.0 (rad)), 3.0),
        (Seq(3.0.deg, 3.0 deg, 3.0 (deg)), 3.0 * Math.PI / 180.0),
        (Seq(3.0.°  , 3.0 °  , 3.0 (°))  , 3.0 * Math.PI / 180.0),
        (Seq(3.0.gon, 3.0 gon, 3.0 (gon)), 3.0 * 2.0 * Math.PI / 400.0),
        (Seq(3.0.ᵍ  , 3.0 ᵍ  , 3.0 (ᵍ))   , 3.0 * 2.0 * Math.PI / 400.0),
        (Seq(3.0.tr , 3.0 tr , 3.0 (tr)) , 3.0 * 2.0 * Math.PI)
      )

    forAll(conversions){ (ls: Seq[Angle[Double]], expected: Double) =>
      ls.foreach{ l =>
        (l rad) should equal (%(expected))
      }
    }
  }

  "Tests where converting radian unit to other units like 3.0 rad => 3.0 * 360 / 2 * PI deg" in {
    val value = 3.0 rad

    val conversions =
      Table(
        ("angle", "expected"),
        (Seq(value.rad, value rad, value (rad)), 3.0),
        (Seq(value.deg, value deg, value (deg)), 3.0 * 180.0 / Math.PI),
        (Seq(value.°  , value °  , value (°))  , 3.0 * 180.0 / Math.PI),
        (Seq(value.gon, value gon, value (gon)), 3.0 * 400.0 / (2.0 * Math.PI)),
        (Seq(value.ᵍ  , value ᵍ   , value (ᵍ))  , 3.0 * 400.0 / (2.0 * Math.PI)),
        (Seq(value.tr , value tr , value (tr)) , 3.0 / (2.0 * Math.PI))
      )

    forAll(conversions){ (as: Seq[Double], expected: Double) =>
      as.foreach{ a =>
        a should equal (%(expected))
      }
    }
  }
}
