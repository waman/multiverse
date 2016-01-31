package org.waman.multiverse

import org.scalatest.prop.PropertyChecks
import org.waman.multiverse.UnitSystem._

import scala.language.postfixOps

/**
  * Expected values are from
  * <a href="https://en.wikipedia.org/wiki/Conversion_of_units">Conversion of units</a>.
  */
class AngleSpec extends MultiverseCustomSpec with PropertyChecks with MKSUnitSystem{

  "UnitSystem#getSupportedUnits method should return supported units of angle" in {
    __SetUp__
    import AngleUnit._
    __Exercise__
    val result = UnitSystem.getSupportedUnits(classOf[AngleUnit])
    __Verify__
    result should contain theSameElementsAs Seq(
      Radian,
      Degree,
      SymbolicDegree,
      Gradian,
      Turn
    )
  }

  "Tests where converting from some units to radian like 3.0 deg => 3.0 * 2 PI / 360 rad" in {
    val conversions =
      Table(
        ("angle", "expected"),
        (Seq(3.0.rad, 3.0 rad, 3.0 (rad)), 3.0),
        (Seq(3.0.deg, 3.0 deg, 3.0 (deg)), 3.0 * 17.453293e-3),
        (Seq(3.0.°  , 3.0 °  , 3.0 (°))  , 3.0 * 17.453293e-3),
        (Seq(3.0.gon, 3.0 gon, 3.0 (gon)), 3.0 * 15.707963e-3),
        (Seq(3.0.tr , 3.0 tr , 3.0 (tr)) , 3.0 * 2.0 * Math.PI)
      )

    forAll(conversions){ (ls: Seq[Angle[Double]], expected: Double) =>
      ls.foreach{ l =>
        (l rad) should equal (%(expected))
      }
    }
  }

  val threeRadian = 3.0 rad

  "Tests where converting radian unit to other units like 3.0 rad => 3.0 * 360 / 2 * PI deg" in {
    val conversions =
      Table(
        ("angle", "expected"),
        (Seq(threeRadian.rad, threeRadian rad, threeRadian (rad)), 3.0),
        (Seq(threeRadian.deg, threeRadian deg, threeRadian (deg)), 3.0 / 17.453293e-3),
        (Seq(threeRadian.°  , threeRadian °  , threeRadian (°))  , 3.0 / 17.453293e-3),
        (Seq(threeRadian.gon, threeRadian gon, threeRadian (gon)), 3.0 / 15.707963e-3),
        (Seq(threeRadian.tr , threeRadian tr , threeRadian (tr)) , 3.0 / (2.0 * Math.PI))
      )

    forAll(conversions){ (as: Seq[Double], expected: Double) =>
      as.foreach{ a =>
        a should equal (%(expected))
      }
    }
  }
}
