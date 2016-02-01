package org.waman.multiverse

import org.scalatest.prop.PropertyChecks
import org.waman.multiverse.UnitSystem._

import scala.language.postfixOps

/**
  * Expected values are from
  * <a href="https://en.wikipedia.org/wiki/Conversion_of_units">Conversion of units</a>.
  */
class SolidAngleSpec extends MultiverseCustomSpec with PropertyChecks with MKSUnitSystem{

  "UnitSystem#getSupportedUnits method should return supported units of solid angle" in {
    __SetUp__
    import SolidAngleUnit._
    __Exercise__
    val result = UnitSystem.getSupportedUnits(classOf[SolidAngleUnit])
    __Verify__
    result should contain theSameElementsAs Seq(
      Steradian,
      SquareDegree
    )
  }

  "Tests where converting from some units to radian like 3.0 deg2 => 3.0 * (2 PI / 360)**2 sr" in {
    val conversions =
      Table(
        ("solidAngle", "expected"),
        (Seq(3.0.sr, 3.0 sr, 3.0 (sr)), 3.0),
        (Seq(3.0.deg2, 3.0 deg2, 3.0 (deg2)), 3.0 * (Math.PI / 180.0) * (Math.PI / 180.0))
      )

    forAll(conversions){ (ls: Seq[SolidAngle[Double]], expected: Double) =>
      ls.foreach{ l =>
        (l sr) should equal (%(expected))
      }
    }
  }

  val three_sr = 3.0 sr

  "Tests where converting radian unit to other units like 3.0 rad => 3.0 * (360 / 2 * PI)**2 deg" in {
    val conversions =
      Table(
        ("solidAngle", "expected"),
        (Seq(three_sr.sr, three_sr sr, three_sr (sr)), 3.0),
        (Seq(three_sr.deg2, three_sr deg2, three_sr (deg2)), 3.0 * (180.0 / Math.PI) * (180.0 / Math.PI))
      )

    forAll(conversions){ (as: Seq[Double], expected: Double) =>
      as.foreach{ a =>
        a should equal (%(expected))
      }
    }
  }
}