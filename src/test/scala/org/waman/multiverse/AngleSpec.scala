package org.waman.multiverse

import org.scalatest.prop.PropertyChecks
import org.waman.multiverse.UnitSystem._

import scala.language.postfixOps

class AngleSpec extends MultiverseCustomSpec with PropertyChecks with MKSUnitSystem{

  "Tests where converting from some units to rad like 1 deg => pi / 180 rad" in {
    val conversions =
      Table(
        ("length"   , "expectedInMetre"),
        (Seq(1.0.deg, 1.0 deg, 1.0 (deg)), Math.PI / 180.0),
        (Seq(1.0.rad, 1.0 rad, 1.0 (rad)), 1.0)
      )

    forAll(conversions){ (as: Seq[Angle[Double]], expectedInMetre: Double) =>
      as.foreach{ a =>
        (a rad) should equal (%(expectedInMetre))
      }
    }
  }

  val oneRadian = 1.0 rad

  "Tests where converting radian unit to other units like 1.0 rad => 180 / pi deg" in {
    val conversions =
      Table(
        ("length"   , "expected"),
        (Seq(oneRadian.deg, oneRadian deg, oneRadian (deg)), 180.0 / Math.PI),
        (Seq(oneRadian.rad, oneRadian rad, oneRadian (rad)), 1.0)
      )

    forAll(conversions){ (as: Seq[Double], expected: Double) =>
      as.foreach{ a =>
        a should equal (%(expected))
      }
    }
  }

}
