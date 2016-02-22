package org.waman.multiverse.luminous

import org.scalatest.prop.PropertyChecks
import org.waman.multiverse.MultiverseCustomSpec
import org.waman.multiverse.UnitSystem._

import scala.language.postfixOps

/**
  * Expected values are from
  * <a href="https://en.wikipedia.org/wiki/Conversion_of_units">Conversion of units</a>.
  */
class LuminanceSpec extends MultiverseCustomSpec with PropertyChecks{

//  "UnitSystem#getSupportedUnits method should return supported units of kinematic viscosity" in {
//    __SetUp__
//    __Exercise__
//    val result = UnitSystem.getSupportedUnits(classOf[LuminanceUnit])
//    __Verify__
//    result should contain (Stokes)
//  }

//  "LuminanceUnit should" - {
//
//    "return a luminance value of 1 km/h in metre per second by 'inMetrePerSecond' property" in {
//      __SetUp__
//      val vf = L/minute
//      __Verify__
//      vf.unitInCubicMetrePerSecond should equal (r"1e-3" / r"60")
//    }
//
//    "be evaluated as equal even if a different object" in {
//      __Verify__
//      (m3/s) should equal (m3/s)
//      (m3/s).hashCode should equal ((m3/s).hashCode)
//    }
//  }

  "Tests where converting from some units to m2/s like 3.0 St => 3.0 * 10e-4 m2/s" in {
    val conversions =
      Table(
        ("luminance", "expected"),
        (Seq(3.0.cd/m2, 3.0 cd/m2, 3.0 (cd/m2)), 3.0),
        (Seq(3.0.sb   , 3.0 sb   , 3.0 (sb))   , 3.0 * 1e4)
      )

    forAll(conversions){ (ls: Seq[Luminance[Double]], expected: Double) =>
      ls.foreach{ l =>
        (l cd/m2) should equal (%(expected))
      }
    }
  }

  "Tests where converting m2/s unit to other units like 3.0 m2/s => 3.0 / 1e-4 St" in {
    val value = 3.0 cd/m2

    val conversions =
      Table(
        ("luminance", "expected"),
        (Seq(value.cd/m2, value cd/m2, value (cd/m2)), 3.0),
        (Seq(value.sb   , value sb   , value (sb))   , 3.0 / 1e4)
      )

    forAll(conversions){ (ls: Seq[Double], expected: Double) =>
      ls.foreach{ l =>
        l should equal (%(expected))
      }
    }
  }
}
