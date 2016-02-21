package org.waman.multiverse.fluid

import org.scalatest.prop.PropertyChecks
import org.waman.multiverse.UnitSystem._
import org.waman.multiverse.{MultiverseCustomSpec, UnitSystem}

import scala.language.postfixOps

/**
  * Expected values are from
  * <a href="https://en.wikipedia.org/wiki/Conversion_of_units">Conversion of units</a>.
  */
class DynamicViscositySpec extends MultiverseCustomSpec with PropertyChecks{

  "UnitSystem#getSupportedUnits method should return supported units of dynamic viscosity" in {
    __SetUp__
    import DynamicViscosityUnit._
    __Exercise__
    val result = UnitSystem.getSupportedUnits(classOf[DynamicViscosityUnit])
    __Verify__
    result should contain (Poise)
  }

//  "DynamicViscosityUnit should" - {
//
//    "return a dynamicViscosity value of 1 km/h in metre per second by 'inMetrePerSecond' property" in {
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

  "Tests where converting from some units to Pa*s like 3.0 P => 3.0 * 0.1 Pa*s" in {
    val conversions =
      Table(
        ("dynamicViscosity", "expected"),
        (Seq(3.0.Pa*s, 3.0 Pa*s, 3.0 (Pa*s)), 3.0),
        (Seq(3.0.P , 3.0 P , 3.0 (P)) , 3.0 * 0.1)
      )

    forAll(conversions){ (dvs: Seq[DynamicViscosity[Double]], expected: Double) =>
      dvs.foreach{ dv =>
        (dv Pa*s) should equal (%(expected))
      }
    }
  }

  "Tests where converting Pa*s unit to other units like 3.0 Pa*s => 3.0 / 0.1 P" in {
    val value = 3.0 Pa*s

    val conversions =
      Table(
        ("dynamicViscosity", "expected"),
        (Seq(value.Pa*s, value Pa*s, value (Pa*s)), 3.0),
        (Seq(value.P   , value P   , value (P))   , 3.0 / 0.1)
      )

    forAll(conversions){ (dvs: Seq[Double], expected: Double) =>
      dvs.foreach{ dv =>
        dv should equal (%(expected))
      }
    }
  }
}
