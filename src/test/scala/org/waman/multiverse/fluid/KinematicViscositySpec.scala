package org.waman.multiverse.fluid

import org.scalatest.prop.PropertyChecks
import org.waman.multiverse.UnitSystem._
import org.waman.multiverse.{MultiverseCustomSpec, UnitSystem}

import scala.language.postfixOps

/**
  * Expected values are from
  * <a href="https://en.wikipedia.org/wiki/Conversion_of_units">Conversion of units</a>.
  */
class KinematicViscositySpec extends MultiverseCustomSpec with PropertyChecks{

  "UnitSystem#getSupportedUnits method should return supported units of kinematic viscosity" in {
    __SetUp__
    import KinematicViscosityUnit._
    __Exercise__
    val result = UnitSystem.getSupportedUnits(classOf[KinematicViscosityUnit])
    __Verify__
    result should contain (Stokes)
  }

//  "KinematicViscosityUnit should" - {
//
//    "return a kinematicViscosity value of 1 km/h in metre per second by 'inMetrePerSecond' property" in {
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
        ("kinematicViscosity", "expected"),
        (Seq(3.0.m2/s, 3.0 m2/s, 3.0 (m2/s)), 3.0),
        (Seq(3.0.St  , 3.0 St  , 3.0 (St))  , 3.0 * 1e-4)
      )

    forAll(conversions){ (kvs: Seq[KinematicViscosity[Double]], expected: Double) =>
      kvs.foreach{ kv =>
        (kv m2/s) should equal (%(expected))
      }
    }
  }

  "Tests where converting m2/s unit to other units like 3.0 m2/s => 3.0 / 1e-4 St" in {
    val value = 3.0 m2/s

    val conversions =
      Table(
        ("kinematicViscosity", "expected"),
        (Seq(value.m2/s, value m2/s, value (m2/s)), 3.0),
        (Seq(value.St  , value St  , value (St))  , 3.0 / 1e-4)
      )

    forAll(conversions){ (kvs: Seq[Double], expected: Double) =>
      kvs.foreach{ kv =>
        kv should equal (%(expected))
      }
    }
  }
}
