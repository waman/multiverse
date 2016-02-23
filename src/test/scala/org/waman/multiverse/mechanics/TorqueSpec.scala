package org.waman.multiverse.mechanics

import org.scalatest.prop.PropertyChecks
import org.waman.multiverse.MultiverseCustomSpec
import org.waman.multiverse.UnitSystem._
import spire.implicits._

import scala.language.postfixOps

/**
  * Expected values are from
  * <a href="https://en.wikipedia.org/wiki/Conversion_of_units">Conversion of units</a>.
  */
class TorqueSpec extends MultiverseCustomSpec with PropertyChecks{

  //  "UnitSystem#getSupportedUnits method should return supported units of torque" in {
  //    __SetUp__
  //    import TorqueUnit._
  //    __Exercise__
  //    val result = UnitSystem.getSupportedUnits(classOf[TorqueUnit])
  //    __Verify__
  //    result should contain ()
  //  }

  "TorqueUnit should" - {

    "return a torque value of 1  in Newton metre by 'inNewtonMetre' property" in {
      __SetUp__
      val tu = N*m
      __Verify__
      tu.unitInNewtonMetre should equal (r"1.0")
    }

    "be evaluated as equal even if different objects" in {
      __Verify__
      (N*m) should equal (N*m)
      (N*m).hashCode should equal ((N*m).hashCode)
    }
  }

  "Tests where converting from some units to m/s like 3.0  => 3000.0 N*m" in {
    __Exercise__
    val conversions =
      Table(
        ("torques", "expected"),
        (Seq(3.0.N*m, 3.0 N*m, 3.0 (N*m)), 3.0)
      )
    __Verify__
    forAll(conversions){ (suts: Seq[Torque[Double]], expected: Double) =>
      suts.foreach{ sut =>
        (sut N*m) should equal (%%%%(expected))
      }
    }
  }

  "Tests where converting a torque unit to other units like 3.0 N*m => 3e-3 " in {
    __SetUp__
    val value = 3.0 (N*m)
    __Exercise__
    val conversions =
      Table(
        ("torques", "expected"),
        (Seq(value.N*m, value N*m, value (N*m)), 3.0)
      )
    __Verify__
    forAll(conversions){ (suts: Seq[Double], expected: Double) =>
      suts.foreach{ sut =>
        sut should equal (%%%%(expected))
      }
    }
  }
}
