package org.waman.multiverse.electric

import org.scalatest.prop.PropertyChecks
import org.waman.multiverse.UnitSystem._
import org.waman.multiverse.{MultiverseCustomSpec, UnitSystem}

import scala.language.postfixOps

/**
  * Expected values are from
  * <a href="https://en.wikipedia.org/wiki/Conversion_of_units">Conversion of units</a>.
  */
class CapacitanceSpec extends MultiverseCustomSpec with PropertyChecks{

    "UnitSystem#getSupportedUnits method should return supported units of capacitance" in {
      __SetUp__
      import CapacitanceUnit._
      __Exercise__
      val result = UnitSystem.getSupportedUnits(classOf[CapacitanceUnit])
      __Verify__
      result should contain (Farad)
    }

  "Tests where converting from some units to C like 3.0 mC => 3e-3 C" in {
    __Exercise__
    val conversions =
      Table(
        ("capacitances", "expected"),
        (Seq(3.0.F, 3.0 F, 3.0 (F)), 3.0)
      )
    __Verify__
    forAll(conversions){ (suts: Seq[Capacitance[Double]], expected: Double) =>
      suts.foreach{ sut =>
        (sut F) should equal (%%%%(expected))
      }
    }
  }

  "Tests where converting a Coulomb to other units like 3.0 C => 3e3 mC" in {
    __SetUp__
    val value = 3.0 (F)
    __Exercise__
    val conversions =
      Table(
        ("capacitances", "expected"),
        (Seq(value.F, value F, value (F)), 3.0)
      )
    __Verify__
    forAll(conversions){ (suts: Seq[Double], expected: Double) =>
      suts.foreach{ sut =>
        sut should equal (%%%%(expected))
      }
    }
  }
}
