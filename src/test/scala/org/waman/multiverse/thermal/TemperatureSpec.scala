package org.waman.multiverse.thermal

import org.scalatest.prop.PropertyChecks
import org.waman.multiverse.UnitSystem._
import org.waman.multiverse.{MultiverseCustomSpec, UnitSystem}

import scala.language.postfixOps

/**
  * Expected values are from
  * <a href="https://en.wikipedia.org/wiki/Conversion_of_units">Conversion of units</a>.
  */
class TemperatureSpec extends MultiverseCustomSpec with PropertyChecks{

    "UnitSystem#getSupportedUnits method should return supported units of temperature" in {
      __SetUp__
      import TemperatureUnit._
      __Exercise__
      val result = UnitSystem.getSupportedUnits(classOf[TemperatureUnit])
      __Verify__
      result should contain (Kelvin)
    }

  "Tests where converting from some units to C like 3.0 mC => 3e-3 C" in {
    __Exercise__
    val conversions =
      Table(
        ("temperatures", "expected"),
        (Seq(3.0.K, 3.0 K, 3.0 (K)), 3.0)
      )
    __Verify__
    forAll(conversions){ (suts: Seq[Temperature[Double]], expected: Double) =>
      suts.foreach{ sut =>
        (sut K) should equal (%%%%(expected))
      }
    }
  }

  "Tests where converting a Coulomb to other units like 3.0 C => 3e3 mC" in {
    __SetUp__
    val value = 3.0 (K)
    __Exercise__
    val conversions =
      Table(
        ("temperatures", "expected"),
        (Seq(value.K, value K, value (K)), 3.0)
      )
    __Verify__
    forAll(conversions){ (suts: Seq[Double], expected: Double) =>
      suts.foreach{ sut =>
        sut should equal (%%%%(expected))
      }
    }
  }
}
