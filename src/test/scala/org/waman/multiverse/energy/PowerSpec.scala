package org.waman.multiverse.energy

import org.scalatest.prop.PropertyChecks
import org.waman.multiverse.UnitSystem._
import org.waman.multiverse.{MultiverseCustomSpec, UnitSystem}

import scala.language.postfixOps

/**
  * Expected values are from
  * <a href="https://en.wikipedia.org/wiki/Conversion_of_units">Conversion of units</a>.
  */
class PowerSpec extends MultiverseCustomSpec with PropertyChecks{

  "UnitSystem#getSupportedUnits method should return supported units of power" in {
    __SetUp__
    import PowerUnit._
    __Exercise__
    val result = UnitSystem.getSupportedUnits(classOf[PowerUnit])
    __Verify__
    result should contain (Watt)
  }

  "Tests where converting from some units to kg like 3.0 kW => 3e3 W" in {
    val conversions =
      Table(
        ("power", "expected"),
        (Seq(3.0.W, 3.0 W, 3.0 (W)), 3.0)
      )

    forAll(conversions){ (ps: Seq[Power[Double]], expected: Double) =>
      ps.foreach{ p =>
        (p W) should equal (%(expected))
      }
    }
  }

  "Tests where converting a power unit to other units like 3.0 W => 3e-3 kW" in {
    val value = 3.0 W

    val conversions =
      Table(
        ("power", "expected"),
        (Seq(value.W, value W, value (W)), 3.0)
      )

    forAll(conversions){ (ps: Seq[Double], expected: Double) =>
      ps.foreach{ p =>
        p should equal (%(expected))
      }
    }
  }
}
