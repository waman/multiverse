package org.waman.multiverse.energy

import org.scalatest.prop.PropertyChecks
import org.waman.multiverse.UnitSystem._
import org.waman.multiverse.{MultiverseCustomSpec, UnitSystem}

import scala.language.postfixOps

/**
  * Expected values are from
  * <a href="https://en.wikipedia.org/wiki/Conversion_of_units">Conversion of units</a>.
  */
class EnergySpec extends MultiverseCustomSpec with PropertyChecks{

  "UnitSystem#getSupportedUnits method should return supported units of energy" in {
    __SetUp__
    import EnergyUnit._
    __Exercise__
    val result = UnitSystem.getSupportedUnits(classOf[EnergyUnit])
    __Verify__
    result should contain allOf (
      Joule,

      ElectronVolt
      )
  }

  "Tests where converting from some units to kg like 3.0 kJ => 3e3 J" in {
    __Exercise__
    val conversions =
      Table(
        ("energies", "expected"),
        (Seq(3.0.J, 3.0 J, 3.0 (J)), 3.0),

        (Seq(3.0.eV, 3.0 eV, 3.0 (eV)), 3.0 * 1.602176565e-19)
      )
    __Verify__
    forAll(conversions){ (suts: Seq[Energy[Double]], expected: Double) =>
      suts.foreach{ sut =>
        (sut J) should equal (%%%%(expected))
      }
    }
  }

  "Tests where converting a energy unit to other units like 3.0 J => 3e-3 kJ" in {
    __SetUp__
    val value = 3.0 (J)
    __Exercise__
    val conversions =
      Table(
        ("energies", "expected"),
        (Seq(value.J, value J, value (J)), 3.0),
        (Seq(value.eV, value eV, value (eV)), 3.0 / 1.602176565e-19)
      )
    __Verify__
    forAll(conversions){ (suts: Seq[Double], expected: Double) =>
      suts.foreach{ sut =>
        sut should equal (%%%%(expected))
      }
    }
  }
}
