package org.waman.multiverse.radiation

import org.scalatest.prop.PropertyChecks
import org.waman.multiverse.UnitSystem._
import org.waman.multiverse.{MultiverseCustomSpec, UnitSystem}

import scala.language.postfixOps

/**
  * Expected values are from
  * <a href="https://en.wikipedia.org/wiki/Conversion_of_units">Conversion of units</a>.
  */
class ExposureSpec extends MultiverseCustomSpec with PropertyChecks{

    "UnitSystem#getSupportedUnits method should return supported units of exposure" in {
      __SetUp__
      import ExposureUnit._
      __Exercise__
      val result = UnitSystem.getSupportedUnits(classOf[ExposureUnit])
      __Verify__
      result should contain (Roentgen)
    }

  "Tests where converting from some units to C like 3.0 mC => 3e-3 C" in {
    __Exercise__
    val conversions =
      Table(
        ("exposures", "expected"),
        (Seq(3.0.C/kg, 3.0 C/kg, 3.0 (C/kg)), 3.0),
        (Seq(3.0.R   , 3.0 R   , 3.0 (R))   , 3.0 * 2.58e-4)
      )
    __Verify__
    forAll(conversions){ (suts: Seq[Exposure[Double]], expected: Double) =>
      suts.foreach{ sut =>
        (sut C/kg) should equal (%%%%(expected))
      }
    }
  }

  "Tests where converting a Coulomb to other units like 3.0 C => 3e3 mC" in {
    __SetUp__
    val value = 3.0 (C/kg)
    __Exercise__
    val conversions =
      Table(
        ("exposures", "expected"),
        (Seq(value.C/kg, value C/kg, value (C/kg)), 3.0),
        (Seq(value.R   , value R   , value (R))   , 3.0 / 2.58e-4)
      )
    __Verify__
    forAll(conversions){ (suts: Seq[Double], expected: Double) =>
      suts.foreach{ sut =>
        sut should equal (%%%%(expected))
      }
    }
  }
}
