package org.waman.multiverse.time

import org.scalatest.prop.PropertyChecks
import org.waman.multiverse.UnitSystem._
import org.waman.multiverse.{MultiverseCustomSpec, UnitSystem}

import scala.language.postfixOps

/**
  * Expected values are from
  * <a href="https://en.wikipedia.org/wiki/Conversion_of_units">Conversion of units</a>.
  */
class TimeSquaredSpec extends MultiverseCustomSpec with PropertyChecks{

  "UnitSystem#getSupportedUnits method should return supported units of time squared" in {
    __SetUp__
    import TimeSquaredUnit._
    __Exercise__
    val result = UnitSystem.getSupportedUnits(classOf[TimeSquaredUnit])
    __Verify__
    result should contain (SecondSquared)
  }

  "Tests where converting from some units to kg like 3.0 g => 3e-3 kg" in {
    val conversions =
      Table(
        ("time squared", "expected"),
        (Seq(3.0.s2, 3.0 s2, 3.0 (s2)), 3.0)
      )

    forAll(conversions){ (sss: Seq[TimeSquared[Double]], expected: Double) =>
      sss.foreach{ ss =>
        (ss s2) should equal (%(expected))
      }
    }
  }

  "Tests where converting a time squared unit to other units like 3.0 kg/m3 => 3e-3 g/cm3" in {
    val value = 3.0 s2

    val conversions =
      Table(
        ("time squared", "expected"),
        (Seq(value.s2, value s2, value (s2)), 3.0)
      )

    forAll(conversions){ (sss: Seq[Double], expected: Double) =>
      sss.foreach{ ss =>
        ss should equal (%(expected))
      }
    }
  }
}
