package org.waman.multiverse.radiation

import org.scalatest.prop.PropertyChecks
import org.waman.multiverse.MultiverseCustomSpec
import org.waman.multiverse.UnitSystem._

import scala.language.postfixOps

/**
  * Expected values are from
  * <a href="https://en.wikipedia.org/wiki/Conversion_of_units">Conversion of units</a>.
  */
class EquivalentDoseRateSpec extends MultiverseCustomSpec with PropertyChecks{

//  "EquivalentDoseRateUnit should" - {
//
//    "return a equivalentDoseRate value of 1 g/cm3 in kilogram per cubic metre by 'inKilogramPerCubicMetre' property" in {
//      __SetUp__
//      val du = g/cm3
//      __Verify__
//      du.unitInKiloGramPerCubicMetre should equal (r"1e3")
//    }
//
//    "be evaluated as equal even if different objects" in {
//      __Verify__
//      (kg/m3) should equal (kg/m3)
//      (kg/m3).hashCode should equal ((kg/m3).hashCode)
//    }
//  }

  "Tests where converting from some units to m/s like 3.0 g/cm3 => 3000.0 kg/m3" in {
    __Exercise__
    val conversions =
      Table(
        ("equivalent dose rates", "expected"),
        (Seq(3.0.Sv/s, 3.0 Sv/s, 3.0 (Sv/s)), 3.0)
      )
    __Verify__
    forAll(conversions){ (suts: Seq[EquivalentDoseRate[Double]], expected: Double) =>
      suts.foreach{ sut =>
        (sut Sv/s) should equal (%%%%(expected))
      }
    }
  }

  "Tests where converting a equivalentDoseRate unit to other units like 3.0 kg/m3 => 3e-3 g/cm3" in {
    __SetUp__
    val value = 3.0 (Sv/s)
    __Exercise__
    val conversions =
      Table(
        ("equivalent dose rates", "expected"),
        (Seq(value.Sv/s, value Sv/s, value (Sv/s)), 3.0)
      )
    __Verify__
    forAll(conversions){ (suts: Seq[Double], expected: Double) =>
      suts.foreach{ sut =>
        sut should equal (%%%%(expected))
      }
    }
  }
}
