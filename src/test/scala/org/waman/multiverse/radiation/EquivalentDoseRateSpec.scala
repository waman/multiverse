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

  "3.0 <<equivalent dose rate unit>> should be converted to the equivalent value in Sievert per second" in {
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

  "3.0 Sv/s should be converted to the equivalent value in other equivalent dose rate units" in {
    __SetUp__
    val q = 3.0 (Sv/s)
    __Exercise__
    val conversions =
      Table(
        ("equivalent dose rates", "expected"),
        (Seq(q.Sv/s, q Sv/s, q (Sv/s)), 3.0)
      )
    __Verify__
    forAll(conversions){ (suts: Seq[Double], expected: Double) =>
      suts.foreach{ sut =>
        sut should equal (%%%%(expected))
      }
    }
  }
}
