package org.waman.multiverse.mass

import org.scalatest.prop.PropertyChecks
import org.waman.multiverse.MultiverseCustomSpec
import org.waman.multiverse.UnitSystem._
import spire.implicits._

import scala.language.postfixOps

/**
  * Expected values are from
  * <a href="https://en.wikipedia.org/wiki/Conversion_of_units">Conversion of units</a>.
  */
class DensitySpec
  extends MultiverseCustomSpec
    with PropertyChecks{

  "DensityUnit should" - {

    "return a density value of 1 g/cm3 in kilogram per cubic metre by 'inKilogramPerCubicMetre' property" in {
      __SetUp__
      val du = g/cm3
      __Verify__
      du.unitValueInSIUnit should equal (r"1e3")
    }

    "be evaluated as equal even if different objects" in {
      __Verify__
      (kg/m3) should equal (kg/m3)
      (kg/m3).hashCode should equal ((kg/m3).hashCode)
    }
  }

  "Predefined density units" - {

    "3.0 <<density unit>> should be converted to the equivalent value in kilogram per cubic metre" in {
      __Exercise__
      val conversions =
        Table(
          ("densities", "expected"),
          (Seq(3.0.g / cm3, 3.0 g / cm3, 3.0 (g / cm3)), 3e3),
          (Seq(3.0.g / mL, 3.0 g / mL, 3.0 (g / mL)), 3e3),
          (Seq(3.0.kg / m3, 3.0 kg / m3, 3.0 (kg / m3)), 3.0),
          (Seq(3.0.kg / L, 3.0 kg / L, 3.0 (kg / L)), 3e3)
        )
      __Verify__
      forAll(conversions) { (suts: Seq[Density[Double]], expected: Double) =>
        suts.foreach { sut =>
          (sut kg / m3) should equal(%%%%(expected))
        }
      }
    }

    "3.0 kg/m3 should be converted to the equivalent value in other density units" in {
      __SetUp__
      val q = 3.0 (kg / m3)
      __Exercise__
      val conversions =
        Table(
          ("densities", "expected"),
          (Seq(q.g / cm3, q g / cm3, q(g / cm3)), 3e-3),
          (Seq(q.g / mL, q g / mL, q(g / mL)), 3e-3),
          (Seq(q.kg / m3, q kg / m3, q(kg / m3)), 3.0),
          (Seq(q.kg / L, q kg / L, q(kg / L)), 3e-3)
        )
      __Verify__
      forAll(conversions) { (suts: Seq[Double], expected: Double) =>
        suts.foreach { sut =>
          sut should equal(%%%%(expected))
        }
      }
    }
  }
}
