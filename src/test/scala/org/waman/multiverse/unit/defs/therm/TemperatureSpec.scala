package org.waman.multiverse.unit.defs.therm

import org.waman.multiverse.MultiverseCustomSpec
import org.waman.multiverse.implicits._
import org.waman.multiverse.unit.defs.therm.TemperatureUnits._

class TemperatureSpec extends MultiverseCustomSpec {

  "Unit" - {

    "toString method of TemperatureUnit object should return a proper string" in {
      // Exercise
      val conversions =
        Table(
          ("temperature unit", "expected"),
          (K, "kelvin (K), dim: Θ"),
          (degC, "celsius (℃) [0(℃) = 273.15(K), Δ(℃) = Δ(K)], aliases: [°C, degC], dim: Θ"),
          (degF, "fahrenheit (℉) [0(℉) = 45967/180(K), Δ(℉) = 5/9*Δ(K)], aliases: [°F, degF], dim: Θ")
        )
      // Verify
      forAll(conversions){ (sut: TemperatureUnit, expected: String) =>
        sut.toString should equal (expected)
      }
    }
  }

  "Quantity" - {

    "0.0 <<temperature unit>> should be converted to the equivalent value in Kelvin" in {
      // Exercise
      val conversions =
        Table(
          ("temperature", "expected"),
          (0.0(K), 0.0),
          (0.0(degC) , 273.15),
          (0.0(degF), 459.67*5.0/9.0)
        )
      // Verify
      forAll(conversions){ (sut: Temperature[Double], expected: Double) =>
        sut(K) should equal (%%%%(expected))
      }
    }

    "0.0(K) should be converted to the equivalent value in other temperature units" in {
      // SetUp
      val q = 0.0 (K)
      // Exercise
      val conversions =
        Table(
          ("temperature", "expected"),
          (q(K), 0.0),
          (q(degC) , -273.15),
          (q(degF), -459.67)
        )
      // Verify
      forAll(conversions){ (sut: Double, expected: Double) =>
        sut should equal (%%%%(expected))
      }
    }

    "Other boundary values" in {
      // Exercise
      val conversions =
        Table(
          ("temperature", "expected"),
          (0.0(degC)(degF), 32.0),
          (32.0(degF)(degC) , 0.0),
          (100.0(degC)(degF), 212.0),
          (212.0(degF)(degC) , 100.0)
        )
      // Verify
      forAll(conversions){ (sut: Double, expected: Double) =>
        sut should equal (%%%%(expected))
      }
    }

    "Temperature should be implicitly converted to AbsoluteTemperature" in {
      // SetUp
      import org.waman.multiverse.unit.defs.therm.AbsoluteTemperatureUnits.{K => KA, mK => mKA}
      // Exercise
      val conversions =
        Table(
          ("sut", "expected"),
          (3.0(degC)(KA), 276.15),
          (3.0(degC)(mKA), 276150.0)
        )
      forAll(conversions) { (sut: Double, expected: Double) =>
        // Verify
        sut should equal(%%%%(expected))
      }
    }
  }
}
