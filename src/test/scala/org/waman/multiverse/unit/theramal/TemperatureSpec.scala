package org.waman.multiverse.unit.theramal

import org.waman.multiverse.MultiverseCustomSpec
import org.waman.multiverse.implicits._
import org.waman.multiverse.unit.thermal.TemperatureUnits._
import org.waman.multiverse.unit.thermal.{Temperature, TemperatureUnit}

class TemperatureSpec extends MultiverseCustomSpec {

  "Unit" - {

    "toString method of TemperatureUnit object should return a proper string" in {
      // Exercise
      val conversions =
        Table(
          ("TemperatureUnit", "expected"),
          (K, "kelvin (K)"),
          (degC, "celsius (°C) [0(°C) = 273.15(K), Δ(°C) = Δ(K)]"),
          (degF, "fahrenheit (°F) [0(°F) = 45967/180(K), Δ(°F) = 5/9*Δ(K)]")
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
  }
}
