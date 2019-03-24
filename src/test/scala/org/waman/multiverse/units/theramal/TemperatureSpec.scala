package org.waman.multiverse.units.theramal

import org.waman.multiverse.MultiverseCustomSpec
import org.waman.multiverse.implicits._
import org.waman.multiverse.predef.thermal.TemperatureUnits._
import org.waman.multiverse.units.thermal.{Temperature, TemperatureUnit}

class TemperatureSpec extends MultiverseCustomSpec {

  "symbol property of TemperatureUnit object should return a proper string" in {
    // Exercise
    val conversions =
      Table(
        ("TemperatureUnit", "expected"),
        (K, "K"),
        (`°C`, "°C"),
        (`°F`, "°F")
      )
    // Verify
    forAll(conversions){ (sut: TemperatureUnit, expected: String) =>
      sut.symbol should equal (expected)
    }
  }

  "toString method of TemperatureUnit object should return a proper string" in {
    // Exercise
    val conversions =
      Table(
        ("TemperatureUnit", "expected"),
        (K, "Kelvin (K)"),
        (`°C`, "Celsius (°C) [(°C) = (K) - 5463/20]"),
        (`°F`, "Fahrenheit (°F) [(°F) = 9/5*(K) - 45967/100]")
      )
    // Verify
    forAll(conversions){ (sut: TemperatureUnit, expected: String) =>
      sut.toString should equal (expected)
    }
  }

  "0.0 <<temperature unit>> should be converted to the equivalent value in Kelvin" in {
    // Exercise
    val conversions =
      Table(
        ("temperature", "expected"),
        (0.0(K), 0.0),
        (0.0(`°C`) , 273.15),
        (0.0(`°F`), 459.67*5.0/9.0)
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
        (q(`°C`) , -273.15),
        (q(`°F`), -459.67)
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
        (0.0(`°C`)(`°F`), 32.0),
        (32.0(`°F`)(`°C`) , 0.0),
        (100.0(`°C`)(`°F`), 212.0),
        (212.0(`°F`)(`°C`) , 100.0)
      )
    // Verify
    forAll(conversions){ (sut: Double, expected: Double) =>
      sut should equal (%%%%(expected))
    }
  }
}
