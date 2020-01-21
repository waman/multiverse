package org.waman.multiverse

import org.waman.multiverse.unit.thermodynamics.TemperatureUnit
import org.waman.multiverse.unit.thermodynamics.TemperatureUnits._

class HomogeneousUnitSpec extends MultiverseCustomSpec{

  "toString method" - {

    "Temperature units should return the proper string" in {
      val conversions =
        Table(
          ("temperature unit", "expected"),
          (K, "kelvin (K), dim: Θ"),  // SI unit
          (degC , "celsius (°C) [0(°C) = 273.15(K), Δ(°C) = Δ(K)], aliases: [degC, ℃], dim: Θ"),
          (degF, "fahrenheit (°F) [0(°F) = 45967/180(K), Δ(°F) = 5/9*Δ(K)], aliases: [degF, ℉], dim: Θ"),
        )

      forAll(conversions){ (unit: TemperatureUnit, expected: String) =>
        // Exercise
        val sut = unit.toString
        //Verify
        sut should equal (expected)
      }
    }
  }
}
