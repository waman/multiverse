package multiverse

import multiverse.unit.thermodynamics.TemperatureUnit
import multiverse.unit.thermodynamics.TemperatureUnits._

class HomogeneousUnitSpec extends MultiverseCustomSpec{

  "toString method" - {

    "Temperature units should return the proper string" in {
      val conversions =
        Table(
          ("temperature unit", "expected"),
          (K, "kelvin (K), dim: Θ"),  // SI unit
          (degC , "celsius (℃) [0(℃) = 273.15(K), Δ(℃) = Δ(K)], aliases: [°C, degC], dim: Θ"),
          (degF, "fahrenheit (℉) [0(℉) = 45967/180(K), Δ(℉) = 5/9*Δ(K)], aliases: [°F, degF], dim: Θ"),
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
