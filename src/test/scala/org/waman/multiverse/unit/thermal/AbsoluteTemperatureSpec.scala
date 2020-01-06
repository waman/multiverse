package org.waman.multiverse.unit.thermal

import org.waman.multiverse.MultiverseCustomSpec
import org.waman.multiverse.implicits._
import org.waman.multiverse.unit.thermal.AbsoluteTemperatureUnits._

class AbsoluteTemperatureSpec extends MultiverseCustomSpec {

  "Unit" - {

    "toString method of AbsoluteTemperatureUnit object should return a proper string" in {
      // Exercise
      val conversions =
        Table(
          ("TemperatureUnit", "expected"),
          (K, "kelvin (K), dim: Θ")
        )
      // Verify
      forAll(conversions){ (sut: AbsoluteTemperatureUnit, expected: String) =>
        sut.toString should equal (expected)
      }
    }
  }

  "Quantity" - {

    "3.0 <<absolute temperature unit>> should be converted to the equivalent value in Kelvin" in {
      // Exercise
      val conversions =
        Table(
          ("absolute temperature", "expected"),
          (3.0(K), 3.0),
          (3.0(mK) , 3e-3),
          (3.0(MK), 3e6)
        )
      // Verify
      forAll(conversions){ (sut: AbsoluteTemperature[Double], expected: Double) =>
        sut(K) should equal (%%%%(expected))
      }
    }

    "3.0(K) should be converted to the equivalent value in other absolute temperature units" in {
      // SetUp
      val q = 3.0 (K)
      // Exercise
      val conversions =
        Table(
          ("temperature", "expected"),
          (q(K), 3.0),
          (q(mK) , 3000.0),
          (q(MK), 3e-6)
        )
      // Verify
      forAll(conversions){ (sut: Double, expected: Double) =>
        sut should equal (%%%%(expected))
      }
    }

    "AbsoluteTemperature should be implicitly converted to Temperature" in {
      // SetUp
      import org.waman.multiverse.unit.thermal.TemperatureUnits.degC
      // Exercise
      val conversions =
        Table(
          ("sut", "expected"),
          (3.0(K)(degC), -270.15),
          (3.0(mK)(degC), -273.147)
        )
      forAll(conversions) { (sut: Double, expected: Double) =>
        // Verify
        sut should equal(%%%%(expected))
      }
    }
  }
}
