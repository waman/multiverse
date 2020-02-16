package org.waman.multiverse.unit.mechanics

import org.waman.multiverse.MultiverseCustomSpec
import org.waman.multiverse.implicits._
import org.waman.multiverse.unit.mechanics.EnergyUnits._
import org.waman.multiverse.unit.mechanics.EnergyAttributes.IT

class EnergySpec extends MultiverseCustomSpec {

  "Quantity" - {

    "3.0 <<energy unit>> should be converted to the equivalent value in Joule" in {
      // Exercise
      val conversions =
        Table(
          ("energy", "expected"),
          (3.0(J), 3.0),
          (3.0(kJ) , 3000.0),
          (3.0(cal) , 3.0*4.184),
          (3.0(cal(IT)), 3.0*4.1868)
        )
      // Verify
      forAll(conversions){ (sut: Energy[Double], expected: Double) =>
        sut(J) should equal (%%%%(expected))
      }
    }

    "3.0(J) should be converted to the equivalent value in other energy units" in {
      // SetUp
      val q = 3.0 (J)
      // Exercise
      val conversions =
        Table(
          ("energy", "expected"),
          (q(J), 3.0),
          (q(kJ) , 0.003),
          (q(cal), 3.0/4.184),
          (q(cal(IT)), 3.0/4.1868)
        )
      // Verify
      forAll(conversions){ (sut: Double, expected: Double) =>
        sut should equal (%%%%(expected))
      }
    }

    "Energy should be implicitly converted to AbsoluteTemperature" in {
      // SetUp
      import org.waman.multiverse.unit.Constants._
      import org.waman.multiverse.unit.thermodynamics.AbsoluteTemperatureUnits._
      // Exercise
      val conversions =
        Table(
          ("sut", "expected"),
          (3.0(J).toAbsoluteTemperature(K), 3.0 / BoltzmannConstant.toDouble),
          (3.0(eV).toAbsoluteTemperature(K), 3.0 * ElementaryCharge.toDouble / BoltzmannConstant.toDouble)
        )
      forAll(conversions){ (sut: Double, expected: Double) =>
        // Verify
        sut should equal (%%%%(expected))
      }
    }
  }
}
