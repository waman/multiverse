package org.waman.multiverse.unit.mechanics

import org.waman.multiverse.MultiverseCustomSpec
import org.waman.multiverse.implicits._
import org.waman.multiverse.unit.mechanics.EnergyUnits._

class EnergySpec extends MultiverseCustomSpec {

  "Quantity" - {

    "3.0 <<energy unit>> should be converted to the equivalent value in Joule" in {
      // Exercise
      val conversions =
        Table(
          ("energy", "expected"),
          (3.0(J), 3.0),
          (3.0(kJ) , 3000.0),
          (3.0(cal) , 3.0*4.1868)
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
          (q(cal), 3.0/4.1868)
        )
      // Verify
      forAll(conversions){ (sut: Double, expected: Double) =>
        sut should equal (%%%%(expected))
      }
    }
  }

  "[SOURCE GENERATION]" - {

    "cal_IT(IT) should not compile" in {
      import EnergyAttributes.IT
      "EnergyUnits.cal_IT" should compile
      "EnergyUnits.cal(IT)" should compile
      "EnergyUnits.cal_IT(IT)" shouldNot compile
    }
  }
}
