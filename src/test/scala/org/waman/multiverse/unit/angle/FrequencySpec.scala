package org.waman.multiverse.unit.angle

import org.waman.multiverse.MultiverseCustomSpec
import org.waman.multiverse.implicits._
import org.waman.multiverse.unit.angle.FrequencyUnits._

class FrequencySpec extends MultiverseCustomSpec {

  "Quantity" - {

    "3.0 <<frequency unit>> should be converted to the equivalent value in radian" in {
      // Exercise
      val conversions =
        Table(
          ("frequency", "expected"),
          (3.0(Hz), 3.0),
          (3.0(kHz) , 3000.0),
          (3.0(GHz), 3.0e9)
        )
      // Verify
      forAll(conversions){ (sut: Frequency[Double], expected: Double) =>
        sut(Hz) should equal (%%%%(expected))
      }
    }

    "3.0(Hz) should be converted to the equivalent value in other frequency units" in {
      // SetUp
      val q = 3.0 (Hz)
      // Exercise
      val conversions =
        Table(
          ("frequency", "expected"),
          (q(Hz), 3.0),
          (q(kHz) , 0.003),
          (q(GHz), 3.0e-9)
        )
      // Verify
      forAll(conversions){ (sut: Double, expected: Double) =>
        sut should equal (%%%%(expected))
      }
    }

    "Frequency should be implicitly converted to AngularVelocity" in {
      // SetUp
      import org.waman.multiverse.unit.angle.AngleUnits._
      import org.waman.multiverse.unit.basic.TimeUnits._
      // Exercise
      val conversions =
        Table(
          ("sut", "expected"),
          (3.0(Hz)(rad/s), 3.0 * 2.0 * Math.PI)
        )
      forAll(conversions){ (sut: Double, expected: Double) =>
        // Verify
        sut should equal (%%%%(expected))
      }
    }
  }
}
