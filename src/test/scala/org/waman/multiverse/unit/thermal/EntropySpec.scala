package org.waman.multiverse.unit.thermal

import org.waman.multiverse.implicits._
import org.waman.multiverse.MultiverseCustomSpec
import org.waman.multiverse.unit.Constants
import org.waman.multiverse.unit.thermal.EntropyUnits._
import org.waman.multiverse.unit.thermal.AbsoluteTemperatureUnits._
import org.waman.multiverse.unit.mechanics.EnergyUnits._
import spire.implicits._
import spire.math.Real
import java.lang.Math.log

class EntropySpec extends MultiverseCustomSpec {

  val oneByte: Double = 8.0 * log(2.0) * Constants.BoltzmannConstant.toDouble

  "Unit" - {

    "A entropy unit should have the proper dimension" in {
      // SetUp
      import org.waman.multiverse.DimensionSymbol._
      // Exercise
      val sut = bit.dimension
      // Verify
      sut should contain theSameElementsAs Map(M -> 1, L -> 2, T -> -2, Θ -> -1)
      sut(L) should be (2)
      sut(T) should be (-2)
      sut(J) should be (0)
    }

    "The interval of byte should have the correct value" in {
      val sut = EntropyUnitObjects.byte.interval.toDouble
      sut should equal (%%%%(oneByte))
    }
  }

  "Quantity" - {

    "3.0 <<entropy unit>> should be converted to the equivalent value in J/K" in {
      // Exercise
      val conversions =
        Table(
          ("entropy", "expected"),
          (3.0(J/K), 3.0),
          (3.0(B) , 3.0 * oneByte),
          (3.0(kB), 3e3 * oneByte),
          (3.0(GB), 3e9 * oneByte)
        )
      // Verify
      forAll(conversions){ (sut: Entropy[Double], expected: Double) =>
        sut(J/K) should equal (%%%%(expected))
      }
    }

    "3.0(m) should be converted to the equivalent value in other entropy units" in {
      // SetUp
      val q = 3.0 (J/K)
      // Exercise
      val conversions =
        Table(
          ("entropy", "expected"),
          (q(J/K), 3.0),
          (q(B) , 3.0 / oneByte),
          (q(kB), 3e-3 / oneByte),
          (q(GB), 3e-9 / oneByte)
        )
      // Verify
      forAll(conversions){ (sut: Double, expected: Double) =>
        sut should equal (%%%%(expected))
      }
    }
  }

  "[SOURCE GENERATION]" - {

    "nat, bit, and ban should have the proper interval values" in {
      // Exercise
      val conversions =
        Table(
          ("entropy unit", "expected"),
          (nat, Constants.BoltzmannConstant),
          (bit, Real(2).log() * Constants.BoltzmannConstant),
          (ban, Real(10).log() * Constants.BoltzmannConstant)
        )
      // Verify
      forAll(conversions){ (sut: EntropyUnit, expected: Real) =>
        sut.interval should equal (expected)
      }
    }

    "millibyte unit should not be defined (excludePrefixes should work well)" in {
      "EntropyUnitObjects.kilobyte" should compile
      "EntropyUnitObjects.millibyte" shouldNot compile
    }
  }
}