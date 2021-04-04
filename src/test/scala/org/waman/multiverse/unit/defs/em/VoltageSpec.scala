package org.waman.multiverse.unit.defs.em

import org.waman.multiverse.implicits._
import org.waman.multiverse.MultiverseCustomSpec
import org.waman.multiverse.unit.defs.em.VoltageUnits._

class VoltageSpec extends MultiverseCustomSpec {

  "Unit" - {

    "A voltage unit should have the proper dimension" in {
      // SetUp
      import org.waman.multiverse.DimensionSymbol._
      // Exercise
      val sut = V.dimension
      // Verify
      sut should contain theSameElementsAs Map(M -> 1, L -> 2, T -> -3, I -> -1)
      sut(J) should be (0)
    }
  }
  
  "Quantity" - {
    
    "3.0 <<voltage unit>> should be converted to the equivalent value in ampare" in {
      // Exercise
      val conversions =
        Table(
          ("voltage", "expected"),
          (3.0(V), 3.0),
          (3.0(abV), 3.0e-8),
          (3.0(statV), 3.0 * 299.792458)
        )
      // Verify
      forAll(conversions) { (sut: Voltage[Double], expected: Double) =>
        sut(V) should equal(%%%%(expected))
      }
    }

    "3.0(A) should be converted to the equivalent value in other voltage units" in {
      // SetUp
      val q = 3.0(V)
      // Exercise
      val conversions =
        Table(
          ("voltage", "expected"),
          (q(V), 3.0),
          (q(abV), 3.0e8),
          (q(statV), 3.0 / 299.792458)
        )
      // Verify
      forAll(conversions) { (sut: Double, expected: Double) =>
        sut should equal(%%%%(expected))
      }
    }
  }
}
