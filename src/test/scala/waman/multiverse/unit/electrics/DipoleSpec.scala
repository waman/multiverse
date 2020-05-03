package waman.multiverse.unit.electrics

import waman.multiverse.MultiverseCustomSpec
import waman.multiverse.implicits._
import waman.multiverse.unit.basic.LengthUnits._
import waman.multiverse.unit.electrics.ChargeUnits._

class DipoleSpec extends MultiverseCustomSpec {

  "Unit" - {

    "Dimension of a product of units should equal the dimension of the corresponding product unit" in {
      // SetUp
      val expected = DipoleUnit.dimension
      // Exercise
      val sut = (C*m).dimension
      // Verify
      sut should contain theSameElementsAs expected
    }
  }

  "Quantity" - {

    "Multipliation between quantities should work well" in {
      // SetUp
      val q = 6(C)
      val x = 3(m)
      val expected = 18(C*m)
      // Exercise
      val sut = q*x
      // Verify
      sut should equal (expected)
    }
  }
}
