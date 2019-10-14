package org.waman.multiverse.unit.electric

import spire.implicits._

import org.waman.multiverse.MultiverseCustomSpec
import org.waman.multiverse.unit.electric
import electric.ChargeUnitObjects.statcoulomb
import electric.DipoleUnitObjects.debye
import electric.ChargeUnits.statC
import org.waman.multiverse.unit.basic.LengthUnits.NM

class DipoleSpec extends MultiverseCustomSpec {

  "[SOURCE GENERATION] " - {

    "The unit value of stat should be collect value (the interval string is a product of constant and number)" in {
      // Exercise
      val sut = debye.interval
      // Verify
      sut should equal (r"1e-20" * statcoulomb.interval)
    }

    "Product unit 'statC*NM' should have combinated aliases" in {
      // SetUp
      val expected = Seq("statC*nmi", "Fr*NM", "Fr*nmi", "esu*NM", "esu*nmi")
      // Exercise
      val sut = statC * NM
      // Verify
      sut.aliases should contain theSameElementsAs expected
      sut.symbol should be ("statC*NM")
    }
  }
}
