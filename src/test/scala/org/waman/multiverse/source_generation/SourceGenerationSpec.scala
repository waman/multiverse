package org.waman.multiverse.source_generation

import org.waman.multiverse.MultiverseCustomSpec
import org.waman.multiverse.unit.basic.{LengthUnitObjects, LengthUnits}

class SourceGenerationSpec extends MultiverseCustomSpec {

  "Optional aliases (alias with parenthesise)" - {

    "Optional aliases should be removed at the aliases property of the case object" in {
      // Exercise
      val sut = LengthUnitObjects.pica
      // Verify
      sut.aliases should not contain "pc"
    }

    "Optional aliases should not be defined on the Units object" in {
      // Exercise
      val sut = LengthUnits.pc
      // Verify
       sut should not be LengthUnitObjects.pica
      // pc is parsec
    }
  }
}
