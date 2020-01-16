package org.waman.multiverse.unit

import org.waman.multiverse.MultiverseCustomSpec

class UnitdefsPropertiesSpec extends MultiverseCustomSpec {

  "[SOURCE GENERATION]" - {

    "The value of UnitdefsProperties.version should be the style of version number" in {
      // Exercise
      val sut = UnitdefsProperties.version.matches("""\d+\.\d+""")
      // Verify
      sut should equal (true)
    }
  }
}
