package org.waman.multiverse.predef

import org.waman.multiverse.MultiverseCustomSpec
import org.waman.multiverse.predef.basic.VelocityUnits
import org.waman.multiverse.predef.basic.VelocityUnits._
import org.waman.multiverse.predef.mechanics.AccelerationUnits.g_0
import org.waman.multiverse.predef.mechanics.TimeSquaredUnits.`s²`

class PhysicalUnitPredefSpec extends MultiverseCustomSpec{

  "getUnits method of VelocityUnits should return Seq(c, M)" in {
    // Exercise
    val sut = VelocityUnits.getUnits
    // Verify
    sut should contain theSameElementsInOrderAs Seq(c, M)
  }

  "getUnits method of MechanicsUnits should return Seq(s2, g0)" in {
    // Exercise
    val sut = MechanicsUnits.getUnits
    // Verify
    sut should contain theSameElementsInOrderAs Seq(`s²`, g_0)
  }
}
