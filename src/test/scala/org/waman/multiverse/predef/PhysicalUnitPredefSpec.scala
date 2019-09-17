package org.waman.multiverse.predef

import org.waman.multiverse.MultiverseCustomSpec
import org.waman.multiverse.unit.basic.VelocityUnits
import org.waman.multiverse.unit.basic.VelocityUnits._
import org.waman.multiverse.unit.mechanics.AccelerationUnits.g_0
import org.waman.multiverse.unit.mechanics.TimeSquaredUnits.s2
import org.waman.multiverse.unit.MechanicsUnits

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
    sut should contain theSameElementsInOrderAs Seq(s2, g_0)
  }
}
