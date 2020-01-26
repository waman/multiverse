package org.waman.multiverse.unitsystem

import org.waman.multiverse.MultiverseCustomSpec

class MKSSpec extends MultiverseCustomSpec{
  import org.waman.multiverse.implicits._
  import org.waman.multiverse.unitsystem.MKS._

  "Length object is implicitly converted to the Double value in metre" in {
    // SetUp
    import org.waman.multiverse.unit.basic.LengthUnits.m
    val expected = 3.0(m)(m)
    // Exercise
    val sut: Double = 3.0(m)
    // Verify
    sut should equal (expected)
  }
}
