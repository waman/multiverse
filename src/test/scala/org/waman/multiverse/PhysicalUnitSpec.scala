package org.waman.multiverse

import org.waman.multiverse.angle.AngleUnit
import org.waman.multiverse.metric.LengthUnit

class PhysicalUnitSpec extends MultiverseCustomSpec{

  "'name' property should return the name of the unit" in {
    __Verify__
    LengthUnit.KiloMetre.name should equal ("KiloMetre")
  }

  "'symbol' property should return the symbol(s) of the unit" in {
    __Verify__
    AngleUnit.Degree.symbols should contain theSameElementsAs Set("deg", "°")
  }

  "toString method should return the String expression of the unit" in {
    __Verify__
    LengthUnit.KiloMetre.toString should equal ("KiloMetre (km)")
  }

  "Order" - {

    "Metre should be less than KiloMetre" in {
      __Exercise__
      val result = LengthUnit.Metre < LengthUnit.KiloMetre
      __Verify__
      result should be (true)
    }

    "Metre should not be less than CentiMetre" in {
      __Exercise__
      val result = LengthUnit.Metre < LengthUnit.CentiMetre
      __Verify__
      result should be (false)
    }
  }
}
