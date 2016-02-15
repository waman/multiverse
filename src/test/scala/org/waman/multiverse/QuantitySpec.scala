package org.waman.multiverse

import org.waman.multiverse.UnitSystem._

class QuantitySpec extends MultiverseCustomSpec{

  "Equality" - {

    "Quantities should be evaluated as true even if units are different" in {
      import IntegralAsRational._
      __SetUp__
      val len1 = 1 (m)
      val len2 = 100 (cm)
      __Verify__
      len1 should equal (len2)
    }

//    "Quantities should be comparable even if units are different" in {
//      import IntegralAsDouble._
//      __SetUp__
//      val len1 = 1 (m)
//      val len2 = 120 (cm)
//      __Verify__
//      len1 should be < len2
//    }
  }
}
