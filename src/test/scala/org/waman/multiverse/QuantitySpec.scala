package org.waman.multiverse

import org.waman.multiverse.UnitSystem._

class QuantitySpec extends MultiverseCustomSpec{

  "Equality" - {

    "1 (m) should equal 100 (cm)" in {
      import IntegralAsRational._
      __Exercise__
      val result = 1 (m) == 100(cm)
      __Verify__
      result should be (true)
    }

    "1 (m) should not equal 120 (cm)" in {
      import IntegralAsRational._
      __Exercise__
      val result = 1 (m) == 120 (cm)
      __Verify__
      result should be (false)
    }
  }

  "Order" - {

    "1 (m) should be less than 120 (cm)" in {
      import IntegralAsDouble._
      __Exercise__
      val result = 1 (m) < 120 (cm)
      __Verify__
      result should be (true)
    }

    "2 (m) should not be less than 120 (cm)" in {
      import IntegralAsDouble._
      __Exercise__
      val result = 2 (m) < 120 (cm)
      __Verify__
      result should be (false)
    }
  }
}
