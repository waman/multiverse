package org.waman.multiverse

import org.waman.multiverse.UnitSystem._
import spire.implicits._

class QuantitySpec extends MultiverseCustomSpec{

  "Equality" - {

    "1 (m) should equal 100 (cm)" in {
      __Exercise__
      val result = r"1"(m) == r"100"(cm)
      __Verify__
      result should be (true)
    }

    "1 (m) should not equal 120 (cm)" in {
      __Exercise__
      val result = r"1"(m) == r"120"(cm)
      __Verify__
      result should be (false)
    }
  }

  "Order" - {

    "1 (m) should be less than 120 (cm)" in {
      __Exercise__
      val result = 1.0(m) < 120.0(cm)
      __Verify__
      result should be (true)
    }

    "2 (m) should not be less than 120 (cm)" in {
      __Exercise__
      val result = 2.0(m) < 120.0(cm)
      __Verify__
      result should be (false)
    }
  }
}
