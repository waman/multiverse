package org.waman.multiverse

import spire.implicits._
import org.waman.multiverse.implicits._
import org.waman.multiverse.predef.LengthUnits._

class QuantitySpec extends MultiverseCustomSpec{

  "Equality" - {

    "1 (m) should equal 1000 (mm)" in {
      // Exercise
      val result = r"1"(m) == r"1000"(mm)
      // Verify
      result should be (true)
    }

    "1 (m) should not equal 1200 (mm)" in {
      // Exercise
      val result = r"1"(m) == r"1200"(mm)
      // Verify
      result should be (false)
    }
  }

  "Order" - {

    "1 (m) should be less than 1200 (mm)" in {
      // Exercise
      val result = 1.0(m) < 1200.0(mm)
      // Verify
      result should be (true)
    }

    "2 (m) should not be less than 1200 (mm)" in {
      // Exercise
      val result = 2.0(m) < 1200.0(mm)
      // Verify
      result should be (false)
    }
  }

  "toString" - {

    "3.0(m) should return '3.0(m)' " in {
      // SetUp
      val q = 3.0(m)
      // Exercise
      val sut = q.toString
      // Verify
      sut should be ("3.0(m)")
    }

    "3.0(m) should return '3.0 [m]' when open ' [', close ']' passed" in {
      // SetUp
      val q = 3.0(m)
      // Exercise
      val sut = q.toString(" [", "]")
      // Verify
      sut should be ("3.0 [m]")
    }
  }
}
