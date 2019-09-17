package org.waman.multiverse

import spire.implicits._
import org.waman.multiverse.implicits._
import org.waman.multiverse.unit.BasicUnits._

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

    "3 (m/s) should equal 3 (mm/ms)" in {
      // Exercise
      val result = r"3"(m/s) == r"3"(mm/ms)
      // Verify
      result should be (true)
    }

    "3 (m/s) should not equal 3 (mm/s)" in {
      // Exercise
      val result = r"3"(m/s) == r"3"(mm/s)
      // Verify
      result should be (false)
    }

    "3 (m/s) should return the same hashCoide as 3 (mm/ms)" in {
      // Exercise
      val result = r"3"(m/s).hashCode == r"3"(mm/ms).hashCode
      // Verify
      result should be (true)
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

  "Extensive Quantity operations" - {

    "Addition: 3.0(m) + 10.0(mm) = 3.01(m)" in {
      // Exercise
      val sut = r"3.0"(m) + r"10.0"(mm)
      // Verify
      sut should be (r"3.01"(m))
    }

    "Subtractioin: 3.0(m) - 10.0(mm) = 2.99(m)" in {
      // Exercise
      val sut = r"3.0"(m) - r"10.0"(mm)
      // Verify
      sut should be (r"2.99"(m))
    }
  }

  "Scalar multiplication: 3.0(m) * 4.0 = 12.0(m)" in {
    // Exercise
    val sut = r"3.0" (m) * r"4.0"
    // Verify
    sut should be(r"12.0" (m))
  }

  "Scalar divistion: 3.0(m) / 4.0 = 0.75(m)" in {
    // Exercise
    val sut = r"3.0" (m) / r"4.0"
    // Verify
    sut should be(r"0.75" (m))
  }
}
