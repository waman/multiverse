package org.waman.multiverse.unit.basic

import org.waman.multiverse.MultiverseCustomSpec
import org.waman.multiverse.implicits._
import org.waman.multiverse.unit.basic.LengthUnits._
import org.waman.multiverse.unit.basic.AreaUnits._
import org.waman.multiverse.unit.basic.VolumeUnits._

class VolumeSpec extends MultiverseCustomSpec {

  "All of m3, m*m*m, and s.cubic should return equivalent objects" in {
    m3 should equal (m*m*m)
    m3 should equal (m.cubic)

    mm*mm*mm should equal (mm.cubic)
  }

  "The name property of volume unit object should return the proper string" in {
    // Exercise
    val conversions =
      Table(
        ("volume unit", "expected"),
        (m3 , "metre cubic"),
        (mm*mm*mm, "millimetre cubic"),
        (m2*mm, "metre squared times millimetre")
      )
    // Verify
    forAll(conversions){ (sut: VolumeUnit, expected: String) =>
      sut.name should equal (expected)
    }
  }

  "The symbol property of volume unit object should return the proper string" in {
    // Exercise
    val conversions =
      Table(
        ("volume unit", "expected"),
        (m3 , "m³"),
        (mm*mm*mm, "mm³"),
        (m*mm*nm, "m*mm*nm")
      )
    // Verify
    forAll(conversions){ (sut: VolumeUnit, expected: String) =>
      sut.symbol should equal (expected)
    }
  }

  "The aliases property of volume unit object should return the proper Seq of String" in {
    // Exercise
    val conversions =
      Table(
        ("volume unit", "expected"),
        (m3 , Seq("m3", "m.cubic", "m*m*m")),
        (mm*mm*mm, Seq("mm.cubic", "mm*mm*mm")),
        (m*mm*nm, Nil)
      )
    // Verify
    forAll(conversions){ (sut: VolumeUnit, expected: Seq[String]) =>
      sut.aliases should contain theSameElementsAs expected
    }
  }

  "3.0 <<volume unit>> should be converted to the equivalent value in metre cubic" in {
    // Exercise
    val conversions =
      Table(
        ("volume", "expected"),
        (3.0(m3) , 3.0),
        (3.0(m*m*m), 3.0),
        (3.0(m.cubic), 3.0),
        (3.0(mm*mm*mm), 3.0*1e-9),
        (3.0(mm.cubic), 3.0*1e-9),
        (3.0(m*mm*nm), 3.0*1e-12)
      )
    // Verify
    forAll(conversions){ (sut: Volume[Double], expected: Double) =>
      sut(m3) should equal (%%%%(expected))
    }
  }

  "3.0(m3) should be converted to the equivalent value in other volume units" in {
    // SetUp
    val q = 3.0 (m3)
    // Exercise
    val conversions =
      Table(
        ("volume", "expected"),
        (q(m3) , 3.0),
        (q(m*m*m), 3.0),
        (q(mm*mm*mm), 3.0*1e9),
        (q(m*mm*nm), 3.0*1e12)
      )
    // Verify
    forAll(conversions){ (sut: Double, expected: Double) =>
      sut should equal (%%%%(expected))
    }
  }
}
