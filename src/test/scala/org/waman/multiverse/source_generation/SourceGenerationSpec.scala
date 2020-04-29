package org.waman.multiverse.source_generation

import spire.math.Real
import spire.implicits._
import org.waman.multiverse
import multiverse.implicits._
import multiverse.unit
import multiverse.MultiverseCustomSpec
import unit.Constants
import unit.basic.{LengthUnitObjects, LengthUnits}
import unit.thermodynamics.EntropyUnit
import unit.thermodynamics.EntropyUnits.{ban, bit, nat}

class SourceGenerationSpec extends MultiverseCustomSpec {

  "JSON" - {

    "aliases" - {

      "Product unit should have the combinated aliases" in {
        import unit.electrics.ChargeUnits.statC
        import unit.basic.LengthUnits.NM
        // SetUp
        val expected = Seq("statC*nmi", "Fr*NM", "Fr*nmi", "esu*NM", "esu*nmi")
        // Exercise
        val sut = statC * NM
        // Verify
        sut.aliases should contain theSameElementsAs expected
        sut.symbol should be ("statC*NM")
      }

      "Quotient unit should have combinated aliases" in {
        import unit.BasicUnits._
        // SetUp
        val expected = Seq("Km/s", "Km/sec", "km/sec")
        // Exercise
        val sut = km/s
        // Verify
        sut.aliases should contain theSameElementsAs expected
        sut.symbol should be ("km/s")
      }

      "Prefixed unit should have combinated aliases of a base unit and prefix" in {
        import unit.basic.TimeUnitObjects
        // SetUp
        val expected = Seq("μsec", "mcs", "mcsec")
        // Exercise
        val sut = TimeUnitObjects.microsecond
        // Verify
        sut.aliases should contain theSameElementsAs expected
        sut.symbol should be ("μs")
      }

      "Optional aliases (alias with parenthesise) should be removed at the aliases property of the case object" in {
        // Exercise
        val sut = LengthUnitObjects.pica
        // Verify
        sut.aliases should not contain "pc"
      }

      "Optional aliases (alias with parenthesise) should not be defined on the Units object" in {
        // Exercise
        val sut = LengthUnits.pc
        // Verify
        sut should not be LengthUnitObjects.pica
        // pc is parsec
      }
    }

    "interval" - {

      "The interval value should be correct when the interval string is a product of constant and number" in {
        import unit.electrics.ChargeUnitObjects.statcoulomb
        import unit.electrics.DipoleUnitObjects.debye
        // Exercise
        val sut = debye.interval
        // Verify
        sut should equal (r"1e-20" * statcoulomb.interval)
      }


      "The interval value should be correct when the interval string is a quotient of constant and number" in {
        import unit.electrics.VoltageUnitObjects.statvolt
        // Exercise
        val sut = statvolt.interval
        // Verify
        sut should equal (Constants.SpeedOfLight / 1e6)
      }
    }

    "baseUnit" - {

      "The baseUnit property that refers its child attribute should provide the proper interval" in {
        //      {"name":"year", "symbol":"yr", ..., "baseUnit":"year(gregorian)",
        //        "attributes": [
        //          ...
        //          {"name":"gregorian", "interval":"365.2425", "baseUnit":"day"},
        //          ...
        //         ]}
        import unit.basic.TimeUnit
        import unit.basic.TimeUnits._
        // Exercise
        val conversions =
        Table(
          ("parent", "expected"),
          (mo, mo(gregorian)),
          (yr, yr(gregorian)),
          (dec, dec(gregorian)),
          (century, century(gregorian))
        )
        // Verify
        forAll(conversions){ (sut: TimeUnit, expected: TimeUnit) =>
          sut.interval should equal (expected.interval)
        }
      }
    }

    "excludePrefixes" - {

      "millibyte unit should not be defined (excludePrefixes should work well)" in {
        "org.waman.multiverse.unit.thermodynamics.EntropyUnitObjects.kilobyte" should compile
        "org.waman.multiverse.unit.thermodynamics.EntropyUnitObjects.millibyte" shouldNot compile
      }
    }

    "convertible" - {

      "'reciprocal' convertible should work well" in {
        import unit.electrics.ConductanceUnits._
        import unit.electrics.ResistanceUnits._
        // Exercise
        val conversions =
          Table(
            ("sut", "expected"),
            (3.0(S).toResistance(ohm), 1.0 / 3.0),
            (3.0(mS).toResistance(mohm), 1.0e6 / 3.0),
            (3.0(ohm).toConductance(S), 1.0 / 3.0),
            (3.0(kohm).toConductance(kS), 1.0e-6 / 3.0)
          )
        forAll(conversions){ (sut: Double, expected: Double) =>
          // Verify
          sut should equal (%%%%(expected))
        }
      }
    }
  }

  "Units" - {

    "Length" - {

      "The interval of metric foot should be sqrt(1/10)" in {
        // SetUp
        val expected = Real("1/10").sqrt()
        // Exercise
        val sut = LengthUnitObjects.metric_foot.interval
        // Verify
        sut should equal (expected)
      }
    }

    "Area" - {

      import unit.basic.LengthUnits._
      import unit.basic.AreaUnit
      import unit.basic.AreaUnitObjects
      import unit.basic.AreaUnits._

      "square_metre should have the proper symbol and some aliases" in {
        // SetUp
        val expected = m.squared
        // Exercise
        val conversions =
          Table(
            "area unit",
            AreaUnitObjects.square_metre,
            m2,
            `m²`
          )
        // Verify
        forAll(conversions){ sut: AreaUnit =>
          sut should equal (expected)
        }
      }

      "square_micrometre should have the proper symbol and some aliases" in {
        // SetUp
        val expected = mcm.squared
        // Exercise
        val conversions =
          Table(
            "area unit",
            AreaUnitObjects.square_micrometre,
            μm2,
            mcm2,
            `μm²`,
            `mcm²`
          )
        // Verify
        forAll(conversions){ sut: AreaUnit =>
          sut should equal (expected)
        }
      }
    }

    "Volume" - {
      import unit.basic.LengthUnits._
      import unit.basic.VolumeUnit
      import unit.basic.VolumeUnitObjects
      import unit.basic.VolumeUnits._

      "metre_cubic should have the proper symbol and some aliases" in {
        // SetUp
        val expected = m.cubic
        // Exercise
        val conversions =
          Table(
            "cubic unit",
            VolumeUnitObjects.cubic_metre,
            m3,
            `m³`
          )
        // Verify
        forAll(conversions){ sut: VolumeUnit =>
          sut should equal (expected)
        }
      }

      "micrometre_cubic should have the proper symbol and some aliases" in {
        // SetUp
        val expected = mcm.cubic
        // Exercise
        val conversions =
          Table(
            "volume unit",
            VolumeUnitObjects.cubic_micrometre,
            μm3,
            mcm3,
            `μm³`,
            `mcm³`
          )
        // Verify
        forAll(conversions){ sut: VolumeUnit =>
          sut should equal (expected)
        }
      }
    }

    "TimeSquared" - {

      import unit.basic.TimeUnits._
      import unit.mechanics.TimeSquaredUnit
      import unit.mechanics.TimeSquaredUnitObjects
      import unit.mechanics.TimeSquaredUnits._

      "second_squared should have the proper symbol and some aliases" in {
        // SetUp
        val expected = s.squared
        // Exercise
        val conversions =
          Table(
            "time squared unit",
            TimeSquaredUnitObjects.second_squared,
            s2,
            `s²`,
            sec2,
            `sec²`
          )
        // Verify
        forAll(conversions){ sut: TimeSquaredUnit =>
          sut should equal (expected)
        }
      }

      "microsecond_squared should have the proper symbol and some aliases" in {
        // SetUp
        val expected = mcs.squared
        // Exercise
        val conversions =
          Table(
            "time squared unit",
            TimeSquaredUnitObjects.microsecond_squared,
            μs2,
            mcs2,
            `μs²`,
            `mcs²`,
            μsec2,
            mcsec2,
            `μsec²`,
            `mcsec²`
          )
        // Verify
        forAll(conversions){ sut: TimeSquaredUnit =>
          sut should equal (expected)
        }
      }
    }

    "Entropy" - {

      "nat, bit, and ban should have the proper interval values" in {
        // Exercise
        val conversions =
          Table(
            ("entropy unit", "expected"),
            (nat, Constants.BoltzmannConstant),
            (bit, Real(2).log() * Constants.BoltzmannConstant),
            (ban, Real(10).log() * Constants.BoltzmannConstant)
          )
        // Verify
        forAll(conversions){ (sut: EntropyUnit, expected: Real) =>
          sut.interval should equal (expected)
        }
      }
    }
  }

  "Scala code" - {

    "getSIUnit method" - {

      "The getSIUnit method return the composite (quotient) unit m/s" in {
        import unit.BasicUnits._
        import unit.basic.VelocityUnit
        // SetUp
        val expected = m/s
        // Exercise
        val sut = VelocityUnit.getSIUnit
        // Verify
        sut should be (expected)
      }
    }

    "getUnits method" - {

      "The getUnits method of VelocityUnits should return Seq(c, M)" in {
        import unit.basic.VelocityUnit
        import unit.basic.VelocityUnits._
        // Exercise
        val sut = VelocityUnit.getUnits
        // Verify
        sut should contain theSameElementsInOrderAs Seq(c, M, kt, kine)
      }
    }
  }
}
