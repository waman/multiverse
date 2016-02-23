package org.waman.multiverse.time

import org.scalatest.prop.PropertyChecks
import org.waman.multiverse.UnitSystem._
import org.waman.multiverse.{MultiverseCustomSpec, UnitSystem}

import scala.language.postfixOps

/**
  * Expected values are from
  * <a href="https://en.wikipedia.org/wiki/Conversion_of_units">Conversion of units</a>.
  */
class FrequencySpec extends MultiverseCustomSpec with PropertyChecks{

  "UnitSystem#getSupportedUnits method should return supported units of frequency" in {
    __SetUp__
    import FrequencyUnit._
    __Exercise__
    val result = UnitSystem.getSupportedUnits(classOf[FrequencyUnit])
    __Verify__
    result should contain allOf (
      YoctoHeltz,
      ZeptoHeltz,
      AttoHeltz ,
      FemtoHeltz,
      PicoHeltz ,
      NanoHeltz ,
      MicroHeltz,
      MilliHeltz,
      CentiHeltz,
      DeciHeltz ,
      Heltz     ,
      DecaHeltz ,
      HectoHeltz,
      KiloHeltz ,
      MegaHeltz ,
      GigaHeltz ,
      TeraHeltz ,
      PetaHeltz ,
      ExaHeltz  ,
      ZettaHeltz,
      YottaHeltz)
  }

  "Frequency object should be converted to a frequency by toFrequency method" in {
    val conversions =
      Table(
        ("frequency", "expected"),
        (3.0 Hz, 3.0 * 2.0 * Math.PI),
        (3.0 GHz, 3.0 * 1e9 * 2.0 * Math.PI)
      )
    __Verify__
    forAll(conversions){ (f: Frequency[Double], expected: Double) =>
      f.toAngularVelocity.rad/s should equal (%(expected))
    }
  }

  "Tests where converting from some units to rad/s like 3.0 deg/s => 3.0 * 2 PI / 360 rad/s" in {
    __Exercise__
    val conversions =
      Table(
        ("frequencies", "expected"),
        (Seq(3.0.yHz, 3.0 yHz, 3.0 (yHz)), 3e-24),
        (Seq(3.0.zHz, 3.0 zHz, 3.0 (zHz)), 3e-21),
        (Seq(3.0.aHz, 3.0 aHz, 3.0 (aHz)), 3e-18),
        (Seq(3.0.fHz, 3.0 fHz, 3.0 (fHz)), 3e-15),
        (Seq(3.0.pHz, 3.0 pHz, 3.0 (pHz)), 3e-12),
        (Seq(3.0.nHz, 3.0 nHz, 3.0 (nHz)), 3e-9),
        (Seq(3.0.μHz, 3.0 μHz, 3.0 (μHz)), 3e-6),
        (Seq(3.0.mHz, 3.0 mHz, 3.0 (mHz)), 3e-3),
        (Seq(3.0.cHz, 3.0 cHz, 3.0 (cHz)), 3e-2),
        (Seq(3.0.dHz, 3.0 dHz, 3.0 (dHz)), 3e-1),
        (Seq(3.0.Hz , 3.0 Hz , 3.0 (Hz))  , 3.0),
        (Seq(3.0.daHz, 3.0 daHz, 3.0 (daHz)), 3e1),
        (Seq(3.0.hHz, 3.0 hHz, 3.0 (hHz)), 3e2),
        (Seq(3.0.kHz, 3.0 kHz, 3.0 (kHz)), 3e3),
        (Seq(3.0.MHz, 3.0 MHz, 3.0 (MHz)), 3e6),
        (Seq(3.0.GHz, 3.0 GHz, 3.0 (GHz)), 3e9),
        (Seq(3.0.THz, 3.0 THz, 3.0 (THz)), 3e12),
        (Seq(3.0.PHz, 3.0 PHz, 3.0 (PHz)), 3e15),
        (Seq(3.0.EHz, 3.0 EHz, 3.0 (EHz)), 3e18),
        (Seq(3.0.ZHz, 3.0 ZHz, 3.0 (ZHz)), 3e21),
        (Seq(3.0.YHz, 3.0 YHz, 3.0 (YHz)), 3e24)
      )
    __Verify__
    forAll(conversions){ (suts: Seq[Frequency[Double]], expected: Double) =>
      suts.foreach{ sut =>
        (sut Hz) should equal (%%%%(expected))
      }
    }
  }

  "Tests where converting metre unit to other units like 3.0 rad/s => 3.0 * 180.0 / PI deg/s" in {
    __SetUp__
    val value = 3.0 (Hz)
    __Exercise__
    val conversions =
      Table(
        ("frequencies", "expected"),
        (Seq(value.yHz, value yHz, value(yHz)), 3e24),
        (Seq(value.zHz, value zHz, value(zHz)), 3e21),
        (Seq(value.aHz, value aHz, value(aHz)), 3e18),
        (Seq(value.fHz, value fHz, value(fHz)), 3e15),
        (Seq(value.pHz, value pHz, value(pHz)), 3e12),
        (Seq(value.nHz, value nHz, value(nHz)), 3e9),
        (Seq(value.μHz, value μHz, value(μHz)), 3e6),
        (Seq(value.mHz, value mHz, value(mHz)), 3e3),
        (Seq(value.cHz, value cHz, value(cHz)), 3e2),
        (Seq(value.dHz, value dHz, value(dHz)), 3e1),
        (Seq(value.Hz, value Hz, value(Hz)), 3.0),
        (Seq(value.daHz, value daHz, value(daHz)), 3e-1),
        (Seq(value.hHz, value hHz, value(hHz)), 3e-2),
        (Seq(value.kHz, value kHz, value(kHz)), 3e-3),
        (Seq(value.MHz, value MHz, value(MHz)), 3e-6),
        (Seq(value.GHz, value GHz, value(GHz)), 3e-9),
        (Seq(value.THz, value THz, value(THz)), 3e-12),
        (Seq(value.PHz, value PHz, value(PHz)), 3e-15),
        (Seq(value.EHz, value EHz, value(EHz)), 3e-18),
        (Seq(value.ZHz, value ZHz, value(ZHz)), 3e-21),
        (Seq(value.YHz, value YHz, value(YHz)), 3e-24)
      )
    __Verify__
    forAll(conversions) { (suts: Seq[Double], expected: Double) =>
      suts.foreach { sut =>
        sut should equal(%%%%(expected))
      }
    }
  }
}
