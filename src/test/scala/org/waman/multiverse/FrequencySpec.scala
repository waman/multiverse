package org.waman.multiverse

import org.scalatest.prop.PropertyChecks
import org.waman.multiverse.UnitSystem._

import scala.language.postfixOps

/**
  * Expected values are from
  * <a href="https://en.wikipedia.org/wiki/Conversion_of_units">Conversion of units</a>.
  */
class FrequencySpec extends MultiverseCustomSpec with PropertyChecks with MKSUnitSystem{

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

  "Tests where converting from some units to rad/s like 3.0 deg/s => 3.0 * 2 PI / 360 rad/s" in {
    val conversions =
      Table(
        ("frequency", "expected"),
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

    forAll(conversions){ (fs: Seq[Frequency[Double]], expected: Double) =>
      fs.foreach{ f =>
        (f Hz) should equal (%(expected))
      }
    }
  }

  val threeHz = 3.0 Hz

  "Tests where converting metre unit to other units like 3.0 rad/s => 3.0 * 180.0 / PI deg/s" in {
    val conversions =
      Table(
        ("frequency", "expected"),
        (Seq(threeHz.yHz, threeHz yHz, threeHz(yHz)), 3e24),
        (Seq(threeHz.zHz, threeHz zHz, threeHz(zHz)), 3e21),
        (Seq(threeHz.aHz, threeHz aHz, threeHz(aHz)), 3e18),
        (Seq(threeHz.fHz, threeHz fHz, threeHz(fHz)), 3e15),
        (Seq(threeHz.pHz, threeHz pHz, threeHz(pHz)), 3e12),
        (Seq(threeHz.nHz, threeHz nHz, threeHz(nHz)), 3e9),
        (Seq(threeHz.μHz, threeHz μHz, threeHz(μHz)), 3e6),
        (Seq(threeHz.mHz, threeHz mHz, threeHz(mHz)), 3e3),
        (Seq(threeHz.cHz, threeHz cHz, threeHz(cHz)), 3e2),
        (Seq(threeHz.dHz, threeHz dHz, threeHz(dHz)), 3e1),
        (Seq(threeHz.Hz, threeHz Hz, threeHz(Hz)), 3.0),
        (Seq(threeHz.daHz, threeHz daHz, threeHz(daHz)), 3e-1),
        (Seq(threeHz.hHz, threeHz hHz, threeHz(hHz)), 3e-2),
        (Seq(threeHz.kHz, threeHz kHz, threeHz(kHz)), 3e-3),
        (Seq(threeHz.MHz, threeHz MHz, threeHz(MHz)), 3e-6),
        (Seq(threeHz.GHz, threeHz GHz, threeHz(GHz)), 3e-9),
        (Seq(threeHz.THz, threeHz THz, threeHz(THz)), 3e-12),
        (Seq(threeHz.PHz, threeHz PHz, threeHz(PHz)), 3e-15),
        (Seq(threeHz.EHz, threeHz EHz, threeHz(EHz)), 3e-18),
        (Seq(threeHz.ZHz, threeHz ZHz, threeHz(ZHz)), 3e-21),
        (Seq(threeHz.YHz, threeHz YHz, threeHz(YHz)), 3e-24)
      )

    forAll(conversions) { (as: Seq[Double], expected: Double) =>
      as.foreach { a =>
        a should equal(%(expected))
      }
    }
  }
}
