package org.waman.multiverse.angle

import org.scalatest.prop.PropertyChecks
import org.waman.multiverse.UnitSystem._
import org.waman.multiverse.{MultiverseCustomSpec, UnitSystem}

import scala.language.postfixOps

/**
  * Expected values are from
  * <a href="https://en.wikipedia.org/wiki/Conversion_of_units">Conversion of units</a>.
  */
class AngleSpec extends MultiverseCustomSpec with PropertyChecks{

  "UnitSystem#getSupportedUnits method should return supported units of angle" in {
    __SetUp__
    import AngleUnit._
    __Exercise__
    val result = UnitSystem.getSupportedUnits(classOf[AngleUnit])
    __Verify__
    result should contain theSameElementsAs Seq(
      Radian,
      DeciRadian,
      CentiRadian,
      MilliRadian,
      MicroRadian,
      NanoRadian,
      PicoRadian,
      FemtoRadian,
      AttoRadian,
      ZeptoRadian,
      YoctoRadian,

      Degree,
      ArcMinute,
      ArcSecond,
      MilliArcSecond,
      MicroArcSecond,
      NanoArcSecond,
      PicoArcSecond,
      FemtoArcSecond,
      AttoArcSecond,
      ZeptoArcSecond,
      YoctoArcSecond,

      Gradian,
      Turn
    )
  }

  "Tests where converting from some units to radian like 3.0 deg => 3.0 * 2 PI / 360 rad" in {
    val conversions =
      Table(
        ("angle", "expected"),
        (Seq(3.0.rad, 3.0 rad, 3.0 (rad)), 3.0),
        (Seq(3.0.drad, 3.0 drad, 3.0 (drad)), 3e-1),
        (Seq(3.0.crad, 3.0 crad, 3.0 (crad)), 3e-2),
        (Seq(3.0.mrad, 3.0 mrad, 3.0 (mrad)), 3e-3),
        (Seq(3.0.μrad, 3.0 μrad, 3.0 (μrad)), 3e-6),
        (Seq(3.0.nrad, 3.0 nrad, 3.0 (nrad)), 3e-9),
        (Seq(3.0.prad, 3.0 prad, 3.0 (prad)), 3e-12),
        (Seq(3.0.frad, 3.0 frad, 3.0 (frad)), 3e-15),
        (Seq(3.0.arad, 3.0 arad, 3.0 (arad)), 3e-18),
        (Seq(3.0.zrad, 3.0 zrad, 3.0 (zrad)), 3e-21),
        (Seq(3.0.yrad, 3.0 yrad, 3.0 (yrad)), 3e-24),

        (Seq(3.0.deg, 3.0 deg, 3.0 (deg)), 3.0 * Math.PI / 180.0),
        (Seq(3.0.°  , 3.0 °  , 3.0 (°))  , 3.0 * Math.PI / 180.0),
        (Seq(3.0.arcmin, 3.0 arcmin, 3.0 (arcmin)), 3.0 * 0.290888e-3),
        (Seq(3.0.MOA   , 3.0 MOA   , 3.0 (MOA))   , 3.0 * 0.290888e-3),
        (Seq(3.0.arcsec, 3.0 arcsec, 3.0 (arcsec)), 3.0 * 4.848137e-6),
        (Seq(3.0.mas, 3.0 mas, 3.0 (mas)), 3.0 * 4.848137e-9),
        (Seq(3.0.μas, 3.0 μas, 3.0 (μas)), 3.0 * 4.848137e-12),
        (Seq(3.0.nas, 3.0 nas, 3.0 (nas)), 3.0 * 4.848137e-15),
        (Seq(3.0.pas, 3.0 pas, 3.0 (pas)), 3.0 * 4.848137e-18),
        (Seq(3.0.fas, 3.0 fas, 3.0 (fas)), 3.0 * 4.848137e-21),
        (Seq(3.0.aas, 3.0 aas, 3.0 (aas)), 3.0 * 4.848137e-24),
        (Seq(3.0.zas, 3.0 zas, 3.0 (zas)), 3.0 * 4.848137e-27),
        (Seq(3.0.yas, 3.0 yas, 3.0 (yas)), 3.0 * 4.848137e-30),

        (Seq(3.0.gon, 3.0 gon, 3.0 (gon)), 3.0 * 2.0 * Math.PI / 400.0),
        (Seq(3.0.ᵍ  , 3.0 ᵍ  , 3.0 (ᵍ))   , 3.0 * 2.0 * Math.PI / 400.0),
        (Seq(3.0.tr , 3.0 tr , 3.0 (tr)) , 3.0 * 2.0 * Math.PI)
      )

    forAll(conversions){ (ls: Seq[Angle[Double]], expected: Double) =>
      ls.foreach{ l =>
        (l rad) should equal (%(expected))
      }
    }
  }

  "Tests where converting radian unit to other units like 3.0 rad => 3.0 * 360 / 2 * PI deg" in {
    val value = 3.0 rad

    val conversions =
      Table(
        ("angle", "expected"),
        (Seq(value.rad, value rad, value (rad)), 3.0),
        (Seq(value.drad, value drad, value (drad)), 3e1),
        (Seq(value.crad, value crad, value (crad)), 3e2),
        (Seq(value.mrad, value mrad, value (mrad)), 3e3),
        (Seq(value.μrad, value μrad, value (μrad)), 3e6),
        (Seq(value.nrad, value nrad, value (nrad)), 3e9),
        (Seq(value.prad, value prad, value (prad)), 3e12),
        (Seq(value.frad, value frad, value (frad)), 3e15),
        (Seq(value.arad, value arad, value (arad)), 3e18),
        (Seq(value.zrad, value zrad, value (zrad)), 3e21),
        (Seq(value.yrad, value yrad, value (yrad)), 3e24),

        (Seq(value.deg, value deg, value (deg)), 3.0 * 180.0 / Math.PI),
        (Seq(value.°  , value °  , value (°))  , 3.0 * 180.0 / Math.PI),
        (Seq(value.arcmin, value arcmin, value (arcmin)), 3.0 / 0.290888e-3),
        (Seq(value.MOA   , value MOA   , value (MOA))   , 3.0 / 0.290888e-3),
        (Seq(value.arcsec, value arcsec, value (arcsec)), 3.0 / 4.848137e-6),
        (Seq(value.mas, value mas, value (mas)), 3.0 / 4.848137e-9),
        (Seq(value.μas, value μas, value (μas)), 3.0 / 4.848137e-12),
        (Seq(value.nas, value nas, value (nas)), 3.0 / 4.848137e-15),
        (Seq(value.pas, value pas, value (pas)), 3.0 / 4.848137e-18),
        (Seq(value.fas, value fas, value (fas)), 3.0 / 4.848137e-21),
        (Seq(value.aas, value aas, value (aas)), 3.0 / 4.848137e-24),
        (Seq(value.zas, value zas, value (zas)), 3.0 / 4.848137e-27),
        (Seq(value.yas, value yas, value (yas)), 3.0 / 4.848137e-30),

        (Seq(value.gon, value gon, value (gon)), 3.0 * 400.0 / (2.0 * Math.PI)),
        (Seq(value.ᵍ  , value ᵍ   , value (ᵍ))  , 3.0 * 400.0 / (2.0 * Math.PI)),
        (Seq(value.tr , value tr , value (tr)) , 3.0 / (2.0 * Math.PI))
      )

    forAll(conversions){ (as: Seq[Double], expected: Double) =>
      as.foreach{ a =>
        a should equal (%(expected))
      }
    }
  }
}
