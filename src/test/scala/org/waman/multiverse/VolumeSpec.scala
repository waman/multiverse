package org.waman.multiverse

import org.scalatest.prop.PropertyChecks
import org.waman.multiverse.UnitSystem._

import scala.language.postfixOps

/**
  * Expected values are from
  * <a href="https://en.wikipedia.org/wiki/Conversion_of_units">Conversion of units</a>.
  */
class VolumeSpec extends MultiverseCustomSpec with PropertyChecks{

  "UnitSystem#getSupportedUnits method should return supported units of volume" in {
    __SetUp__
    import VolumeUnit._
    __Exercise__
    val result = UnitSystem.getSupportedUnits(classOf[VolumeUnit])
    __Verify__
    result should contain theSameElementsAs Seq(
      CubicYoctoMetre,
      CubicZeptoMetre,
      CubicAttoMetre,
      CubicFemtoMetre,
      CubicPicoMetre,
      CubicNanoMetre,
      CubicMicroMetre,
      CubicMilliMetre,
      CubicCentiMetre,
      CubicDeciMetre,
      CubicMetre,
      CubicDecaMetre,
      CubicHectoMetre,
      CubicKiloMetre,
      CubicMegaMetre,
      CubicGigaMetre,
      CubicTeraMetre,
      CubicPetaMetre,
      CubicExaMetre,
      CubicZettaMetre,
      CubicYottaMetre,

      YoctoLitre,
      ZeptoLitre,
      AttoLitre,
      FemtoLitre,
      PicoLitre,
      NanoLitre,
      MicroLitre,
      MilliLitre,
      CentiLitre,
      DeciLitre,
      Litre,
      DecaLitre,
      HectoLitre,
      KiloLitre,
      MegaLitre,
      GigaLitre,
      TeraLitre,
      PetaLitre,
      ExaLitre,
      ZettaLitre,
      YottaLitre,

      CubicInch,

      Gallon_US_fluid,
      Gallon_imperial,

//      Barrel_USFluid,

      Lambda
    )
  }

  "units with context " in {
    import Context._
    val conversions =
      Table(
        ("unitSymbol", "contextSeq"),
        ("gal", Seq(UnitedStates, Imperial))
      )

    forAll(conversions){ (unitSymbol: String, contextSeq: Seq[Context]) =>
      val sut = UnitSystem.getSupportedContext(classOf[VolumeUnit], unitSymbol)
      sut should contain theSameElementsAs contextSeq
    }
  }

  "Tests where converting from some units to cubic metre like 3.0 km3 => 3e9 m3" in {
    val conversions =
      Table(
        ("volume", "expected"),
        (Seq(3.0.ym3, 3.0 ym3, 3.0 (ym3)), 3e-72),
        (Seq(3.0.zm3, 3.0 zm3, 3.0 (zm3)), 3e-63),
        (Seq(3.0.am3, 3.0 am3, 3.0 (am3)), 3e-54),
        (Seq(3.0.fm3, 3.0 fm3, 3.0 (fm3)), 3e-45),
        (Seq(3.0.pm3, 3.0 pm3, 3.0 (pm3)), 3e-36),
        (Seq(3.0.nm3, 3.0 nm3, 3.0 (nm3)), 3e-27),
        (Seq(3.0.μm3, 3.0 μm3, 3.0 (μm3)), 3e-18),
        (Seq(3.0.mm3, 3.0 mm3, 3.0 (mm3)), 3e-9),
        (Seq(3.0.cm3, 3.0 cm3, 3.0 (cm3)), 3e-6),
        (Seq(3.0.dm3, 3.0 dm3, 3.0 (dm3)), 3e-3),
        (Seq(3.0.m3 , 3.0 m3 , 3.0 (m3) ), 3.0),
        (Seq(3.0.dam3, 3.0 dam3, 3.0 (dam3)), 3e3),
        (Seq(3.0.hm3, 3.0 hm3, 3.0 (hm3)), 3e6),
        (Seq(3.0.km3, 3.0 km3, 3.0 (km3)), 3e9),
        (Seq(3.0.Mm3, 3.0 Mm3, 3.0 (Mm3)), 3e18),
        (Seq(3.0.Gm3, 3.0 Gm3, 3.0 (Gm3)), 3e27),
        (Seq(3.0.Tm3, 3.0 Tm3, 3.0 (Tm3)), 3e36),
        (Seq(3.0.Pm3, 3.0 Pm3, 3.0 (Pm3)), 3e45),
        (Seq(3.0.Em3, 3.0 Em3, 3.0 (Em3)), 3e54),
        (Seq(3.0.Zm3, 3.0 Zm3, 3.0 (Zm3)), 3e63),
        (Seq(3.0.Ym3, 3.0 Ym3, 3.0 (Ym3)), 3e72),

        (Seq(3.0.yL, 3.0 yL, 3.0 (yL)), 3e-27),
        (Seq(3.0.zL, 3.0 zL, 3.0 (zL)), 3e-24),
        (Seq(3.0.aL, 3.0 aL, 3.0 (aL)), 3e-21),
        (Seq(3.0.fL, 3.0 fL, 3.0 (fL)), 3e-18),
        (Seq(3.0.pL, 3.0 pL, 3.0 (pL)), 3e-15),
        (Seq(3.0.nL, 3.0 nL, 3.0 (nL)), 3e-12),
        (Seq(3.0.μL, 3.0 μL, 3.0 (μL)), 3e-9),
        (Seq(3.0.mL, 3.0 mL, 3.0 (mL)), 3e-6),
        (Seq(3.0.cL, 3.0 cL, 3.0 (cL)), 3e-5),
        (Seq(3.0.dL, 3.0 dL, 3.0 (dL)), 3e-4),
        (Seq(3.0.L , 3.0 L , 3.0 (L)) , 3e-3),
        (Seq(3.0.daL, 3.0 daL, 3.0 (daL)), 3e-2),
        (Seq(3.0.hL, 3.0 hL, 3.0 (hL)), 3e-1),
        (Seq(3.0.kL, 3.0 kL, 3.0 (kL)), 3.0),
        (Seq(3.0.ML, 3.0 ML, 3.0 (ML)), 3e3),
        (Seq(3.0.GL, 3.0 GL, 3.0 (GL)), 3e6),
        (Seq(3.0.TL, 3.0 TL, 3.0 (TL)), 3e9),
        (Seq(3.0.PL, 3.0 PL, 3.0 (PL)), 3e12),
        (Seq(3.0.EL, 3.0 EL, 3.0 (EL)), 3e15),
        (Seq(3.0.ZL, 3.0 ZL, 3.0 (ZL)), 3e18),
        (Seq(3.0.YL, 3.0 YL, 3.0 (YL)), 3e21),

//        (Seq(3.0.cu_in), 3.0 cu_in, 3.0 (cu_in), 16.387064e-6),
        (Seq(3.0.gal(US), 3.0 gal(US), 3.0 (gal(US))), 3.785411784e-3),

        (Seq(3.0.λ, 3.0 λ, 3.0 (λ)), 3e-9)
      )

    forAll(conversions){ (ls: Seq[Volume[Double]], expected: Double) =>
      ls.foreach{ l =>
        (l m3) should equal (%(expected))
      }
    }
  }

  "Tests where converting cubic metre unit to other units like 3.0 m3 => 3.0 * 1e-6 km3" in {
    val value = 3.0 m3

    val conversions =
      Table(
        ("volume", "expected"),
        (Seq(value.ym3, value ym3, value (ym3)), 3e72),
        (Seq(value.zm3, value zm3, value (zm3)), 3e63),
        (Seq(value.am3, value am3, value (am3)), 3e54),
        (Seq(value.fm3, value fm3, value (fm3)), 3e45),
        (Seq(value.pm3, value pm3, value (pm3)), 3e36),
        (Seq(value.nm3, value nm3, value (nm3)), 3e27),
        (Seq(value.μm3, value μm3, value (μm3)), 3e18),
        (Seq(value.mm3, value mm3, value (mm3)), 3e9),
        (Seq(value.cm3, value cm3, value (cm3)), 3e6),
        (Seq(value.dm3, value dm3, value (dm3)), 3e3),
        (Seq(value.m3 , value m3 , value (m3)) , 3.0),
        (Seq(value.dam3, value dam3, value (dam3)), 3e-3),
        (Seq(value.hm3, value hm3, value (hm3)), 3e-6),
        (Seq(value.km3, value km3, value (km3)), 3e-9),
        (Seq(value.Mm3, value Mm3, value (Mm3)), 3e-18),
        (Seq(value.Gm3, value Gm3, value (Gm3)), 3e-27),
        (Seq(value.Tm3, value Tm3, value (Tm3)), 3e-36),
        (Seq(value.Pm3, value Pm3, value (Pm3)), 3e-45),
        (Seq(value.Em3, value Em3, value (Em3)), 3e-54),
        (Seq(value.Zm3, value Zm3, value (Zm3)), 3e-63),
        (Seq(value.Ym3, value Ym3, value (Ym3)), 3e-72),

        (Seq(value.yL, value yL, value (yL)), 3e27),
        (Seq(value.zL, value zL, value (zL)), 3e24),
        (Seq(value.aL, value aL, value (aL)), 3e21),
        (Seq(value.fL, value fL, value (fL)), 3e18),
        (Seq(value.pL, value pL, value (pL)), 3e15),
        (Seq(value.nL, value nL, value (nL)), 3e12),
        (Seq(value.μL, value μL, value (μL)), 3e9),
        (Seq(value.mL, value mL, value (mL)), 3e6),
        (Seq(value.cL, value cL, value (cL)), 3e5),
        (Seq(value.dL, value dL, value (dL)), 3e4),
        (Seq(value.L , value L , value (L)) , 3e3),
        (Seq(value.daL, value daL, value (daL)), 3e2),
        (Seq(value.hL, value hL, value (hL)), 3e1),
        (Seq(value.kL, value kL, value (kL)), 3.0),
        (Seq(value.ML, value ML, value (ML)), 3e-3),
        (Seq(value.GL, value GL, value (GL)), 3e-6),
        (Seq(value.TL, value TL, value (TL)), 3e-9),
        (Seq(value.PL, value PL, value (PL)), 3e-12),
        (Seq(value.EL, value EL, value (EL)), 3e-15),
        (Seq(value.ZL, value ZL, value (ZL)), 3e-18),
        (Seq(value.YL, value YL, value (YL)), 3e-21),

        (Seq(value.λ , value λ , value (λ)) , 3.0e9)
      )

    forAll(conversions){ (as: Seq[Double], expected: Double) =>
      as.foreach{ a =>
        a should equal (%(expected))
      }
    }
  }
}
