package org.waman.multiverse

import org.scalatest.prop.PropertyChecks
import org.waman.multiverse.UnitSystem._

import scala.language.postfixOps

/**
  * Expected values are from
  * <a href="https://en.wikipedia.org/wiki/Conversion_of_units">Conversion of units</a>.
  */
class VolumeSpec extends MultiverseCustomSpec with PropertyChecks with MKSUnitSystem{

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

      Lambda
    )
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

        (Seq(3.0.yL, 3.0 yL, 3.0 (yL)), 3e-24),
        (Seq(3.0.zL, 3.0 zL, 3.0 (zL)), 3e-21),
        (Seq(3.0.aL, 3.0 aL, 3.0 (aL)), 3e-18),
        (Seq(3.0.fL, 3.0 fL, 3.0 (fL)), 3e-15),
        (Seq(3.0.pL, 3.0 pL, 3.0 (pL)), 3e-12),
        (Seq(3.0.nL, 3.0 nL, 3.0 (nL)), 3e-9),
        (Seq(3.0.μL, 3.0 μL, 3.0 (μL)), 3e-6),
        (Seq(3.0.mL, 3.0 mL, 3.0 (mL)), 3e-3),
        (Seq(3.0.cL, 3.0 cL, 3.0 (cL)), 3e-2),
        (Seq(3.0.dL, 3.0 dL, 3.0 (dL)), 3e-1),
        (Seq(3.0.L , 3.0 L , 3.0 (L)) , 3.0),
        (Seq(3.0.daL, 3.0 daL, 3.0 (daL)), 3e1),
        (Seq(3.0.hL, 3.0 hL, 3.0 (hL)), 3e2),
        (Seq(3.0.kL, 3.0 kL, 3.0 (kL)), 3e3),
        (Seq(3.0.ML, 3.0 ML, 3.0 (ML)), 3e6),
        (Seq(3.0.GL, 3.0 GL, 3.0 (GL)), 3e9),
        (Seq(3.0.TL, 3.0 TL, 3.0 (TL)), 3e12),
        (Seq(3.0.PL, 3.0 PL, 3.0 (PL)), 3e15),
        (Seq(3.0.EL, 3.0 EL, 3.0 (EL)), 3e18),
        (Seq(3.0.ZL, 3.0 ZL, 3.0 (ZL)), 3e21),
        (Seq(3.0.YL, 3.0 YL, 3.0 (YL)), 3e24),

        (Seq(3.0.λ, 3.0 λ, 3.0 (λ)), 3e-9)
      )

    forAll(conversions){ (ls: Seq[Volume[Double]], expected: Double) =>
      ls.foreach{ l =>
        (l m3) should equal (%(expected))
      }
    }
  }

  val three_m3 = 3.0 m3

  "Tests where converting cubic metre unit to other units like 3.0 m3 => 3.0 * 1e-6 km3" in {
    val conversions =
      Table(
        ("volume", "expected"),
        (Seq(three_m3.ym3, three_m3 ym3, three_m3 (ym3)), 3e72),
        (Seq(three_m3.zm3, three_m3 zm3, three_m3 (zm3)), 3e63),
        (Seq(three_m3.am3, three_m3 am3, three_m3 (am3)), 3e54),
        (Seq(three_m3.fm3, three_m3 fm3, three_m3 (fm3)), 3e45),
        (Seq(three_m3.pm3, three_m3 pm3, three_m3 (pm3)), 3e36),
        (Seq(three_m3.nm3, three_m3 nm3, three_m3 (nm3)), 3e27),
        (Seq(three_m3.μm3, three_m3 μm3, three_m3 (μm3)), 3e18),
        (Seq(three_m3.mm3, three_m3 mm3, three_m3 (mm3)), 3e9),
        (Seq(three_m3.cm3, three_m3 cm3, three_m3 (cm3)), 3e6),
        (Seq(three_m3.dm3, three_m3 dm3, three_m3 (dm3)), 3e3),
        (Seq(three_m3.m3 , three_m3 m3 , three_m3 (m3)) , 3.0),
        (Seq(three_m3.dam3, three_m3 dam3, three_m3 (dam3)), 3e-3),
        (Seq(three_m3.hm3, three_m3 hm3, three_m3 (hm3)), 3e-6),
        (Seq(three_m3.km3, three_m3 km3, three_m3 (km3)), 3e-9),
        (Seq(three_m3.Mm3, three_m3 Mm3, three_m3 (Mm3)), 3e-18),
        (Seq(three_m3.Gm3, three_m3 Gm3, three_m3 (Gm3)), 3e-27),
        (Seq(three_m3.Tm3, three_m3 Tm3, three_m3 (Tm3)), 3e-36),
        (Seq(three_m3.Pm3, three_m3 Pm3, three_m3 (Pm3)), 3e-45),
        (Seq(three_m3.Em3, three_m3 Em3, three_m3 (Em3)), 3e-54),
        (Seq(three_m3.Zm3, three_m3 Zm3, three_m3 (Zm3)), 3e-63),
        (Seq(three_m3.Ym3, three_m3 Ym3, three_m3 (Ym3)), 3e-72),

        (Seq(three_m3.yL, three_m3 yL, three_m3 (yL)), 3e24),
        (Seq(three_m3.zL, three_m3 zL, three_m3 (zL)), 3e21),
        (Seq(three_m3.aL, three_m3 aL, three_m3 (aL)), 3e18),
        (Seq(three_m3.fL, three_m3 fL, three_m3 (fL)), 3e15),
        (Seq(three_m3.pL, three_m3 pL, three_m3 (pL)), 3e12),
        (Seq(three_m3.nL, three_m3 nL, three_m3 (nL)), 3e9),
        (Seq(three_m3.μL, three_m3 μL, three_m3 (μL)), 3e6),
        (Seq(three_m3.mL, three_m3 mL, three_m3 (mL)), 3e3),
        (Seq(three_m3.cL, three_m3 cL, three_m3 (cL)), 3e2),
        (Seq(three_m3.dL, three_m3 dL, three_m3 (dL)), 3e1),
        (Seq(three_m3.L , three_m3 L , three_m3 (L)) , 3.0),
        (Seq(three_m3.daL, three_m3 daL, three_m3 (daL)), 3e-1),
        (Seq(three_m3.hL, three_m3 hL, three_m3 (hL)), 3e-2),
        (Seq(three_m3.kL, three_m3 kL, three_m3 (kL)), 3e-3),
        (Seq(three_m3.ML, three_m3 ML, three_m3 (ML)), 3e-6),
        (Seq(three_m3.GL, three_m3 GL, three_m3 (GL)), 3e-9),
        (Seq(three_m3.TL, three_m3 TL, three_m3 (TL)), 3e-12),
        (Seq(three_m3.PL, three_m3 PL, three_m3 (PL)), 3e-15),
        (Seq(three_m3.EL, three_m3 EL, three_m3 (EL)), 3e-18),
        (Seq(three_m3.ZL, three_m3 ZL, three_m3 (ZL)), 3e-21),
        (Seq(three_m3.YL, three_m3 YL, three_m3 (YL)), 3e-24),

        (Seq(three_m3.λ , three_m3 λ , three_m3 (λ)) , 3.0e9)
      )

    forAll(conversions){ (as: Seq[Double], expected: Double) =>
      as.foreach{ a =>
        a should equal (%(expected))
      }
    }
  }
}
