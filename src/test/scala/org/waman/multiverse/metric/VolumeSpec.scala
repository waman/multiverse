package org.waman.multiverse.metric

import org.scalatest.prop.PropertyChecks
import org.waman.multiverse.UnitSystem._
import org.waman.multiverse.{MultiverseCustomSpec, UnitSystem}

import scala.language.postfixOps

/**
  * Expected values are from
  * <a href="https://en.wikipedia.org/wiki/Conversion_of_units">Conversion of units</a>.
  */
class VolumeSpec extends MultiverseCustomSpec with PropertyChecks{

  "Predefiend volume units" - {

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

        Lambda,

        CubicInch,
        CubicFathom,
        CubicFoot,
        CubicYard,
        CubicMile,

        BoardFoot,
        AcreFoot,

        Gallon_beer,
        Perch,
        Barrel,

        Minim_US,
        Fluid_Ounce_US,
        Gill_US,
        Pint_US_fluid,
        Quart_US_fluid,
        Gallon_US_fluid,
        Barrel_US_fluid,
        Hogshead_US,

        FluidDram_US,

        Pint_US_dry,
        Quart_US_dry,
        Gallon_US_dry,
        Peck_US_dry,
        Bushel_US_dry,
        Barrel_US_dry,

        Bushel_US_dry_level,

        Minim_imperial,
        Fluid_Ounce_imperial,
        Gill_imperial,
        Pint_imperial,
        Quart_imperial,
        Gallon_imperial,
        Peck_imperial,
        Bushel_imperial,
        Barrel_imperial,
        Hogshead_imperial,

        FluidScruple,
        FluidDrachm_imperial,

        Bucket
      )
    }

    "Tests where converting from some units to cubic metre like 3.0 km3 => 3e9 m3" in {
      __Exercise__
      val conversions =
        Table(
          ("volumes", "expected"),
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
          (Seq(3.0.m3, 3.0 m3, 3.0 (m3)), 3.0),
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
          (Seq(3.0.L, 3.0 L, 3.0 (L)), 3e-3),
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

          (Seq(3.0.λ, 3.0 λ, 3.0 (λ)), 3e-9),

          (Seq(3.0.in3, 3.0 in3, 3.0 (in3)), 3.0 * 16.387064e-6),
          (Seq(3.0.ft3, 3.0 ft3, 3.0 (ft3)), 3.0 * 0.028316846592),
          (Seq(3.0.yd3, 3.0 yd3, 3.0 (yd3)), 3.0 * 0.764554857984),
          (Seq(3.0.ftm3, 3.0 ftm3, 3.0 (ftm3)), 3.0 * 6.116438863872),
          (Seq(3.0.mi3, 3.0 mi3, 3.0 (mi3)), 3.0 * 4168181825.440579584),
          (Seq(3.0.cu_in, 3.0 cu_in, 3.0 (cu_in)), 3.0 * 16.387064e-6),
          (Seq(3.0.cu_ft, 3.0 cu_ft, 3.0 (cu_ft)), 3.0 * 0.028316846592),
          (Seq(3.0.cu_yd, 3.0 cu_yd, 3.0 (cu_yd)), 3.0 * 0.764554857984),
          (Seq(3.0.cu_fm, 3.0 cu_fm, 3.0 (cu_fm)), 3.0 * 6.116438863872),
          (Seq(3.0.cu_mi, 3.0 cu_mi, 3.0 (cu_mi)), 3.0 * 4168181825.440579584),

          (Seq(3.0.fbm, 3.0 fbm, 3.0 (fbm)), 3.0 * 2.359737216e-3),
          (Seq(3.0.ac_ft, 3.0 ac_ft, 3.0 (ac_ft)), 3.0 * 1233.48183754752),

          (Seq(3.0.beer_gal, 3.0 beer_gal, 3.0 (beer_gal)), 3.0 * 4.621152048e-3),
          (Seq(3.0.per, 3.0 per, 3.0 (per)), 3.0 * 0.700841953152),
          (Seq(3.0.bl, 3.0 bl, 3.0 (bl)), 3.0 * 0.158987294928),
          (Seq(3.0.bbl, 3.0 bbl, 3.0 (bbl)), 3.0 * 0.158987294928),

          // US
          (Seq(3.0.minim(US), 3.0 minim (US), 3.0 (minim(US))), 3.0 * 61.611519921875e-9),
          (Seq(3.0.US_fl_oz, 3.0 US_fl_oz, 3.0 (US_fl_oz)), 3.0 * 29.5735295625e-6),
          (Seq(3.0.fl_oz(US), 3.0 fl_oz (US), 3.0 (fl_oz(US))), 3.0 * 29.5735295625e-6),
          (Seq(3.0.gi(US), 3.0 gi (US), 3.0 (gi(US))), 3.0 * 118.29411825e-6),
          (Seq(3.0.gal(US), 3.0 gal (US), 3.0 (gal(US))), 3.0 * 3.785411784e-3),
          (Seq(3.0.hhd(US), 3.0 hhd (US), 3.0 (hhd(US))), 3.0 * 0.238480942392),

          // US fluid
          (Seq(3.0.pt(US_fl), 3.0 pt (US_fl), 3.0 (pt(US_fl))), 3.0 * 473.176473e-6),
          (Seq(3.0.qt(US_fl), 3.0 qt (US_fl), 3.0 (qt(US_fl))), 3.0 * 946.352946e-6),
          (Seq(3.0.gal, 3.0 gal, 3.0 (gal)), 3.0 * 3.785411784e-3),
          (Seq(3.0.US_gal, 3.0 US_gal, 3.0 (US_gal)), 3.0 * 3.785411784e-3),
          (Seq(3.0.gal(US_fl), 3.0 gal (US_fl), 3.0 (gal(US_fl))), 3.0 * 3.785411784e-3),
          (Seq(3.0.bl(US_fl), 3.0 bl (US_fl), 3.0 (bl(US_fl))), 3.0 * 0.119240471196),
          (Seq(3.0.fl_bl, 3.0 fl_bl, 3.0 (fl_bl)), 3.0 * 0.119240471196),
          (Seq(3.0.fl_bl(US), 3.0 fl_bl (US), 3.0 (fl_bl(US))), 3.0 * 0.119240471196),
          (Seq(3.0.fl_dr(US), 3.0 fl_dr (US), 3.0 (fl_dr(US))), 3.0 * 3.6966911953125e-6),

          // US dry
          (Seq(3.0.pt(US_dry), 3.0 pt (US_dry), 3.0 (pt(US_dry))), 3.0 * 550.6104713575e-6),
          (Seq(3.0.qt(US_dry), 3.0 qt (US_dry), 3.0 (qt(US_dry))), 3.0 * 1.101220942715e-3),
          (Seq(3.0.gal(US_dry), 3.0 gal (US_dry), 3.0 (gal(US_dry))), 3.0 * 4.40488377086e-3),
          (Seq(3.0.pk(US_dry), 3.0 pk (US_dry), 3.0 (pk(US_dry))), 3.0 * 8.80976754172e-3),
          (Seq(3.0.bu(US_dry), 3.0 bu (US_dry), 3.0 (bu(US_dry))), 3.0 * 0.0440488377086),
          (Seq(3.0.bl(US_dry), 3.0 bl (US_dry), 3.0 (bl(US_dry))), 3.0 * 0.115628198985075),
          (Seq(3.0.bu(US_lvl), 3.0 bu (US_lvl), 3.0 (bu(US_lvl))), 3.0 * 0.03523907016688),

          // imperial
          (Seq(3.0.minim(imp), 3.0 minim (imp), 3.0 (minim(imp))), 3.0 * 59.1938802083e-9),
          (Seq(3.0.fl_oz(imp), 3.0 fl_oz (imp), 3.0 (fl_oz(imp))), 3.0 * 28.4130625e-6),
          (Seq(3.0.gi(imp), 3.0 gi (imp), 3.0 (gi(imp))), 3.0 * 142.0653125e-6),
          (Seq(3.0.nog, 3.0 nog, 3.0 (nog)), 3.0 * 142.0653125e-6),
          (Seq(3.0.pt(imp), 3.0 pt (imp), 3.0 (pt(imp))), 3.0 * 568.26125e-6),
          (Seq(3.0.qt(imp), 3.0 qt (imp), 3.0 (qt(imp))), 3.0 * 1.1365225e-3),
          (Seq(3.0.imp_gal, 3.0 imp_gal, 3.0 (imp_gal)), 3.0 * 4.54609e-3),
          (Seq(3.0.gal(imp), 3.0 gal (imp), 3.0 (gal(imp))), 3.0 * 4.54609e-3),
          (Seq(3.0.pk(imp), 3.0 pk (imp), 3.0 (pk(imp))), 3.0 * 9.09218e-3),
          (Seq(3.0.bu(imp), 3.0 bu (imp), 3.0 (bu(imp))), 3.0 * 0.03636872),
          (Seq(3.0.bl(imp), 3.0 bl (imp), 3.0 (bl(imp))), 3.0 * 0.16365924),
          (Seq(3.0.hhd(imp), 3.0 hhd (imp), 3.0 (hhd(imp))), 3.0 * 0.32731848),

          (Seq(3.0.fl_s, 3.0 fl_s, 3.0 (fl_s)), 3.0 * 1.18387760417e-6),
          (Seq(3.0.fl_dr(imp), 3.0 fl_dr (imp), 3.0 (fl_dr(imp))), 3.0 * 3.5516328125e-6),

          (Seq(3.0.bkt, 3.0 bkt, 3.0 (bkt)), 3.0 * 0.01818436)
        )
      __Verify__
      forAll(conversions) { (suts: Seq[Volume[Double]], expected: Double) =>
        suts.foreach { sut =>
          (sut m3) should equal(%%%%(expected))
        }
      }
    }

    "Tests where converting cubic metre unit to other units like 3.0 m3 => 3.0 * 1e-6 km3" in {
      __SetUp__
      val value = 3.0 (m3)
      __Exercise__
      val conversions =
        Table(
          ("volumes", "expected"),
          (Seq(value.ym3, value ym3, value(ym3)), 3e72),
          (Seq(value.zm3, value zm3, value(zm3)), 3e63),
          (Seq(value.am3, value am3, value(am3)), 3e54),
          (Seq(value.fm3, value fm3, value(fm3)), 3e45),
          (Seq(value.pm3, value pm3, value(pm3)), 3e36),
          (Seq(value.nm3, value nm3, value(nm3)), 3e27),
          (Seq(value.μm3, value μm3, value(μm3)), 3e18),
          (Seq(value.mm3, value mm3, value(mm3)), 3e9),
          (Seq(value.cm3, value cm3, value(cm3)), 3e6),
          (Seq(value.dm3, value dm3, value(dm3)), 3e3),
          (Seq(value.m3, value m3, value(m3)), 3.0),
          (Seq(value.dam3, value dam3, value(dam3)), 3e-3),
          (Seq(value.hm3, value hm3, value(hm3)), 3e-6),
          (Seq(value.km3, value km3, value(km3)), 3e-9),
          (Seq(value.Mm3, value Mm3, value(Mm3)), 3e-18),
          (Seq(value.Gm3, value Gm3, value(Gm3)), 3e-27),
          (Seq(value.Tm3, value Tm3, value(Tm3)), 3e-36),
          (Seq(value.Pm3, value Pm3, value(Pm3)), 3e-45),
          (Seq(value.Em3, value Em3, value(Em3)), 3e-54),
          (Seq(value.Zm3, value Zm3, value(Zm3)), 3e-63),
          (Seq(value.Ym3, value Ym3, value(Ym3)), 3e-72),

          (Seq(value.yL, value yL, value(yL)), 3e27),
          (Seq(value.zL, value zL, value(zL)), 3e24),
          (Seq(value.aL, value aL, value(aL)), 3e21),
          (Seq(value.fL, value fL, value(fL)), 3e18),
          (Seq(value.pL, value pL, value(pL)), 3e15),
          (Seq(value.nL, value nL, value(nL)), 3e12),
          (Seq(value.μL, value μL, value(μL)), 3e9),
          (Seq(value.mL, value mL, value(mL)), 3e6),
          (Seq(value.cL, value cL, value(cL)), 3e5),
          (Seq(value.dL, value dL, value(dL)), 3e4),
          (Seq(value.L, value L, value(L)), 3e3),
          (Seq(value.daL, value daL, value(daL)), 3e2),
          (Seq(value.hL, value hL, value(hL)), 3e1),
          (Seq(value.kL, value kL, value(kL)), 3.0),
          (Seq(value.ML, value ML, value(ML)), 3e-3),
          (Seq(value.GL, value GL, value(GL)), 3e-6),
          (Seq(value.TL, value TL, value(TL)), 3e-9),
          (Seq(value.PL, value PL, value(PL)), 3e-12),
          (Seq(value.EL, value EL, value(EL)), 3e-15),
          (Seq(value.ZL, value ZL, value(ZL)), 3e-18),
          (Seq(value.YL, value YL, value(YL)), 3e-21),

          (Seq(value.λ, value λ, value(λ)), 3.0e9),

          (Seq(value.in3, value in3, value(in3)), 3.0 / 16.387064e-6),
          (Seq(value.ft3, value ft3, value(ft3)), 3.0 / 0.028316846592),
          (Seq(value.yd3, value yd3, value(yd3)), 3.0 / 0.764554857984),
          (Seq(value.ftm3, value ftm3, value(ftm3)), 3.0 / 6.116438863872),
          (Seq(value.mi3, value mi3, value(mi3)), 3.0 / 4168181825.440579584),
          (Seq(value.cu_in, value cu_in, value(cu_in)), 3.0 / 16.387064e-6),
          (Seq(value.cu_ft, value cu_ft, value(cu_ft)), 3.0 / 0.028316846592),
          (Seq(value.cu_yd, value cu_yd, value(cu_yd)), 3.0 / 0.764554857984),
          (Seq(value.cu_fm, value cu_fm, value(cu_fm)), 3.0 / 6.116438863872),
          (Seq(value.cu_mi, value cu_mi, value(cu_mi)), 3.0 / 4168181825.440579584),

          (Seq(value.fbm, value fbm, value(fbm)), 3.0 / 2.359737216e-3),
          (Seq(value.ac_ft, value ac_ft, value(ac_ft)), 3.0 / 1233.48183754752),

          (Seq(value.beer_gal, value beer_gal, value(beer_gal)), 3.0 / 4.621152048e-3),
          (Seq(value.per, value per, value(per)), 3.0 / 0.700841953152),
          (Seq(value.bl, value bl, value(bl)), 3.0 / 0.158987294928),
          (Seq(value.bbl, value bbl, value(bbl)), 3.0 / 0.158987294928),

          // US
          (Seq(value.minim(US), value minim (US), value(minim(US))), 3.0 / 61.611519921875e-9),
          (Seq(value.US_fl_oz, value US_fl_oz, value(US_fl_oz)), 3.0 / 29.5735295625e-6),
          (Seq(value.fl_oz(US), value fl_oz (US), value(fl_oz(US))), 3.0 / 29.5735295625e-6),
          (Seq(value.gi(US), value gi (US), value(gi(US))), 3.0 / 118.29411825e-6),
          (Seq(value.gal(US), value gal (US), value(gal(US))), 3.0 / 3.785411784e-3),
          (Seq(value.hhd(US), value hhd (US), value(hhd(US))), 3.0 / 0.238480942392),

          // US fluid
          (Seq(value.pt(US_fl), value pt (US_fl), value(pt(US_fl))), 3.0 / 473.176473e-6),
          (Seq(value.qt(US_fl), value qt (US_fl), value(qt(US_fl))), 3.0 / 946.352946e-6),
          (Seq(value.gal, value gal, value(gal)), 3.0 / 3.785411784e-3),
          (Seq(value.US_gal, value US_gal, value(US_gal)), 3.0 / 3.785411784e-3),
          (Seq(value.gal(US_fl), value gal (US_fl), value(gal(US_fl))), 3.0 / 3.785411784e-3),
          (Seq(value.bl(US_fl), value bl (US_fl), value(bl(US_fl))), 3.0 / 0.119240471196),
          (Seq(value.fl_bl, value fl_bl, value(fl_bl)), 3.0 / 0.119240471196),
          (Seq(value.fl_bl(US), value fl_bl (US), value(fl_bl(US))), 3.0 / 0.119240471196),
          (Seq(value.fl_dr(US), value fl_dr (US), value(fl_dr(US))), 3.0 / 3.6966911953125e-6),

          // US dry
          (Seq(value.pt(US_dry), value pt (US_dry), value(pt(US_dry))), 3.0 / 550.6104713575e-6),
          (Seq(value.qt(US_dry), value qt (US_dry), value(qt(US_dry))), 3.0 / 1.101220942715e-3),
          (Seq(value.gal(US_dry), value gal (US_dry), value(gal(US_dry))), 3.0 / 4.40488377086e-3),
          (Seq(value.pk(US_dry), value pk (US_dry), value(pk(US_dry))), 3.0 / 8.80976754172e-3),
          (Seq(value.bu(US_dry), value bu (US_dry), value(bu(US_dry))), 3.0 / 0.0440488377086),
          (Seq(value.bl(US_dry), value bl (US_dry), value(bl(US_dry))), 3.0 / 0.115628198985075),
          (Seq(value.bu(US_lvl), value bu (US_lvl), value(bu(US_lvl))), 3.0 / 0.03523907016688),

          // imperial
          (Seq(value.minim(imp), value minim (imp), value(minim(imp))), 3.0 / 59.1938802083e-9),
          (Seq(value.fl_oz(imp), value fl_oz (imp), value(fl_oz(imp))), 3.0 / 28.4130625e-6),
          (Seq(value.gi(imp), value gi (imp), value(gi(imp))), 3.0 / 142.0653125e-6),
          (Seq(value.nog, value nog, value(nog)), 3.0 / 142.0653125e-6),
          (Seq(value.pt(imp), value pt (imp), value(pt(imp))), 3.0 / 568.26125e-6),
          (Seq(value.qt(imp), value qt (imp), value(qt(imp))), 3.0 / 1.1365225e-3),
          (Seq(value.imp_gal, value imp_gal, value(imp_gal)), 3.0 / 4.54609e-3),
          (Seq(value.gal(imp), value gal (imp), value(gal(imp))), 3.0 / 4.54609e-3),
          (Seq(value.pk(imp), value pk (imp), value(pk(imp))), 3.0 / 9.09218e-3),
          (Seq(value.bu(imp), value bu (imp), value(bu(imp))), 3.0 / 0.03636872),
          (Seq(value.bl(imp), value bl (imp), value(bl(imp))), 3.0 / 0.16365924),
          (Seq(value.hhd(imp), value hhd (imp), value(hhd(imp))), 3.0 / 0.32731848),

          (Seq(value.fl_s, value fl_s, value(fl_s)), 3.0 / 1.18387760417e-6),
          (Seq(value.fl_dr(imp), value fl_dr (imp), value(fl_dr(imp))), 3.0 / 3.5516328125e-6),

          (Seq(value.bkt, value bkt, value(bkt)), 3.0 / 0.01818436)
        )
      __Verify__
      forAll(conversions) { (suts: Seq[Double], expected: Double) =>
        suts.foreach { sut =>
          sut should equal(%%%%(expected))
        }
      }
    }
  }

  "product volume unit" - {

    "Area unit of ac*ft should equal 1233.48183754752 m3" in {
      __Exercise__
      val sut = ac*ft
      __Verify__
      sut.unitInCubicMetre.toDouble should equal (%%%%(1233.48183754752))
    }

    "3.0 ac*ft should equal 3.0 * 43560.0 cu_ft" in {
      __Exercise__
      val conversions =
        Table(
          ("area", "expected"),
          (3.0.ac*ft, 0.03 * 43560.0),
          (3.0 ac*ft, 0.03 * 43560.0),
          (3.0 (ac*ft), 0.03 * 43560.0)
        )
      __Verify__
      forAll(conversions){ (sut: Volume[Double], expected: Double) =>
        sut.cu_ft should equal (%%%%(expected))
      }
    }

    "3.0 m3 should equal 3.0 / 1233.48183754752 ac*ft" in {
      __SetUp__
      val value = 3.0 (m3)
      val expected = 3.0 / 1233.48183754752
      __Exercise__
      val conversions =
        Table(
          ("area", "expected"),
          (value.ac*ft, expected),
          (value ac*ft, expected),
          (value (ac*ft), expected)
        )
      __Verify__
      forAll(conversions){ (sut: Double, expected: Double) =>
        sut should equal (%%%%(expected))
      }
    }
  }
}
