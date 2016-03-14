package org.waman.multiverse.metric

import org.scalatest.prop.PropertyChecks
import org.waman.multiverse.AbstractQuantityAndUnitSpec
import org.waman.multiverse.UnitSystem._

import scala.language.postfixOps

/**
  * Expected values are from
  * <a href="https://en.wikipedia.org/wiki/Conversion_of_units">Conversion of units</a>.
  */
class VolumeSpec
  extends AbstractQuantityAndUnitSpec[VolumeUnit]
    with PropertyChecks{

  override protected val getUnitClass = classOf[VolumeUnit]

  "Predefined volume units" - {

    "3.0 <<volume unit>> should be converted to the equivalent value in cubic metre" in {
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
          (Seq(3.0.microCubicMetre, 3.0 microCubicMetre, 3.0 (microCubicMetre)), 3e-18),
          (Seq(3.0.microM3, 3.0 microM3, 3.0 (microM3)), 3e-18),
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
          (Seq(3.0.microLitre, 3.0 microLitre, 3.0 (microLitre)), 3e-9),
          (Seq(3.0.microL, 3.0 microL, 3.0 (microL)), 3e-9),
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

          (Seq(3.0.lambda, 3.0 lambda, 3.0 (lambda)), 3e-9),
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

    "3.0 m3 should be converted to the equivalent value in other volume units" in {
      __SetUp__
      val q = 3.0 (m3)
      __Exercise__
      val conversions =
        Table(
          ("volumes", "expected"),
          (Seq(q.ym3, q ym3, q(ym3)), 3e72),
          (Seq(q.zm3, q zm3, q(zm3)), 3e63),
          (Seq(q.am3, q am3, q(am3)), 3e54),
          (Seq(q.fm3, q fm3, q(fm3)), 3e45),
          (Seq(q.pm3, q pm3, q(pm3)), 3e36),
          (Seq(q.nm3, q nm3, q(nm3)), 3e27),
          (Seq(q.microCubicMetre, q microCubicMetre, q(microCubicMetre)), 3e18),
          (Seq(q.microM3, q microM3, q(microM3)), 3e18),
          (Seq(q.μm3, q μm3, q(μm3)), 3e18),
          (Seq(q.mm3, q mm3, q(mm3)), 3e9),
          (Seq(q.cm3, q cm3, q(cm3)), 3e6),
          (Seq(q.dm3, q dm3, q(dm3)), 3e3),
          (Seq(q.m3, q m3, q(m3)), 3.0),
          (Seq(q.dam3, q dam3, q(dam3)), 3e-3),
          (Seq(q.hm3, q hm3, q(hm3)), 3e-6),
          (Seq(q.km3, q km3, q(km3)), 3e-9),
          (Seq(q.Mm3, q Mm3, q(Mm3)), 3e-18),
          (Seq(q.Gm3, q Gm3, q(Gm3)), 3e-27),
          (Seq(q.Tm3, q Tm3, q(Tm3)), 3e-36),
          (Seq(q.Pm3, q Pm3, q(Pm3)), 3e-45),
          (Seq(q.Em3, q Em3, q(Em3)), 3e-54),
          (Seq(q.Zm3, q Zm3, q(Zm3)), 3e-63),
          (Seq(q.Ym3, q Ym3, q(Ym3)), 3e-72),

          (Seq(q.yL, q yL, q(yL)), 3e27),
          (Seq(q.zL, q zL, q(zL)), 3e24),
          (Seq(q.aL, q aL, q(aL)), 3e21),
          (Seq(q.fL, q fL, q(fL)), 3e18),
          (Seq(q.pL, q pL, q(pL)), 3e15),
          (Seq(q.nL, q nL, q(nL)), 3e12),
          (Seq(q.microLitre, q microLitre, q(microLitre)), 3e9),
          (Seq(q.microL, q microL, q(microL)), 3e9),
          (Seq(q.μL, q μL, q(μL)), 3e9),
          (Seq(q.mL, q mL, q(mL)), 3e6),
          (Seq(q.cL, q cL, q(cL)), 3e5),
          (Seq(q.dL, q dL, q(dL)), 3e4),
          (Seq(q.L, q L, q(L)), 3e3),
          (Seq(q.daL, q daL, q(daL)), 3e2),
          (Seq(q.hL, q hL, q(hL)), 3e1),
          (Seq(q.kL, q kL, q(kL)), 3.0),
          (Seq(q.ML, q ML, q(ML)), 3e-3),
          (Seq(q.GL, q GL, q(GL)), 3e-6),
          (Seq(q.TL, q TL, q(TL)), 3e-9),
          (Seq(q.PL, q PL, q(PL)), 3e-12),
          (Seq(q.EL, q EL, q(EL)), 3e-15),
          (Seq(q.ZL, q ZL, q(ZL)), 3e-18),
          (Seq(q.YL, q YL, q(YL)), 3e-21),

          (Seq(q.lambda, q lambda, q(lambda)), 3.0e9),
          (Seq(q.λ, q λ, q(λ)), 3.0e9),

          (Seq(q.in3, q in3, q(in3)), 3.0 / 16.387064e-6),
          (Seq(q.ft3, q ft3, q(ft3)), 3.0 / 0.028316846592),
          (Seq(q.yd3, q yd3, q(yd3)), 3.0 / 0.764554857984),
          (Seq(q.ftm3, q ftm3, q(ftm3)), 3.0 / 6.116438863872),
          (Seq(q.mi3, q mi3, q(mi3)), 3.0 / 4168181825.440579584),
          (Seq(q.cu_in, q cu_in, q(cu_in)), 3.0 / 16.387064e-6),
          (Seq(q.cu_ft, q cu_ft, q(cu_ft)), 3.0 / 0.028316846592),
          (Seq(q.cu_yd, q cu_yd, q(cu_yd)), 3.0 / 0.764554857984),
          (Seq(q.cu_fm, q cu_fm, q(cu_fm)), 3.0 / 6.116438863872),
          (Seq(q.cu_mi, q cu_mi, q(cu_mi)), 3.0 / 4168181825.440579584),

          (Seq(q.fbm, q fbm, q(fbm)), 3.0 / 2.359737216e-3),

          (Seq(q.beer_gal, q beer_gal, q(beer_gal)), 3.0 / 4.621152048e-3),
          (Seq(q.per, q per, q(per)), 3.0 / 0.700841953152),
          (Seq(q.bl, q bl, q(bl)), 3.0 / 0.158987294928),
          (Seq(q.bbl, q bbl, q(bbl)), 3.0 / 0.158987294928),

          // US
          (Seq(q.minim(US), q minim (US), q(minim(US))), 3.0 / 61.611519921875e-9),
          (Seq(q.US_fl_oz, q US_fl_oz, q(US_fl_oz)), 3.0 / 29.5735295625e-6),
          (Seq(q.fl_oz(US), q fl_oz (US), q(fl_oz(US))), 3.0 / 29.5735295625e-6),
          (Seq(q.gi(US), q gi (US), q(gi(US))), 3.0 / 118.29411825e-6),
          (Seq(q.gal(US), q gal (US), q(gal(US))), 3.0 / 3.785411784e-3),
          (Seq(q.hhd(US), q hhd (US), q(hhd(US))), 3.0 / 0.238480942392),

          // US fluid
          (Seq(q.pt(US_fl), q pt (US_fl), q(pt(US_fl))), 3.0 / 473.176473e-6),
          (Seq(q.qt(US_fl), q qt (US_fl), q(qt(US_fl))), 3.0 / 946.352946e-6),
          (Seq(q.gal, q gal, q(gal)), 3.0 / 3.785411784e-3),
          (Seq(q.US_gal, q US_gal, q(US_gal)), 3.0 / 3.785411784e-3),
          (Seq(q.gal(US_fl), q gal (US_fl), q(gal(US_fl))), 3.0 / 3.785411784e-3),
          (Seq(q.bl(US_fl), q bl (US_fl), q(bl(US_fl))), 3.0 / 0.119240471196),
          (Seq(q.fl_bl, q fl_bl, q(fl_bl)), 3.0 / 0.119240471196),
          (Seq(q.fl_bl(US), q fl_bl (US), q(fl_bl(US))), 3.0 / 0.119240471196),
          (Seq(q.fl_dr(US), q fl_dr (US), q(fl_dr(US))), 3.0 / 3.6966911953125e-6),

          // US dry
          (Seq(q.pt(US_dry), q pt (US_dry), q(pt(US_dry))), 3.0 / 550.6104713575e-6),
          (Seq(q.qt(US_dry), q qt (US_dry), q(qt(US_dry))), 3.0 / 1.101220942715e-3),
          (Seq(q.gal(US_dry), q gal (US_dry), q(gal(US_dry))), 3.0 / 4.40488377086e-3),
          (Seq(q.pk(US_dry), q pk (US_dry), q(pk(US_dry))), 3.0 / 8.80976754172e-3),
          (Seq(q.bu(US_dry), q bu (US_dry), q(bu(US_dry))), 3.0 / 0.0440488377086),
          (Seq(q.bl(US_dry), q bl (US_dry), q(bl(US_dry))), 3.0 / 0.115628198985075),
          (Seq(q.bu(US_lvl), q bu (US_lvl), q(bu(US_lvl))), 3.0 / 0.03523907016688),

          // imperial
          (Seq(q.minim(imp), q minim (imp), q(minim(imp))), 3.0 / 59.1938802083e-9),
          (Seq(q.fl_oz(imp), q fl_oz (imp), q(fl_oz(imp))), 3.0 / 28.4130625e-6),
          (Seq(q.gi(imp), q gi (imp), q(gi(imp))), 3.0 / 142.0653125e-6),
          (Seq(q.nog, q nog, q(nog)), 3.0 / 142.0653125e-6),
          (Seq(q.pt(imp), q pt (imp), q(pt(imp))), 3.0 / 568.26125e-6),
          (Seq(q.qt(imp), q qt (imp), q(qt(imp))), 3.0 / 1.1365225e-3),
          (Seq(q.imp_gal, q imp_gal, q(imp_gal)), 3.0 / 4.54609e-3),
          (Seq(q.gal(imp), q gal (imp), q(gal(imp))), 3.0 / 4.54609e-3),
          (Seq(q.pk(imp), q pk (imp), q(pk(imp))), 3.0 / 9.09218e-3),
          (Seq(q.bu(imp), q bu (imp), q(bu(imp))), 3.0 / 0.03636872),
          (Seq(q.bl(imp), q bl (imp), q(bl(imp))), 3.0 / 0.16365924),
          (Seq(q.hhd(imp), q hhd (imp), q(hhd(imp))), 3.0 / 0.32731848),

          (Seq(q.fl_s, q fl_s, q(fl_s)), 3.0 / 1.18387760417e-6),
          (Seq(q.fl_dr(imp), q fl_dr (imp), q(fl_dr(imp))), 3.0 / 3.5516328125e-6),

          (Seq(q.bkt, q bkt, q(bkt)), 3.0 / 0.01818436)
        )
      __Verify__
      forAll(conversions) { (suts: Seq[Double], expected: Double) =>
        suts.foreach { sut =>
          sut should equal(%%%%(expected))
        }
      }
    }
  }

  "Product volume unit" - {

    "Volume unit of ac*ft should equal 1233.48183754752 m3" in {
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
          (3.0.ac*ft, 3.0 * 43560.0),
          (3.0 ac*ft, 3.0 * 43560.0),
          (3.0 (ac*ft), 3.0 * 43560.0)
        )
      __Verify__
      forAll(conversions){ (sut: Volume[Double], expected: Double) =>
        sut.cu_ft should equal (%%%%(expected))
      }
    }

    "3.0 m3 should equal 3.0 / 1233.48183754752 ac*ft" in {
      __SetUp__
      val q = 3.0 (m3)
      val expected = 3.0 / 1233.48183754752
      __Exercise__
      val conversions =
        Table(
          ("area", "expected"),
          (q.ac*ft, expected),
          (q ac*ft, expected),
          (q (ac*ft), expected)
        )
      __Verify__
      forAll(conversions){ (sut: Double, expected: Double) =>
        sut should equal (%%%%(expected))
      }
    }
  }
}
