package org.waman.multiverse.metric

import org.scalatest.prop.PropertyChecks
import org.waman.multiverse.UnitSystem._
import org.waman.multiverse.{AbstractQuantityAndUnitSpec, UnitSystem}
import spire.implicits._

import scala.language.postfixOps

/**
  * Expected values are from
  * <a href="https://en.wikipedia.org/wiki/Conversion_of_units">Conversion of units</a>.
  */
class AreaSpec
  extends AbstractQuantityAndUnitSpec[AreaUnit]
    with PropertyChecks{

  override protected val getUnitClass = classOf[AreaUnit]

  val usFoot = 1200.0 / 3937.0

  "Predefined area units" - {

    "Tests where converting from some units to square metre like 3.0 km2 => 3e6 m2" in {
      __SetUp__
      import UnitSystem.{a => are}
      __Exercise__
      val conversions =
        Table(
          ("areas", "expected"),
          (Seq(3.0.ym2, 3.0 ym2, 3.0 (ym2)), 3e-48),
          (Seq(3.0.zm2, 3.0 zm2, 3.0 (zm2)), 3e-42),
          (Seq(3.0.am2, 3.0 am2, 3.0 (am2)), 3e-36),
          (Seq(3.0.fm2, 3.0 fm2, 3.0 (fm2)), 3e-30),
          (Seq(3.0.pm2, 3.0 pm2, 3.0 (pm2)), 3e-24),
          (Seq(3.0.nm2, 3.0 nm2, 3.0 (nm2)), 3e-18),
          (Seq(3.0.μm2, 3.0 μm2, 3.0 (μm2)), 3e-12),
          (Seq(3.0.mm2, 3.0 mm2, 3.0 (mm2)), 3e-6),
          (Seq(3.0.cm2, 3.0 cm2, 3.0 (cm2)), 3e-4),
          (Seq(3.0.dm2, 3.0 dm2, 3.0 (dm2)), 3e-2),
          (Seq(3.0.m2, 3.0 m2, 3.0 (m2)), 3.0),
          (Seq(3.0.dam2, 3.0 dam2, 3.0 (dam2)), 3e2),
          (Seq(3.0.hm2, 3.0 hm2, 3.0 (hm2)), 3e4),
          (Seq(3.0.km2, 3.0 km2, 3.0 (km2)), 3e6),
          (Seq(3.0.Mm2, 3.0 Mm2, 3.0 (Mm2)), 3e12),
          (Seq(3.0.Gm2, 3.0 Gm2, 3.0 (Gm2)), 3e18),
          (Seq(3.0.Tm2, 3.0 Tm2, 3.0 (Tm2)), 3e24),
          (Seq(3.0.Pm2, 3.0 Pm2, 3.0 (Pm2)), 3e30),
          (Seq(3.0.Em2, 3.0 Em2, 3.0 (Em2)), 3e36),
          (Seq(3.0.Zm2, 3.0 Zm2, 3.0 (Zm2)), 3e42),
          (Seq(3.0.Ym2, 3.0 Ym2, 3.0 (Ym2)), 3e48),

          (Seq(3.0.a, 3.0 a, 3.0 (are)), 3e2),
          (Seq(3.0.ha, 3.0 ha, 3.0 (ha)), 3e4),

          (Seq(3.0.yb, 3.0 yb, 3.0 (yb)), 3e-52),
          (Seq(3.0.zb, 3.0 zb, 3.0 (zb)), 3e-49),
          (Seq(3.0.ab, 3.0 ab, 3.0 (ab)), 3e-46),
          (Seq(3.0.fb, 3.0 fb, 3.0 (fb)), 3e-43),
          (Seq(3.0.pb, 3.0 pb, 3.0 (pb)), 3e-40),
          (Seq(3.0.nb, 3.0 nb, 3.0 (nb)), 3e-37),
          (Seq(3.0.μb, 3.0 μb, 3.0 (μb)), 3e-34),
          (Seq(3.0.mb, 3.0 mb, 3.0 (mb)), 3e-31),
          (Seq(3.0.b, 3.0 b, 3.0 (b)), 3e-28),
          (Seq(3.0.kb, 3.0 kb, 3.0 (kb)), 3e-25),
          (Seq(3.0.Mb, 3.0 Mb, 3.0 (Mb)), 3e-22),
          (Seq(3.0.Gb, 3.0 Gb, 3.0 (Gb)), 3e-19),
          (Seq(3.0.Tb, 3.0 Tb, 3.0 (Tb)), 3e-16),
          (Seq(3.0.Pb, 3.0 Pb, 3.0 (Pb)), 3e-13),
          (Seq(3.0.Eb, 3.0 Eb, 3.0 (Eb)), 3e-10),
          (Seq(3.0.Zb, 3.0 Zb, 3.0 (Zb)), 3e-7),
          (Seq(3.0.Yb, 3.0 Yb, 3.0 (Yb)), 3e-4),

          (Seq(3.0.sq_mil, 3.0 sq_mil, 3.0 (sq_mil)), 3.0 * 6.4516e-10),
          (Seq(3.0.sq_in, 3.0 sq_in, 3.0 (sq_in)), 3.0 * 6.4516e-4),
          (Seq(3.0.sq_lnk, 3.0 sq_lnk, 3.0 (sq_lnk)), 3.0 * 4.0468564224e-2),
          (Seq(3.0.sq_ft, 3.0 sq_ft, 3.0 (sq_ft)), 3.0 * 9.290304e-2),
          (Seq(3.0.sq_ch, 3.0 sq_ch, 3.0 (sq_ch)), 3.0 * 404.68564224),
          (Seq(3.0.sq_yd, 3.0 sq_yd, 3.0 (sq_yd)), 3.0 * 0.83612736),
          (Seq(3.0.sq_rd, 3.0 sq_rd, 3.0 (sq_rd)), 3.0 * 25.29285264),
          (Seq(3.0.sq_mi, 3.0 sq_mi, 3.0 (sq_mi)), 3.0 * 2.589988110336e6),
          (Seq(3.0.ac, 3.0 ac, 3.0 (ac)), 3.0 * 4046.8564224),
          (Seq(3.0.ro, 3.0 ro, 3.0 (ro)), 3.0 * 1011.7141056),

          (Seq(3.0.sq_lnk(US), 3.0 sq_lnk (US), 3.0 (sq_lnk(US))), 3.0 * Math.pow(0.66 * usFoot, 2)),
          (Seq(3.0.sq_ft(US), 3.0 sq_ft (US), 3.0 (sq_ft(US))), 3.0 * Math.pow(usFoot, 2)),
          (Seq(3.0.sq_ch(US), 3.0 sq_ch (US), 3.0 (sq_ch(US))), 3.0 * Math.pow(66 * usFoot, 2)),
          (Seq(3.0.sq_mi(US), 3.0 sq_mi (US), 3.0 (sq_mi(US))), 3.0 * Math.pow(5280 * usFoot, 2)),
          (Seq(3.0.ac(US), 3.0 ac (US), 3.0 (ac(US))), 3.0 * 10.0 * Math.pow(66 * usFoot, 2)),

          (Seq(3.0.circ_mil, 3.0 circ_mil, 3.0 (circ_mil)), 3.0 * Math.PI * Math.pow(2.54e-5, 2) / 4.0),
          (Seq(3.0.circ_in, 3.0 circ_in, 3.0 (circ_in)), 3.0 * Math.PI * Math.pow(0.0254, 2) / 4.0),

          (Seq(3.0.bd, 3.0 bd, 3.0 (bd)), 3.0 * 7.74192e-3)
        )
      __Verify__
      forAll(conversions) { (suts: Seq[Area[Double]], expected: Double) =>
        suts.foreach { sut =>
          (sut m2) should equal(%%%%(expected))
        }
      }
    }

    "Tests where converting square metre unit to other units like 3.0 m2 => 3.0 * 1e-6 km2" in {
      __SetUp__
      import UnitSystem.{a => are}
      val value = 3.0 (m2)
      __Exercise__
      val conversions =
        Table(
          ("areas", "expected"),
          (Seq(value.ym2, value ym2, value(ym2)), 3e48),
          (Seq(value.zm2, value zm2, value(zm2)), 3e42),
          (Seq(value.am2, value am2, value(am2)), 3e36),
          (Seq(value.fm2, value fm2, value(fm2)), 3e30),
          (Seq(value.pm2, value pm2, value(pm2)), 3e24),
          (Seq(value.nm2, value nm2, value(nm2)), 3e18),
          (Seq(value.μm2, value μm2, value(μm2)), 3e12),
          (Seq(value.mm2, value mm2, value(mm2)), 3e6),
          (Seq(value.cm2, value cm2, value(cm2)), 3e4),
          (Seq(value.dm2, value dm2, value(dm2)), 3e2),
          (Seq(value.m2, value m2, value(m2)), 3.0),
          (Seq(value.dam2, value dam2, value(dam2)), 3e-2),
          (Seq(value.hm2, value hm2, value(hm2)), 3e-4),
          (Seq(value.km2, value km2, value(km2)), 3e-6),
          (Seq(value.Mm2, value Mm2, value(Mm2)), 3e-12),
          (Seq(value.Gm2, value Gm2, value(Gm2)), 3e-18),
          (Seq(value.Tm2, value Tm2, value(Tm2)), 3e-24),
          (Seq(value.Pm2, value Pm2, value(Pm2)), 3e-30),
          (Seq(value.Em2, value Em2, value(Em2)), 3e-36),
          (Seq(value.Zm2, value Zm2, value(Zm2)), 3e-42),
          (Seq(value.Ym2, value Ym2, value(Ym2)), 3e-48),

          (Seq(value.a, value a, value(are)), 3e-2),
          (Seq(value.ha, value ha, value(ha)), 3e-4),

          (Seq(value.yb, value yb, value(yb)), 3e52),
          (Seq(value.zb, value zb, value(zb)), 3e49),
          (Seq(value.ab, value ab, value(ab)), 3e46),
          (Seq(value.fb, value fb, value(fb)), 3e43),
          (Seq(value.pb, value pb, value(pb)), 3e40),
          (Seq(value.nb, value nb, value(nb)), 3e37),
          (Seq(value.μb, value μb, value(μb)), 3e34),
          (Seq(value.mb, value mb, value(mb)), 3e31),
          (Seq(value.b, value b, value(b)), 3e28),
          (Seq(value.kb, value kb, value(kb)), 3e25),
          (Seq(value.Mb, value Mb, value(Mb)), 3e22),
          (Seq(value.Gb, value Gb, value(Gb)), 3e19),
          (Seq(value.Tb, value Tb, value(Tb)), 3e16),
          (Seq(value.Pb, value Pb, value(Pb)), 3e13),
          (Seq(value.Eb, value Eb, value(Eb)), 3e10),
          (Seq(value.Zb, value Zb, value(Zb)), 3e7),
          (Seq(value.Yb, value Yb, value(Yb)), 3e4),

          (Seq(value.sq_mil, value sq_mil, value(sq_mil)), 3.0 / 6.4516e-10),
          (Seq(value.sq_in, value sq_in, value(sq_in)), 3.0 / 6.4516e-4),
          (Seq(value.sq_lnk, value sq_lnk, value(sq_lnk)), 3.0 / 4.0468564224e-2),
          (Seq(value.sq_ft, value sq_ft, value(sq_ft)), 3.0 / 9.290304e-2),
          (Seq(value.sq_ch, value sq_ch, value(sq_ch)), 3.0 / 404.68564224),
          (Seq(value.sq_yd, value sq_yd, value(sq_yd)), 3.0 / 0.83612736),
          (Seq(value.sq_rd, value sq_rd, value(sq_rd)), 3.0 / 25.29285264),
          (Seq(value.sq_mi, value sq_mi, value(sq_mi)), 3.0 / 2.589988110336e6),
          (Seq(value.ac, value ac, value(ac)), 3.0 / 4046.8564224),
          (Seq(value.ro, value ro, value(ro)), 3.0 / 1011.7141056),

          (Seq(value.sq_lnk(US), value sq_lnk (US), value(sq_lnk(US))), 3.0 / Math.pow(0.66 * usFoot, 2)),
          (Seq(value.sq_ft(US), value sq_ft (US), value(sq_ft(US))), 3.0 / Math.pow(usFoot, 2)),
          (Seq(value.sq_ch(US), value sq_ch (US), value(sq_ch(US))), 3.0 / Math.pow(66 * usFoot, 2)),
          (Seq(value.sq_mi(US), value sq_mi (US), value(sq_mi(US))), 3.0 / Math.pow(5280 * usFoot, 2)),
          (Seq(value.ac(US), value ac (US), value(ac(US))), 3.0 / (10.0 * Math.pow(66 * usFoot, 2))),

          (Seq(value.circ_mil, value circ_mil, value(circ_mil)), 3.0 * 4.0 / (Math.PI * Math.pow(2.54e-5, 2))),
          (Seq(value.circ_in, value circ_in, value(circ_in)), 3.0 * 4.0 / (Math.PI * Math.pow(0.0254, 2))),

          (Seq(value.bd, value bd, value(bd)), 3.0 / 7.74192e-3)
        )
      __Verify__
      forAll(conversions) { (suts: Seq[Double], expected: Double) =>
        suts.foreach { sut =>
          sut should equal(%%%%(expected))
        }
      }
    }
  }

  "Product area unit" - {

    "Area unit of m*cm should equal 0.01 m2" in {
      __Exercise__
      val sut = m*cm
      __Verify__
      sut.unitInSquareMetre should equal (r"0.01")
    }

    "3.0 m*cm should equal 0.03 m2" in {
      __Exercise__
      val conversions =
        Table(
          ("area", "expected"),
          (3.0.m*cm, 0.03.m2),
          (3.0 m*cm, 0.03.m2),
          (3.0 (m*cm), 0.03.m2)
        )
      __Verify__
      forAll(conversions){ (sut: Area[Double], expected: Area[Double]) =>
        sut should equal (expected)
      }
    }

    "3.0 m2 should equal 300.0 m*cm" in {
      __SetUp__
      val value = 3.0 (m2)
      __Exercise__
      val conversions =
        Table(
          ("area", "expected"),
          (value.m*cm, 300.0),
          (value m*cm, 300.0),
          (value (m*cm), 300.0)
        )
      __Verify__
      forAll(conversions){ (sut: Double, expected: Double) =>
        sut should equal (%%%%(expected))
      }
    }
  }
}
