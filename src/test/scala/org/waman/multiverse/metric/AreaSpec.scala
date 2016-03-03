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

    "3.0 <<area unit>> should be converted to the equivalent value in square metre" in {
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
          (Seq(3.0.microMetre2, 3.0 microMetre2, 3.0 (microMetre2)), 3e-12),
          (Seq(3.0.micrometre2, 3.0 micrometre2, 3.0 (micrometre2)), 3e-12),
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
          (Seq(3.0.microBarn, 3.0 microBarn, 3.0 (microBarn)), 3e-34),
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

          (Seq(3.0.mil2, 3.0 mil2, 3.0 (mil2)), 3.0 * 6.4516e-10),
          (Seq(3.0.in2 , 3.0 in2 , 3.0 (in2)) , 3.0 * 6.4516e-4),
          (Seq(3.0.li2 , 3.0 li2 , 3.0 (li2)) , 3.0 * 4.0468564224e-2),
          (Seq(3.0.lnk2, 3.0 lnk2, 3.0 (lnk2)), 3.0 * 4.0468564224e-2),
          (Seq(3.0.ft2 , 3.0 ft2 , 3.0 (ft2)) , 3.0 * 9.290304e-2),
          (Seq(3.0.ch2 , 3.0 ch2 , 3.0 (ch2)) , 3.0 * 404.68564224),
          (Seq(3.0.yd2 , 3.0 yd2 , 3.0 (yd2)) , 3.0 * 0.83612736),
          (Seq(3.0.rd2 , 3.0 rd2 , 3.0 (rd2)) , 3.0 * 25.29285264),
          (Seq(3.0.mi2 , 3.0 mi2 , 3.0 (mi2)) , 3.0 * 2.589988110336e6),

          (Seq(3.0.sq_mil, 3.0 sq_mil, 3.0 (sq_mil)), 3.0 * 6.4516e-10),
          (Seq(3.0.sq_in , 3.0 sq_in , 3.0 (sq_in)) , 3.0 * 6.4516e-4),
          (Seq(3.0.sq_li , 3.0 sq_li , 3.0 (sq_li)) , 3.0 * 4.0468564224e-2),
          (Seq(3.0.sq_lnk, 3.0 sq_lnk, 3.0 (sq_lnk)), 3.0 * 4.0468564224e-2),
          (Seq(3.0.sq_ft , 3.0 sq_ft , 3.0 (sq_ft)) , 3.0 * 9.290304e-2),
          (Seq(3.0.sq_ch , 3.0 sq_ch , 3.0 (sq_ch)) , 3.0 * 404.68564224),
          (Seq(3.0.sq_yd , 3.0 sq_yd , 3.0 (sq_yd)) , 3.0 * 0.83612736),
          (Seq(3.0.sq_rd , 3.0 sq_rd , 3.0 (sq_rd)) , 3.0 * 25.29285264),
          (Seq(3.0.sq_mi , 3.0 sq_mi , 3.0 (sq_mi)) , 3.0 * 2.589988110336e6),

          (Seq(3.0.ac, 3.0 ac, 3.0 (ac)), 3.0 * 4046.8564224),
          (Seq(3.0.ro, 3.0 ro, 3.0 (ro)), 3.0 * 1011.7141056),

          (Seq(3.0.sq_li(US), 3.0 sq_li(US), 3.0 (sq_li(US))), 3.0 * Math.pow(0.66 * usFoot, 2)),
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

    "3.0 m2 should be converted to the equivalent value in other area units" in {
      __SetUp__
      import UnitSystem.{a => are}
      val q = 3.0 (m2)
      __Exercise__
      val conversions =
        Table(
          ("areas", "expected"),
          (Seq(q.ym2, q ym2, q(ym2)), 3e48),
          (Seq(q.zm2, q zm2, q(zm2)), 3e42),
          (Seq(q.am2, q am2, q(am2)), 3e36),
          (Seq(q.fm2, q fm2, q(fm2)), 3e30),
          (Seq(q.pm2, q pm2, q(pm2)), 3e24),
          (Seq(q.nm2, q nm2, q(nm2)), 3e18),
          (Seq(q.μm2, q μm2, q(μm2)), 3e12),
          (Seq(q.mm2, q mm2, q(mm2)), 3e6),
          (Seq(q.cm2, q cm2, q(cm2)), 3e4),
          (Seq(q.dm2, q dm2, q(dm2)), 3e2),
          (Seq(q.m2, q m2, q(m2)), 3.0),
          (Seq(q.dam2, q dam2, q(dam2)), 3e-2),
          (Seq(q.hm2, q hm2, q(hm2)), 3e-4),
          (Seq(q.km2, q km2, q(km2)), 3e-6),
          (Seq(q.Mm2, q Mm2, q(Mm2)), 3e-12),
          (Seq(q.Gm2, q Gm2, q(Gm2)), 3e-18),
          (Seq(q.Tm2, q Tm2, q(Tm2)), 3e-24),
          (Seq(q.Pm2, q Pm2, q(Pm2)), 3e-30),
          (Seq(q.Em2, q Em2, q(Em2)), 3e-36),
          (Seq(q.Zm2, q Zm2, q(Zm2)), 3e-42),
          (Seq(q.Ym2, q Ym2, q(Ym2)), 3e-48),

          (Seq(q.a, q a, q(are)), 3e-2),
          (Seq(q.ha, q ha, q(ha)), 3e-4),

          (Seq(q.yb, q yb, q(yb)), 3e52),
          (Seq(q.zb, q zb, q(zb)), 3e49),
          (Seq(q.ab, q ab, q(ab)), 3e46),
          (Seq(q.fb, q fb, q(fb)), 3e43),
          (Seq(q.pb, q pb, q(pb)), 3e40),
          (Seq(q.nb, q nb, q(nb)), 3e37),
          (Seq(q.μb, q μb, q(μb)), 3e34),
          (Seq(q.mb, q mb, q(mb)), 3e31),
          (Seq(q.b, q b, q(b)), 3e28),
          (Seq(q.kb, q kb, q(kb)), 3e25),
          (Seq(q.Mb, q Mb, q(Mb)), 3e22),
          (Seq(q.Gb, q Gb, q(Gb)), 3e19),
          (Seq(q.Tb, q Tb, q(Tb)), 3e16),
          (Seq(q.Pb, q Pb, q(Pb)), 3e13),
          (Seq(q.Eb, q Eb, q(Eb)), 3e10),
          (Seq(q.Zb, q Zb, q(Zb)), 3e7),
          (Seq(q.Yb, q Yb, q(Yb)), 3e4),

          (Seq(q.mil2, q mil2, q (mil2)), 3.0 / 6.4516e-10),
          (Seq(q.in2 , q in2 , q (in2)) , 3.0 / 6.4516e-4),
          (Seq(q.li2 , q li2 , q (li2)) , 3.0 / 4.0468564224e-2),
          (Seq(q.lnk2, q lnk2, q (lnk2)), 3.0 / 4.0468564224e-2),
          (Seq(q.ft2 , q ft2 , q (ft2)) , 3.0 / 9.290304e-2),
          (Seq(q.ch2 , q ch2 , q (ch2)) , 3.0 / 404.68564224),
          (Seq(q.yd2 , q yd2 , q (yd2)) , 3.0 / 0.83612736),
          (Seq(q.rd2 , q rd2 , q (rd2)) , 3.0 / 25.29285264),
          (Seq(q.mi2 , q mi2 , q (mi2)) , 3.0 / 2.589988110336e6),

          (Seq(q.sq_mil, q sq_mil, q (sq_mil)), 3.0 / 6.4516e-10),
          (Seq(q.sq_in , q sq_in , q (sq_in)) , 3.0 / 6.4516e-4),
          (Seq(q.sq_li , q sq_li , q (sq_li)) , 3.0 / 4.0468564224e-2),
          (Seq(q.sq_lnk, q sq_lnk, q (sq_lnk)), 3.0 / 4.0468564224e-2),
          (Seq(q.sq_ft , q sq_ft , q (sq_ft)) , 3.0 / 9.290304e-2),
          (Seq(q.sq_ch , q sq_ch , q (sq_ch)) , 3.0 / 404.68564224),
          (Seq(q.sq_yd , q sq_yd , q (sq_yd)) , 3.0 / 0.83612736),
          (Seq(q.sq_rd , q sq_rd , q (sq_rd)) , 3.0 / 25.29285264),
          (Seq(q.sq_mi , q sq_mi , q (sq_mi)) , 3.0 / 2.589988110336e6),

          (Seq(q.ac, q ac, q(ac)), 3.0 / 4046.8564224),
          (Seq(q.ro, q ro, q(ro)), 3.0 / 1011.7141056),

          (Seq(q.sq_li(US), q sq_li (US), q(sq_li(US))), 3.0 / Math.pow(0.66 * usFoot, 2)),
          (Seq(q.sq_lnk(US), q sq_lnk (US), q(sq_lnk(US))), 3.0 / Math.pow(0.66 * usFoot, 2)),
          (Seq(q.sq_ft(US), q sq_ft (US), q(sq_ft(US))), 3.0 / Math.pow(usFoot, 2)),
          (Seq(q.sq_ch(US), q sq_ch (US), q(sq_ch(US))), 3.0 / Math.pow(66 * usFoot, 2)),
          (Seq(q.sq_mi(US), q sq_mi (US), q(sq_mi(US))), 3.0 / Math.pow(5280 * usFoot, 2)),
          (Seq(q.ac(US), q ac (US), q(ac(US))), 3.0 / (10.0 * Math.pow(66 * usFoot, 2))),

          (Seq(q.circ_mil, q circ_mil, q(circ_mil)), 3.0 * 4.0 / (Math.PI * Math.pow(2.54e-5, 2))),
          (Seq(q.circ_in, q circ_in, q(circ_in)), 3.0 * 4.0 / (Math.PI * Math.pow(0.0254, 2))),

          (Seq(q.bd, q bd, q(bd)), 3.0 / 7.74192e-3)
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
      val q = 3.0 (m2)
      __Exercise__
      val conversions =
        Table(
          ("area", "expected"),
          (q.m*cm, 300.0),
          (q m*cm, 300.0),
          (q (m*cm), 300.0)
        )
      __Verify__
      forAll(conversions){ (sut: Double, expected: Double) =>
        sut should equal (%%%%(expected))
      }
    }
  }
}
