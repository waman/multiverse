package org.waman.multiverse.metric

import org.scalatest.prop.PropertyChecks
import org.waman.multiverse.AbstractQuantityAndUnitSpec
import org.waman.multiverse.UnitSystem._
import org.waman.multiverse.mechanics.Velocity

import scala.language.postfixOps

/**
  * Expected values are from
  * <a href="https://en.wikipedia.org/wiki/Conversion_of_units">Conversion of units</a>.
  */
class LengthSpec
  extends AbstractQuantityAndUnitSpec[LengthUnit]
    with PropertyChecks{

  override protected val getUnitClass = classOf[LengthUnit]

  "3.0 <<length unit>> should be converted to the equivalent value in metre" in {
    __Exercise__
    val conversions =
      Table(
        ("lengths", "expected"),
        (Seq(3.0.ym, 3.0 ym, 3.0 (ym)), 3e-24),
        (Seq(3.0.zm, 3.0 zm, 3.0 (zm)), 3e-21),
        (Seq(3.0.am, 3.0 am, 3.0 (am)), 3e-18),
        (Seq(3.0.fm, 3.0 fm, 3.0 (fm)), 3e-15),
        (Seq(3.0.pm, 3.0 pm, 3.0 (pm)), 3e-12),
        (Seq(3.0.nm, 3.0 nm, 3.0 (nm)), 3e-9),
        (Seq(3.0.μm, 3.0 μm, 3.0 (μm)), 3e-6),
        (Seq(3.0.mm, 3.0 mm, 3.0 (mm)), 3e-3),
        (Seq(3.0.cm, 3.0 cm, 3.0 (cm)), 3e-2),
        (Seq(3.0.dm, 3.0 dm, 3.0 (dm)), 3e-1),
        (Seq(3.0.m , 3.0 m , 3.0 (m)) , 3.0),
        (Seq(3.0.dam, 3.0 dam, 3.0 (dam)), 3e1),
        (Seq(3.0.hm, 3.0 hm, 3.0 (hm)), 3e2),
        (Seq(3.0.km, 3.0 km, 3.0 (km)), 3e3),
        (Seq(3.0.Mm, 3.0 Mm, 3.0 (Mm)), 3e6),
        (Seq(3.0.Gm, 3.0 Gm, 3.0 (Gm)), 3e9),
        (Seq(3.0.Tm, 3.0 Tm, 3.0 (Tm)), 3e12),
        (Seq(3.0.Pm, 3.0 Pm, 3.0 (Pm)), 3e15),
        (Seq(3.0.Em, 3.0 Em, 3.0 (Em)), 3e18),
        (Seq(3.0.Zm, 3.0 Zm, 3.0 (Zm)), 3e21),
        (Seq(3.0.Ym, 3.0 Ym, 3.0 (Ym)), 3e24),

        // microscopic
        (Seq(3.0.μ , 3.0 μ , 3.0 (μ)) , 3.0 * 1e-6),
        (Seq(3.0.Å , 3.0 Å , 3.0 (Å)) , 3.0 * 1e-10),
        (Seq(3.0.a0, 3.0 a0, 3.0 (a0)), 3.0 * 5.2917721092e-11),
        (Seq(3.0.xu, 3.0 xu, 3.0 (xu)), 3.0 * 1.0021e-13),
        (Seq(3.0.xu(CuKα1), 3.0 xu(CuKα1), 3.0 (xu(CuKα1))), 3.0 * 1.0020769928e-13),
        (Seq(3.0.xu(MoKα1), 3.0 xu(MoKα1), 3.0 (xu(MoKα1))), 3.0 * 1.0020995553e-13),
        (Seq(3.0.lp, 3.0 lp, 3.0 (lp)), 3.0 * 1.61624e-35),

        // astronomy
        (Seq(3.0.au, 3.0 au, 3.0 (au)), 3.0 * 149597870700.0),
        (Seq(3.0.ly, 3.0 ly, 3.0 (ly)), 3.0 * 9.4607304725808e15),
        (Seq(3.0.pc, 3.0 pc, 3.0 (pc)), 3.0 * 3.08567782e16),

        // yard-pond
        (Seq(3.0.mil , 3.0 mil , 3.0 (mil)) , 3.0 * 2.54e-5),
        (Seq(3.0.thou, 3.0 thou, 3.0 (thou)), 3.0 * 2.54e-5),
        (Seq(3.0.twp , 3.0 twp , 3.0 (twp)) , 3.0 * 1.76388888889e-5),
        (Seq(3.0.pt  , 3.0 pt  , 3.0 (pt))  , 3.0 * 0.35277777778e-3),
        (Seq(3.0.ln  , 3.0 ln  , 3.0 (ln))  , 3.0 * 0.00211666667),
        (Seq(3.0.in  , 3.0 in  , 3.0 (in))  , 3.0 * 0.0254),
        (Seq(3.0.li  , 3.0 li  , 3.0 (li))  , 3.0 * 0.66 * 0.3048),
        (Seq(3.0.lnk , 3.0 lnk , 3.0 (lnk)) , 3.0 * 0.66 * 0.3048),
        (Seq(3.0.ft  , 3.0 ft  , 3.0 (ft))  , 3.0 * 0.3048),
        (Seq(3.0.yd  , 3.0 yd  , 3.0 (yd))  , 3.0 * 0.9144),
        (Seq(3.0.ell , 3.0 ell , 3.0 (ell)) , 3.0 * 1.143),
        (Seq(3.0.ftm , 3.0 ftm , 3.0 (ftm)) , 3.0 * 1.8288),
        (Seq(3.0.rd  , 3.0 rd  , 3.0 (rd))  , 3.0 * 5.0292),
        (Seq(3.0.rope, 3.0 rope, 3.0 (rope)), 3.0 * 6.096),
        (Seq(3.0.ch  , 3.0 ch  , 3.0 (ch))  , 3.0 * 20.1168),
        (Seq(3.0.mi  , 3.0 mi  , 3.0 (mi))  , 3.0 * 1609.344),
        (Seq(3.0.lea , 3.0 lea , 3.0 (lea)) , 3.0 * 3.0 * 1609.344),

        (Seq(3.0.NM      , 3.0 NM      , 3.0 (NM))      , 3.0 * 1852),
        (Seq(3.0.nmi     , 3.0 nmi     , 3.0 (nmi))     , 3.0 * 1852),
        (Seq(3.0.NM(Adm) , 3.0 NM(Adm) , 3.0 (NM(Adm))) , 3.0 * 1853.184),
        (Seq(3.0.nmi(Adm), 3.0 nmi(Adm), 3.0 (nmi(Adm))), 3.0 * 1853.184),
        (Seq(3.0.nl      , 3.0 nl      , 3.0 (nl))      , 3.0 * 5556),
        (Seq(3.0.NL      , 3.0 NL      , 3.0 (NL))      , 3.0 * 5556),
        (Seq(3.0.cb      , 3.0 cb      , 3.0 (cb))      , 3.0 * 185.2),
        (Seq(3.0.cb(US)  , 3.0 cb(US)  , 3.0 (cb(US)))  , 3.0 * 219.456),
        (Seq(3.0.cb(imp) , 3.0 cb(imp) , 3.0 (cb(imp))) , 3.0 * 185.3184),

        (Seq(3.0.li(US) , 3.0 li(US) , 3.0 (li(US))) , 3.0 * 0.66 * 1200 / 3937),
        (Seq(3.0.lnk(US), 3.0 lnk(US), 3.0 (lnk(US))), 3.0 * 0.66 * 1200 / 3937),
        (Seq(3.0.ft(US) , 3.0 ft(US) , 3.0 (ft(US))) , 3.0 * 1200 / 3937),
        (Seq(3.0.ch(US) , 3.0 ch(US) , 3.0 (ch(US))) , 3.0 * 66 * 1200 / 3937),
        (Seq(3.0.mi(US) , 3.0 mi(US) , 3.0 (mi(US))) , 3.0 * 5280 * 1200 / 3937),

        (Seq(3.0.mf, 3.0 mf, 3.0 (mf)), 3.0 * 0.31622776601),
        (Seq(3.0.smf, 3.0 smf, 3.0 (smf)), 3.0 * 0.3),
        (Seq(3.0.lmf, 3.0 lmf, 3.0 (lmf)), 3.0 * 0.3333333333),

        (Seq(3.0.Fr, 3.0 Fr, 3.0 (Fr)), 3.0 * 0.3333333333e-3),
        (Seq(3.0.fur, 3.0 fur, 3.0 (fur)), 3.0 * 201.168)
      )
    __Verify__
    forAll(conversions){ (suts: Seq[Length[Double]], expected: Double) =>
      suts.foreach{ sut =>
        (sut m) should equal (%%%%(expected))
      }
    }
  }

  "3.0 m should be converted to the equivalent value in other length units" in {
    __SetUp__
    val q = 3.0 (m)
    __Exercise__
    val conversions =
      Table(
        ("lengths", "expected"),
        (Seq(q.ym, q ym, q (ym)), 3e24),
        (Seq(q.zm, q zm, q (zm)), 3e21),
        (Seq(q.am, q am, q (am)), 3e18),
        (Seq(q.fm, q fm, q (fm)), 3e15),
        (Seq(q.pm, q pm, q (pm)), 3e12),
        (Seq(q.nm, q nm, q (nm)), 3e9),
        (Seq(q.μm, q μm, q (μm)), 3e6),
        (Seq(q.mm, q mm, q (mm)), 3e3),
        (Seq(q.cm, q cm, q (cm)), 3e2),
        (Seq(q.dm, q dm, q (dm)), 3e1),
        (Seq(q.m , q m , q (m)) , 3.0),
        (Seq(q.dam, q dam, q (dam)), 3e-1),
        (Seq(q.hm, q hm, q (hm)), 3e-2),
        (Seq(q.km, q km, q (km)), 3e-3),
        (Seq(q.Mm, q Mm, q (Mm)), 3e-6),
        (Seq(q.Gm, q Gm, q (Gm)), 3e-9),
        (Seq(q.Tm, q Tm, q (Tm)), 3e-12),
        (Seq(q.Pm, q Pm, q (Pm)), 3e-15),
        (Seq(q.Em, q Em, q (Em)), 3e-18),
        (Seq(q.Zm, q Zm, q (Zm)), 3e-21),
        (Seq(q.Ym, q Ym, q (Ym)), 3e-24),

        // microscopic
        (Seq(q.μ, q μ, q (μ)), 3e6),
        (Seq(q.Å, q Å, q (Å)), 3e10),
        (Seq(q.a0, q a0, q (a0)), 3.0 / 5.2917721092e-11),
        (Seq(q.xu, q xu, q (xu)), 3.0 / 1.0021e-13),
        (Seq(q.xu(CuKα1), q xu(CuKα1), q (xu(CuKα1))), 3.0 / 1.0020769928e-13),
        (Seq(q.xu(MoKα1), q xu(MoKα1), q (xu(MoKα1))), 3.0 / 1.0020995553e-13),
        (Seq(q.lp, q lp, q (lp)), 3.0 / 1.61624e-35),

        // astronomy
        (Seq(q.au, q au, q (au)), 3.0 / 149597870700.0),
        (Seq(q.ly, q ly, q (ly)), 3.0 / 9.4607304725808e15),
        (Seq(q.pc, q pc, q (pc)), 3.0 / 3.08567782e16),

        // yard-pond
        (Seq(q.thou, q thou, q (thou)), 3.0 / 2.54e-5),
        (Seq(q.mil , q mil , q (mil)) , 3.0 / 2.54e-5),
        (Seq(q.twp , q twp , q (twp)) , 3.0 / 1.76388888889e-5),
        (Seq(q.pt  , q pt  , q (pt))  , 3.0 / 0.35277777778e-3),
        (Seq(q.ln  , q ln  , q (ln))  , 3.0 / 0.00211666667),
        (Seq(q.in  , q in  , q (in))  , 3.0 / 0.0254),
        (Seq(q.li  , q li  , q (li))  , 3.0 / (0.66 * 0.3048)),
        (Seq(q.lnk , q lnk , q (lnk)) , 3.0 / (0.66 * 0.3048)),
        (Seq(q.ft  , q ft  , q (ft))  , 3.0 / 0.3048),
        (Seq(q.yd  , q yd  , q (yd))  , 3.0 / 0.9144),
        (Seq(q.ell , q ell , q (ell)) , 3.0 / 1.143),
        (Seq(q.ftm , q ftm , q (ftm)) , 3.0 / 1.8288),
        (Seq(q.rd  , q rd  , q (rd))  , 3.0 / 5.0292),
        (Seq(q.rope, q rope, q (rope)), 3.0 / 6.096),
        (Seq(q.ch  , q ch  , q (ch))  , 3.0 / 20.1168),
        (Seq(q.mi  , q mi  , q (mi))  , 3.0 / 1609.344),
        (Seq(q.lea , q lea , q (lea)) , 3.0 / (3.0 * 1609.344)),

        (Seq(q.NM      , q NM      , q (NM))      , 3.0 / 1852.0),
        (Seq(q.nmi     , q nmi     , q (nmi))     , 3.0 / 1852.0),
        (Seq(q.NM(Adm) , q NM(Adm) , q (NM(Adm))) , 3.0 / 1853.184),
        (Seq(q.nmi(Adm), q nmi(Adm), q (nmi(Adm))), 3.0 / 1853.184),
        (Seq(q.nl      , q nl      , q (nl))      , 3.0 / 5556),
        (Seq(q.NL      , q NL      , q (NL))      , 3.0 / 5556),
        (Seq(q.cb      , q cb      , q (cb))      , 3.0 / 185.2),
        (Seq(q.cb(US)  , q cb(US)  , q (cb(US)))  , 3.0 / 219.456),
        (Seq(q.cb(imp) , q cb(imp) , q (cb(imp))) , 3.0 / 185.3184),

        (Seq(q.li(US) , q li(US) , q (li(US))) , 3.0 * 3937 / (0.66 * 1200)),
        (Seq(q.lnk(US), q lnk(US), q (lnk(US))), 3.0 * 3937 / (0.66 * 1200)),
        (Seq(q.ft(US) , q ft(US) , q (ft(US))) , 3.0 * 3937 / 1200),
        (Seq(q.ch(US) , q ch(US) , q (ch(US))) , 3.0 * 3937 / (66 * 1200)),
        (Seq(q.mi(US) , q mi(US) , q (mi(US))) , 3.0 * 3937 / (5280 * 1200)),

        (Seq(q.mf , q mf , q (mf)) , 3.0 / 0.31622776601),
        (Seq(q.smf, q smf, q (smf)), 3.0 / 0.3),
        (Seq(q.lmf, q lmf, q (lmf)), 3.0 / 0.3333333333),

        (Seq(q.Fr , q Fr , q (Fr)) , 3.0 / 0.3333333333e-3),
        (Seq(q.fur, q fur, q (fur)), 3.0 / 201.168)
      )
    __Verify__
    forAll(conversions){ (suts: Seq[Double], expected: Double) =>
      suts.foreach{ sut =>
        sut should equal (%%%%(expected))
      }
    }
  }

  "Unit with Context should" in {
    __Exercise__
    val conversions =
      Table(
        "velocity",
        3.0.nmi(Adm)/h,
//        3.0 nmi(Adm)/h,
        3.0 (nmi(Adm)/h)
      )
    __Verify__
    forAll(conversions){ (v: Velocity[Double]) =>
      (v m/s) should equal (%(3.0.nmi(Adm).m / 3600.0))
    }
  }
}
