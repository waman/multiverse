package org.waman.multiverse.metric

import org.scalatest.prop.PropertyChecks
import org.waman.multiverse.UnitSystem._
import org.waman.multiverse.mechanics.Velocity
import org.waman.multiverse.metric.LengthUnit._
import org.waman.multiverse.{MultiverseCustomSpec, UnitSystem}

import scala.language.postfixOps

/**
  * Expected values are from
  * <a href="https://en.wikipedia.org/wiki/Conversion_of_units">Conversion of units</a>.
  */
class LengthSpec extends MultiverseCustomSpec with PropertyChecks{

  "UnitSystem#getSupportedUnits method should return supported units of length" in {
    __SetUp__
    __Exercise__
    val result = UnitSystem.getSupportedUnits(classOf[LengthUnit])
    __Verify__
    result should contain theSameElementsAs Seq(
      YoctoMetre,
      ZeptoMetre,
      AttoMetre,
      FemtoMetre,
      PicoMetre,
      NanoMetre,
      MicroMetre,
      MilliMetre,
      CentiMetre,
      DeciMetre,
      Metre,
      DecaMetre,
      HectoMetre,
      KiloMetre,
      MegaMetre,
      GigaMetre,
      TeraMetre,
      PetaMetre,
      ExaMetre,
      ZettaMetre,
      YottaMetre,

      Micron,
      Angstrom,
      AtomicUnitOfLength,
      XUnit,
      XUnit_CuKAlpha1,
      XUnit_MoKAlpha1,
      PlanckLength,

      AstronomicalUnit,
      LightYear,
      Parsec,

      Mil,
      Twip,
      Point,
      Line,
      Inch,
      Link,
      Foot,
      Yard,
      Ell,
      Fathom,
      Rod,
      Rope,
      Chain,
      Mile,
      League,

      NauticalMile,
      NauticalMile_Admiralty,
      NauticalLeague,
      Cable,
      Cable_US,
      Cable_imperial,

      Link_US_Survey,
      Foot_US_Survey,
      Chain_US_Survey,
      Mile_US_Survey,

      MetricFoot,
      ShortMetricFoot,
      LongMetricFoot,

      French,
      Furlong
    )
  }

  "Tests where converting from some units to Metre like 3.0 mm => 0.003 m" in {
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

  "Tests where converting Metre unit to other units like 3.0 m => 3000.0 mm" in {
    __SetUp__
    val value = 3.0 (m)
    __Exercise__
    val conversions =
      Table(
        ("lengths", "expected"),
        (Seq(value.ym, value ym, value (ym)), 3e24),
        (Seq(value.zm, value zm, value (zm)), 3e21),
        (Seq(value.am, value am, value (am)), 3e18),
        (Seq(value.fm, value fm, value (fm)), 3e15),
        (Seq(value.pm, value pm, value (pm)), 3e12),
        (Seq(value.nm, value nm, value (nm)), 3e9),
        (Seq(value.μm, value μm, value (μm)), 3e6),
        (Seq(value.mm, value mm, value (mm)), 3e3),
        (Seq(value.cm, value cm, value (cm)), 3e2),
        (Seq(value.dm, value dm, value (dm)), 3e1),
        (Seq(value.m , value m , value (m)) , 3.0),
        (Seq(value.dam, value dam, value (dam)), 3e-1),
        (Seq(value.hm, value hm, value (hm)), 3e-2),
        (Seq(value.km, value km, value (km)), 3e-3),
        (Seq(value.Mm, value Mm, value (Mm)), 3e-6),
        (Seq(value.Gm, value Gm, value (Gm)), 3e-9),
        (Seq(value.Tm, value Tm, value (Tm)), 3e-12),
        (Seq(value.Pm, value Pm, value (Pm)), 3e-15),
        (Seq(value.Em, value Em, value (Em)), 3e-18),
        (Seq(value.Zm, value Zm, value (Zm)), 3e-21),
        (Seq(value.Ym, value Ym, value (Ym)), 3e-24),

        // microscopic
        (Seq(value.μ, value μ, value (μ)), 3e6),
        (Seq(value.Å, value Å, value (Å)), 3e10),
        (Seq(value.a0, value a0, value (a0)), 3.0 / 5.2917721092e-11),
        (Seq(value.xu, value xu, value (xu)), 3.0 / 1.0021e-13),
        (Seq(value.xu(CuKα1), value xu(CuKα1), value (xu(CuKα1))), 3.0 / 1.0020769928e-13),
        (Seq(value.xu(MoKα1), value xu(MoKα1), value (xu(MoKα1))), 3.0 / 1.0020995553e-13),
        (Seq(value.lp, value lp, value (lp)), 3.0 / 1.61624e-35),

        // astronomy
        (Seq(value.au, value au, value (au)), 3.0 / 149597870700.0),
        (Seq(value.ly, value ly, value (ly)), 3.0 / 9.4607304725808e15),
        (Seq(value.pc, value pc, value (pc)), 3.0 / 3.08567782e16),

        // yard-pond
        (Seq(value.thou, value thou, value (thou)), 3.0 / 2.54e-5),
        (Seq(value.mil , value mil , value (mil)) , 3.0 / 2.54e-5),
        (Seq(value.twp , value twp , value (twp)) , 3.0 / 1.76388888889e-5),
        (Seq(value.pt  , value pt  , value (pt))  , 3.0 / 0.35277777778e-3),
        (Seq(value.ln  , value ln  , value (ln))  , 3.0 / 0.00211666667),
        (Seq(value.in  , value in  , value (in))  , 3.0 / 0.0254),
        (Seq(value.li  , value li  , value (li))  , 3.0 / (0.66 * 0.3048)),
        (Seq(value.lnk , value lnk , value (lnk)) , 3.0 / (0.66 * 0.3048)),
        (Seq(value.ft  , value ft  , value (ft))  , 3.0 / 0.3048),
        (Seq(value.yd  , value yd  , value (yd))  , 3.0 / 0.9144),
        (Seq(value.ell , value ell , value (ell)) , 3.0 / 1.143),
        (Seq(value.ftm , value ftm , value (ftm)) , 3.0 / 1.8288),
        (Seq(value.rd  , value rd  , value (rd))  , 3.0 / 5.0292),
        (Seq(value.rope, value rope, value (rope)), 3.0 / 6.096),
        (Seq(value.ch  , value ch  , value (ch))  , 3.0 / 20.1168),
        (Seq(value.mi  , value mi  , value (mi))  , 3.0 / 1609.344),
        (Seq(value.lea , value lea , value (lea)) , 3.0 / (3.0 * 1609.344)),

        (Seq(value.NM      , value NM      , value (NM))      , 3.0 / 1852.0),
        (Seq(value.nmi     , value nmi     , value (nmi))     , 3.0 / 1852.0),
        (Seq(value.NM(Adm) , value NM(Adm) , value (NM(Adm))) , 3.0 / 1853.184),
        (Seq(value.nmi(Adm), value nmi(Adm), value (nmi(Adm))), 3.0 / 1853.184),
        (Seq(value.nl      , value nl      , value (nl))      , 3.0 / 5556),
        (Seq(value.NL      , value NL      , value (NL))      , 3.0 / 5556),
        (Seq(value.cb      , value cb      , value (cb))      , 3.0 / 185.2),
        (Seq(value.cb(US)  , value cb(US)  , value (cb(US)))  , 3.0 / 219.456),
        (Seq(value.cb(imp) , value cb(imp) , value (cb(imp))) , 3.0 / 185.3184),

        (Seq(value.li(US) , value li(US) , value (li(US))) , 3.0 * 3937 / (0.66 * 1200)),
        (Seq(value.lnk(US), value lnk(US), value (lnk(US))), 3.0 * 3937 / (0.66 * 1200)),
        (Seq(value.ft(US) , value ft(US) , value (ft(US))) , 3.0 * 3937 / 1200),
        (Seq(value.ch(US) , value ch(US) , value (ch(US))) , 3.0 * 3937 / (66 * 1200)),
        (Seq(value.mi(US) , value mi(US) , value (mi(US))) , 3.0 * 3937 / (5280 * 1200)),

        (Seq(value.mf , value mf , value (mf)) , 3.0 / 0.31622776601),
        (Seq(value.smf, value smf, value (smf)), 3.0 / 0.3),
        (Seq(value.lmf, value lmf, value (lmf)), 3.0 / 0.3333333333),

        (Seq(value.Fr , value Fr , value (Fr)) , 3.0 / 0.3333333333e-3),
        (Seq(value.fur, value fur, value (fur)), 3.0 / 201.168)
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
