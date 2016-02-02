package org.waman.multiverse

import org.scalatest.prop.PropertyChecks
import org.waman.multiverse.UnitSystem._

import scala.language.postfixOps

/**
  * Expected values are from
  * <a href="https://en.wikipedia.org/wiki/Conversion_of_units">Conversion of units</a>.
  */
class AreaSpec extends MultiverseCustomSpec with PropertyChecks{

  "UnitSystem#getSupportedUnits method should return supported units of area" in {
    __SetUp__
    import AreaUnit._
    __Exercise__
    val result = UnitSystem.getSupportedUnits(classOf[AreaUnit])
    __Verify__
    result should contain theSameElementsAs Seq(
      SquareYoctoMetre,
      SquareZeptoMetre,
      SquareAttoMetre,
      SquareFemtoMetre,
      SquarePicoMetre,
      SquareNanoMetre,
      SquareMicroMetre,
      SquareMilliMetre,
      SquareCentiMetre,
      SquareDeciMetre,
      SquareMetre,
      SquareDecaMetre,
      SquareHectoMetre,
      SquareKiloMetre,
      SquareMegaMetre,
      SquareGigaMetre,
      SquareTeraMetre,
      SquarePetaMetre,
      SquareExaMetre,
      SquareZettaMetre,
      SquareYottaMetre,

      Are,
      Hectare,

      YoctoBarn,
      ZeptoBarn,
      AttoBarn,
      FemtoBarn,
      PicoBarn,
      NanoBarn,
      MicroBarn,
      MilliBarn,
      Barn,
      KiloBarn,
      MegaBarn,
      GigaBarn,
      TeraBarn,
      PetaBarn,
      ExaBarn,
      ZettaBarn,
      YottaBarn,

      Acre
    )
  }

  "Tests where converting from some units to square metre like 3.0 km2 => 3e6 m2" in {
    import UnitSystem.{a => are}
    val conversions =
      Table(
        ("area", "expected"),
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
        (Seq(3.0.m2 , 3.0 m2 , 3.0 (m2) ), 3.0),
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

        (Seq(3.0.a  , 3.0 a  , 3.0 (are)), 3e2),
        (Seq(3.0.ha , 3.0 ha , 3.0 (ha)) , 3e4),

        (Seq(3.0.yb, 3.0 yb, 3.0 (yb)), 3e-52),
        (Seq(3.0.zb, 3.0 zb, 3.0 (zb)), 3e-49),
        (Seq(3.0.ab, 3.0 ab, 3.0 (ab)), 3e-46),
        (Seq(3.0.fb, 3.0 fb, 3.0 (fb)), 3e-43),
        (Seq(3.0.pb, 3.0 pb, 3.0 (pb)), 3e-40),
        (Seq(3.0.nb, 3.0 nb, 3.0 (nb)), 3e-37),
        (Seq(3.0.μb, 3.0 μb, 3.0 (μb)), 3e-34),
        (Seq(3.0.mb, 3.0 mb, 3.0 (mb)), 3e-31),
        (Seq(3.0.b , 3.0 b , 3.0 (b)) , 3e-28),
        (Seq(3.0.kb, 3.0 kb, 3.0 (kb)), 3e-25),
        (Seq(3.0.Mb, 3.0 Mb, 3.0 (Mb)), 3e-22),
        (Seq(3.0.Gb, 3.0 Gb, 3.0 (Gb)), 3e-19),
        (Seq(3.0.Tb, 3.0 Tb, 3.0 (Tb)), 3e-16),
        (Seq(3.0.Pb, 3.0 Pb, 3.0 (Pb)), 3e-13),
        (Seq(3.0.Eb, 3.0 Eb, 3.0 (Eb)), 3e-10),
        (Seq(3.0.Zb, 3.0 Zb, 3.0 (Zb)), 3e-7),
        (Seq(3.0.Yb, 3.0 Yb, 3.0 (Yb)), 3e-4),

        (Seq(3.0.ac, 3.0 ac, 3.0 (ac)), 3.0 * 4046.8564224)
      )

    forAll(conversions){ (ls: Seq[Area[Double]], expected: Double) =>
      ls.foreach{ l =>
        (l m2) should equal (%(expected))
      }
    }
  }

  val three_m2 = 3.0 m2

  "Tests where converting square metre unit to other units like 3.0 m2 => 3.0 * 1e-6 km2" in {
    import UnitSystem.{a => are}
    val conversions =
      Table(
        ("area", "expected"),
        (Seq(three_m2.ym2, three_m2 ym2, three_m2 (ym2)), 3e48),
        (Seq(three_m2.zm2, three_m2 zm2, three_m2 (zm2)), 3e42),
        (Seq(three_m2.am2, three_m2 am2, three_m2 (am2)), 3e36),
        (Seq(three_m2.fm2, three_m2 fm2, three_m2 (fm2)), 3e30),
        (Seq(three_m2.pm2, three_m2 pm2, three_m2 (pm2)), 3e24),
        (Seq(three_m2.nm2, three_m2 nm2, three_m2 (nm2)), 3e18),
        (Seq(three_m2.μm2, three_m2 μm2, three_m2 (μm2)), 3e12),
        (Seq(three_m2.mm2, three_m2 mm2, three_m2 (mm2)), 3e6),
        (Seq(three_m2.cm2, three_m2 cm2, three_m2 (cm2)), 3e4),
        (Seq(three_m2.dm2, three_m2 dm2, three_m2 (dm2)), 3e2),
        (Seq(three_m2.m2 , three_m2 m2 , three_m2 (m2)) , 3.0),
        (Seq(three_m2.dam2, three_m2 dam2, three_m2 (dam2)), 3e-2),
        (Seq(three_m2.hm2, three_m2 hm2, three_m2 (hm2)), 3e-4),
        (Seq(three_m2.km2, three_m2 km2, three_m2 (km2)), 3e-6),
        (Seq(three_m2.Mm2, three_m2 Mm2, three_m2 (Mm2)), 3e-12),
        (Seq(three_m2.Gm2, three_m2 Gm2, three_m2 (Gm2)), 3e-18),
        (Seq(three_m2.Tm2, three_m2 Tm2, three_m2 (Tm2)), 3e-24),
        (Seq(three_m2.Pm2, three_m2 Pm2, three_m2 (Pm2)), 3e-30),
        (Seq(three_m2.Em2, three_m2 Em2, three_m2 (Em2)), 3e-36),
        (Seq(three_m2.Zm2, three_m2 Zm2, three_m2 (Zm2)), 3e-42),
        (Seq(three_m2.Ym2, three_m2 Ym2, three_m2 (Ym2)), 3e-48),

        (Seq(three_m2.a  , three_m2 a  , three_m2 (are)), 3e-2),
        (Seq(three_m2.ha , three_m2 ha , three_m2 (ha)) , 3e-4),

        (Seq(three_m2.yb, three_m2 yb, three_m2 (yb)), 3e52),
        (Seq(three_m2.zb, three_m2 zb, three_m2 (zb)), 3e49),
        (Seq(three_m2.ab, three_m2 ab, three_m2 (ab)), 3e46),
        (Seq(three_m2.fb, three_m2 fb, three_m2 (fb)), 3e43),
        (Seq(three_m2.pb, three_m2 pb, three_m2 (pb)), 3e40),
        (Seq(three_m2.nb, three_m2 nb, three_m2 (nb)), 3e37),
        (Seq(three_m2.μb, three_m2 μb, three_m2 (μb)), 3e34),
        (Seq(three_m2.mb, three_m2 mb, three_m2 (mb)), 3e31),
        (Seq(three_m2.b , three_m2 b , three_m2 (b)) , 3e28),
        (Seq(three_m2.kb, three_m2 kb, three_m2 (kb)), 3e25),
        (Seq(three_m2.Mb, three_m2 Mb, three_m2 (Mb)), 3e22),
        (Seq(three_m2.Gb, three_m2 Gb, three_m2 (Gb)), 3e19),
        (Seq(three_m2.Tb, three_m2 Tb, three_m2 (Tb)), 3e16),
        (Seq(three_m2.Pb, three_m2 Pb, three_m2 (Pb)), 3e13),
        (Seq(three_m2.Eb, three_m2 Eb, three_m2 (Eb)), 3e10),
        (Seq(three_m2.Zb, three_m2 Zb, three_m2 (Zb)), 3e7),
        (Seq(three_m2.Yb, three_m2 Yb, three_m2 (Yb)), 3e4),

        (Seq(three_m2.ac , three_m2 ac , three_m2 (ac)) , 3.0 / 4046.8564224)
      )

    forAll(conversions){ (as: Seq[Double], expected: Double) =>
      as.foreach{ a =>
        a should equal (%(expected))
      }
    }
  }
}
