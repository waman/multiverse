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

  "Tests where converting square metre unit to other units like 3.0 m2 => 3.0 * 1e-6 km2" in {
    import UnitSystem.{a => are}
    val value = 3.0 m2

    val conversions =
      Table(
        ("area", "expected"),
        (Seq(value.ym2, value ym2, value (ym2)), 3e48),
        (Seq(value.zm2, value zm2, value (zm2)), 3e42),
        (Seq(value.am2, value am2, value (am2)), 3e36),
        (Seq(value.fm2, value fm2, value (fm2)), 3e30),
        (Seq(value.pm2, value pm2, value (pm2)), 3e24),
        (Seq(value.nm2, value nm2, value (nm2)), 3e18),
        (Seq(value.μm2, value μm2, value (μm2)), 3e12),
        (Seq(value.mm2, value mm2, value (mm2)), 3e6),
        (Seq(value.cm2, value cm2, value (cm2)), 3e4),
        (Seq(value.dm2, value dm2, value (dm2)), 3e2),
        (Seq(value.m2 , value m2 , value (m2)) , 3.0),
        (Seq(value.dam2, value dam2, value (dam2)), 3e-2),
        (Seq(value.hm2, value hm2, value (hm2)), 3e-4),
        (Seq(value.km2, value km2, value (km2)), 3e-6),
        (Seq(value.Mm2, value Mm2, value (Mm2)), 3e-12),
        (Seq(value.Gm2, value Gm2, value (Gm2)), 3e-18),
        (Seq(value.Tm2, value Tm2, value (Tm2)), 3e-24),
        (Seq(value.Pm2, value Pm2, value (Pm2)), 3e-30),
        (Seq(value.Em2, value Em2, value (Em2)), 3e-36),
        (Seq(value.Zm2, value Zm2, value (Zm2)), 3e-42),
        (Seq(value.Ym2, value Ym2, value (Ym2)), 3e-48),

        (Seq(value.a  , value a  , value (are)), 3e-2),
        (Seq(value.ha , value ha , value (ha)) , 3e-4),

        (Seq(value.yb, value yb, value (yb)), 3e52),
        (Seq(value.zb, value zb, value (zb)), 3e49),
        (Seq(value.ab, value ab, value (ab)), 3e46),
        (Seq(value.fb, value fb, value (fb)), 3e43),
        (Seq(value.pb, value pb, value (pb)), 3e40),
        (Seq(value.nb, value nb, value (nb)), 3e37),
        (Seq(value.μb, value μb, value (μb)), 3e34),
        (Seq(value.mb, value mb, value (mb)), 3e31),
        (Seq(value.b , value b , value (b)) , 3e28),
        (Seq(value.kb, value kb, value (kb)), 3e25),
        (Seq(value.Mb, value Mb, value (Mb)), 3e22),
        (Seq(value.Gb, value Gb, value (Gb)), 3e19),
        (Seq(value.Tb, value Tb, value (Tb)), 3e16),
        (Seq(value.Pb, value Pb, value (Pb)), 3e13),
        (Seq(value.Eb, value Eb, value (Eb)), 3e10),
        (Seq(value.Zb, value Zb, value (Zb)), 3e7),
        (Seq(value.Yb, value Yb, value (Yb)), 3e4),

        (Seq(value.ac , value ac , value (ac)) , 3.0 / 4046.8564224)
      )

    forAll(conversions){ (as: Seq[Double], expected: Double) =>
      as.foreach{ a =>
        a should equal (%(expected))
      }
    }
  }
}
