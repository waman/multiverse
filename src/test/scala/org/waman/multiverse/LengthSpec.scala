package org.waman.multiverse

import org.scalatest.prop.PropertyChecks
import org.waman.multiverse.UnitSystem._
import scala.language.postfixOps

class LengthSpec extends MultiverseCustomSpec with PropertyChecks with MKSUnitSystem{

  "Tests where converting from some units to metre like 1.0 mm => 0.001 m" in {
    val conversions =
      Table(
        ("length"   , "expectedInMetre"),
        (Seq(1.0.nm, 1.0 nm, 1.0 (nm)), 1e-9),
        (Seq(1.0.µm, 1.0 µm, 1.0 (µm)), 1e-6),
        (Seq(1.0.mm, 1.0 mm, 1.0 (mm)), 1e-3),
        (Seq(1.0.cm, 1.0 cm, 1.0 (cm)), 1e-2),
        (Seq(1.0.m , 1.0 m , 1.0 (m)) , 1.0),
        (Seq(1.0.km, 1.0 km, 1.0 (km)), 1e3),
        (Seq(1.0.Mm, 1.0 Mm, 1.0 (Mm)), 1e6),
        (Seq(1.0.Gm, 1.0 Gm, 1.0 (Gm)), 1e9),
        (Seq(1.0.Tm, 1.0 Tm, 1.0 (Tm)), 1e12),

        // astronomy
        (Seq(1.0.au, 1.0 au, 1.0 (au)), 149597870700.0),
        (Seq(1.0.ly, 1.0 ly, 1.0 (ly)), 9.4607304725808e15),
        (Seq(1.0.pc, 1.0 pc, 1.0 (pc)), 3.08567782e16),

        // yard-pond
        (Seq(1.0.in, 1.0 in, 1.0 (in)), 0.0254),
        (Seq(1.0.ft, 1.0 ft, 1.0 (ft)), 0.3048),
        (Seq(1.0.yd, 1.0 yd, 1.0 (yd)), 0.9144),
        (Seq(1.0.mi, 1.0 mi, 1.0 (mi)), 1609.344)
      )

    forAll(conversions){ (ls: Seq[Length[Double]], expectedInMetre: Double) =>
      ls.foreach{ l =>
        (l m) should equal (%(expectedInMetre))
      }
    }
  }

  val oneMetre = 1.0 m

  "Tests where converting metre unit to other units like 1.0 m => 1000.0 mm" in {
    val conversions =
      Table(
        ("length"   , "expected"),
        (Seq(oneMetre.nm, oneMetre nm, oneMetre (nm)), 1e9),
        (Seq(oneMetre.µm, oneMetre µm, oneMetre (µm)), 1e6),
        (Seq(oneMetre.mm, oneMetre mm, oneMetre (mm)), 1e3),
        (Seq(oneMetre.cm, oneMetre cm, oneMetre (cm)), 1e2),
        (Seq(oneMetre.m, oneMetre m, oneMetre (m)), 1.0),
        (Seq(oneMetre.km, oneMetre km, oneMetre (km)), 1e-3),
        (Seq(oneMetre.Mm, oneMetre Mm, oneMetre (Mm)), 1e-6),
        (Seq(oneMetre.Gm, oneMetre Gm, oneMetre (Gm)), 1e-9),
        (Seq(oneMetre.Tm, oneMetre Tm, oneMetre (Tm)), 1e-12),

        // astronomy
        (Seq(oneMetre.au, oneMetre au, oneMetre (au)), 1.0/149597870700.0),
        (Seq(oneMetre.ly, oneMetre ly, oneMetre (ly)), 1.0/9.4607304725808e15),
        (Seq(oneMetre.pc, oneMetre pc, oneMetre (pc)), 1.0/3.08567782e16),

        // yard-pond
        (Seq(oneMetre.in, oneMetre in, oneMetre (in)), 1.0/0.0254),
        (Seq(oneMetre.ft, oneMetre ft, oneMetre (ft)), 1.0/0.3048),
        (Seq(oneMetre.yd, oneMetre yd, oneMetre (yd)), 1.0/0.9144),
        (Seq(oneMetre.mi, oneMetre mi, oneMetre (mi)), 1.0/1609.344)
      )

    forAll(conversions){ (ls: Seq[Double], expected: Double) =>
      ls.foreach{ l =>
        l should equal (%(expected))
      }
    }
  }

  "Conversion between inch, ft, yard and mile" in {
    ((1.0 ft) in) should equal (%(12.0))
    ((1.0 yd) ft) should equal (%(3.0))
    ((1.0 mi) yd) should equal (%(1760))
  }
}
