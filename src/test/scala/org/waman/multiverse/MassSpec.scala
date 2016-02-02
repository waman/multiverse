package org.waman.multiverse

import org.scalatest.prop.PropertyChecks
import org.waman.multiverse.UnitSystem._
import spire.implicits._

import scala.language.postfixOps

/**
  * Expected values are from
  * <a href="https://en.wikipedia.org/wiki/Conversion_of_units">Conversion of units</a>.
  */
class MassSpec extends MultiverseCustomSpec with PropertyChecks{

    "UnitSystem#getSupportedUnits method should return supported units of mass" in {
      __SetUp__
      import MassUnit._
      __Exercise__
      val result = UnitSystem.getSupportedUnits(classOf[MassUnit])
      __Verify__
      result should contain allOf (
        YoctoGram,
        ZeptoGram,
        AttoGram,
        FemtoGram,
        PicoGram,
        NanoGram,
        MicroGram,
        MilliGram,
        CentiGram,
        DeciGram,
        Gram,
        DecaGram,
        HectoGram,
        KiloGram,
        MegaGram,
        GigaGram,
        TeraGram,
        PetaGram,
        ExaGram,
        ZettaGram,
        YottaGram
      )
    }

  "Tests where converting from some units to kg like 3.0 g => 3e-3 kg" in {
    val conversions =
      Table(
        ("mass", "expected"),
        (Seq(3.0.yg, 3.0 yg, 3.0 (yg)), 3e-27),
        (Seq(3.0.zg, 3.0 zg, 3.0 (zg)), 3e-24),
        (Seq(3.0.ag, 3.0 ag, 3.0 (ag)), 3e-21),
        (Seq(3.0.fg, 3.0 fg, 3.0 (fg)), 3e-18),
        (Seq(3.0.pg, 3.0 pg, 3.0 (pg)), 3e-15),
        (Seq(3.0.ng, 3.0 ng, 3.0 (ng)), 3e-12),
        (Seq(3.0.μg, 3.0 μg, 3.0 (μg)), 3e-9),
        (Seq(3.0.mg, 3.0 mg, 3.0 (mg)), 3e-6),
        (Seq(3.0.cg, 3.0 cg, 3.0 (cg)), 3e-5),
        (Seq(3.0.dg, 3.0 dg, 3.0 (dg)), 3e-4),
        (Seq(3.0.g , 3.0 g , 3.0 (g)) , 3e-3),
        (Seq(3.0.dag, 3.0 dag, 3.0 (dag)), 3e-2),
        (Seq(3.0.hg, 3.0 hg, 3.0 (hg)), 3e-1),
        (Seq(3.0.kg, 3.0 kg, 3.0 (kg)), 3.0),
        (Seq(3.0.Mg, 3.0 Mg, 3.0 (Mg)), 3e3),
        (Seq(3.0.Gg, 3.0 Gg, 3.0 (Gg)), 3e6),
        (Seq(3.0.Tg, 3.0 Tg, 3.0 (Tg)), 3e9),
        (Seq(3.0.Pg, 3.0 Pg, 3.0 (Pg)), 3e12),
        (Seq(3.0.Eg, 3.0 Eg, 3.0 (Eg)), 3e15),
        (Seq(3.0.Zg, 3.0 Zg, 3.0 (Zg)), 3e18),
        (Seq(3.0.Yg, 3.0 Yg, 3.0 (Yg)), 3e21)
      )

    forAll(conversions){ (ds: Seq[Mass[Double]], expected: Double) =>
      ds.foreach{ d =>
        (d kg) should equal (%(expected))
      }
    }
  }

  val three_kg = 3.0 kg

  "Tests where converting a mass unit to other units like 3.0 kg/m3 => 3e-3 g/cm3" in {
    val conversions =
      Table(
        ("mass", "expected"),
        (Seq(three_kg.yg, three_kg yg, three_kg (yg)), 3e27),
        (Seq(three_kg.zg, three_kg zg, three_kg (zg)), 3e24),
        (Seq(three_kg.ag, three_kg ag, three_kg (ag)), 3e21),
        (Seq(three_kg.fg, three_kg fg, three_kg (fg)), 3e18),
        (Seq(three_kg.pg, three_kg pg, three_kg (pg)), 3e15),
        (Seq(three_kg.ng, three_kg ng, three_kg (ng)), 3e12),
        (Seq(three_kg.μg, three_kg μg, three_kg (μg)), 3e9),
        (Seq(three_kg.mg, three_kg mg, three_kg (mg)), 3e6),
        (Seq(three_kg.cg, three_kg cg, three_kg (cg)), 3e5),
        (Seq(three_kg.dg, three_kg dg, three_kg (dg)), 3e4),
        (Seq(three_kg.g , three_kg g , three_kg (g)) , 3e3),
        (Seq(three_kg.dag, three_kg dag, three_kg (dag)), 3e2),
        (Seq(three_kg.hg, three_kg hg, three_kg (hg)), 3e1),
        (Seq(three_kg.kg, three_kg kg, three_kg (kg)), 3.0),
        (Seq(three_kg.Mg, three_kg Mg, three_kg (Mg)), 3e-3),
        (Seq(three_kg.Gg, three_kg Gg, three_kg (Gg)), 3e-6),
        (Seq(three_kg.Tg, three_kg Tg, three_kg (Tg)), 3e-9),
        (Seq(three_kg.Pg, three_kg Pg, three_kg (Pg)), 3e-12),
        (Seq(three_kg.Eg, three_kg Eg, three_kg (Eg)), 3e-15),
        (Seq(three_kg.Zg, three_kg Zg, three_kg (Zg)), 3e-18),
        (Seq(three_kg.Yg, three_kg Yg, three_kg (Yg)), 3e-21)
      )

    forAll(conversions){ (ms: Seq[Double], expected: Double) =>
      ms.foreach{ m =>
        m should equal (%(expected))
      }
    }
  }
}
