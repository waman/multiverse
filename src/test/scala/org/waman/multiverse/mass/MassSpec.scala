package org.waman.multiverse.mass

import org.scalatest.prop.PropertyChecks
import org.waman.multiverse.AbstractQuantityAndUnitSpec
import org.waman.multiverse.UnitSystem._

import scala.language.postfixOps

/**
  * Expected values are from
  * <a href="https://en.wikipedia.org/wiki/Conversion_of_units">Conversion of units</a>.
  */
class MassSpec
  extends AbstractQuantityAndUnitSpec[MassUnit]
    with PropertyChecks{

  override protected val getUnitClass = classOf[MassUnit]

  "Tests where converting from some units to kg like 3.0 g => 3e-3 kg" in {
    __Exercise__
    val conversions =
      Table(
        ("masses", "expected"),
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
    __Verify__
    forAll(conversions){ (suts: Seq[Mass[Double]], expected: Double) =>
      suts.foreach{ sut =>
        (sut kg) should equal (%%%%(expected))
      }
    }
  }

  "Tests where converting a mass unit to other units like 3.0 kg => 3e3 g" in {
    __SetUp__
    val value = 3.0 (kg)
    __Exercise__
    val conversions =
      Table(
        ("masses", "expected"),
        (Seq(value.yg, value yg, value (yg)), 3e27),
        (Seq(value.zg, value zg, value (zg)), 3e24),
        (Seq(value.ag, value ag, value (ag)), 3e21),
        (Seq(value.fg, value fg, value (fg)), 3e18),
        (Seq(value.pg, value pg, value (pg)), 3e15),
        (Seq(value.ng, value ng, value (ng)), 3e12),
        (Seq(value.μg, value μg, value (μg)), 3e9),
        (Seq(value.mg, value mg, value (mg)), 3e6),
        (Seq(value.cg, value cg, value (cg)), 3e5),
        (Seq(value.dg, value dg, value (dg)), 3e4),
        (Seq(value.g , value g , value (g)) , 3e3),
        (Seq(value.dag, value dag, value (dag)), 3e2),
        (Seq(value.hg, value hg, value (hg)), 3e1),
        (Seq(value.kg, value kg, value (kg)), 3.0),
        (Seq(value.Mg, value Mg, value (Mg)), 3e-3),
        (Seq(value.Gg, value Gg, value (Gg)), 3e-6),
        (Seq(value.Tg, value Tg, value (Tg)), 3e-9),
        (Seq(value.Pg, value Pg, value (Pg)), 3e-12),
        (Seq(value.Eg, value Eg, value (Eg)), 3e-15),
        (Seq(value.Zg, value Zg, value (Zg)), 3e-18),
        (Seq(value.Yg, value Yg, value (Yg)), 3e-21)
      )
    __Verify__
    forAll(conversions){ (suts: Seq[Double], expected: Double) =>
      suts.foreach{ sut =>
        sut should equal (%%%%(expected))
      }
    }
  }
}
