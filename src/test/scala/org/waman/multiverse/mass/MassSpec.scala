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

  "3.0 <<mass unit>> should be converted to the equivalent value in kilogram" in {
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
        (Seq(3.0.mcg, 3.0 mcg, 3.0 (mcg)), 3e-9),
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
        (Seq(3.0.Yg, 3.0 Yg, 3.0 (Yg)), 3e21),

        (Seq(3.0.mcg, 3.0 mcg, 3.0 (mcg)), 3e-9)
      )
    __Verify__
    forAll(conversions){ (suts: Seq[Mass[Double]], expected: Double) =>
      suts.foreach{ sut =>
        (sut kg) should equal (%%%%(expected))
      }
    }
  }

  "3.0 kg should be converted to the equivalent value in other mass units" in {
    __SetUp__
    val q = 3.0 (kg)
    __Exercise__
    val conversions =
      Table(
        ("masses", "expected"),
        (Seq(q.yg, q yg, q (yg)), 3e27),
        (Seq(q.zg, q zg, q (zg)), 3e24),
        (Seq(q.ag, q ag, q (ag)), 3e21),
        (Seq(q.fg, q fg, q (fg)), 3e18),
        (Seq(q.pg, q pg, q (pg)), 3e15),
        (Seq(q.ng, q ng, q (ng)), 3e12),
        (Seq(q.μg, q μg, q (μg)), 3e9),
        (Seq(q.mcg, q mcg, q (mcg)), 3e9),
        (Seq(q.mg, q mg, q (mg)), 3e6),
        (Seq(q.cg, q cg, q (cg)), 3e5),
        (Seq(q.dg, q dg, q (dg)), 3e4),
        (Seq(q.g , q g , q (g)) , 3e3),
        (Seq(q.dag, q dag, q (dag)), 3e2),
        (Seq(q.hg, q hg, q (hg)), 3e1),
        (Seq(q.kg, q kg, q (kg)), 3.0),
        (Seq(q.Mg, q Mg, q (Mg)), 3e-3),
        (Seq(q.Gg, q Gg, q (Gg)), 3e-6),
        (Seq(q.Tg, q Tg, q (Tg)), 3e-9),
        (Seq(q.Pg, q Pg, q (Pg)), 3e-12),
        (Seq(q.Eg, q Eg, q (Eg)), 3e-15),
        (Seq(q.Zg, q Zg, q (Zg)), 3e-18),
        (Seq(q.Yg, q Yg, q (Yg)), 3e-21),

        (Seq(q.mcg, q mcg, q (mcg)), 3e9)
      )
    __Verify__
    forAll(conversions){ (suts: Seq[Double], expected: Double) =>
      suts.foreach{ sut =>
        sut should equal (%%%%(expected))
      }
    }
  }
}
