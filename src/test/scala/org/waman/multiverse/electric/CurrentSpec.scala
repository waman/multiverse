package org.waman.multiverse.electric

import org.scalatest.prop.PropertyChecks
import org.waman.multiverse.AbstractQuantityAndUnitSpec
import org.waman.multiverse.UnitSystem._

import scala.language.postfixOps

/**
  * Expected values are from
  * <a href="https://en.wikipedia.org/wiki/Conversion_of_units">Conversion of units</a>.
  */
class CurrentSpec
  extends AbstractQuantityAndUnitSpec[CurrentUnit]
    with PropertyChecks{

  override protected val getUnitClass = classOf[CurrentUnit]

  "3.0 <<current unit>> should be converted to the equivalent value in Ampere" in {
    __Exercise__
    val conversions =
      Table(
        ("currents", "expected"),
        (Seq(3.0.yA, 3.0 yA, 3.0 (yA)), 3e-24),
        (Seq(3.0.zA, 3.0 zA, 3.0 (zA)), 3e-21),
        (Seq(3.0.aA, 3.0 aA, 3.0 (aA)), 3e-18),
        (Seq(3.0.fA, 3.0 fA, 3.0 (fA)), 3e-15),
        (Seq(3.0.pA, 3.0 pA, 3.0 (pA)), 3e-12),
        (Seq(3.0.nA, 3.0 nA, 3.0 (nA)), 3e-9),
        (Seq(3.0.microAmpere, 3.0 microAmpere, 3.0 (microAmpere)), 3e-6),
        (Seq(3.0.microA, 3.0 microA, 3.0 (microA)), 3e-6),
        (Seq(3.0.μA, 3.0 μA, 3.0 (μA)), 3e-6),
        (Seq(3.0.mA, 3.0 mA, 3.0 (mA)), 3e-3),
        (Seq(3.0.cA, 3.0 cA, 3.0 (cA)), 3e-2),
        (Seq(3.0.dA, 3.0 dA, 3.0 (dA)), 3e-1),
        (Seq(3.0.A , 3.0 A , 3.0 (A)) , 3.0),
        (Seq(3.0.daA, 3.0 daA, 3.0 (daA)), 3e1),
        (Seq(3.0.hA, 3.0 hA, 3.0 (hA)), 3e2),
        (Seq(3.0.kA, 3.0 kA, 3.0 (kA)), 3e3),
        (Seq(3.0.MA, 3.0 MA, 3.0 (MA)), 3e6),
        (Seq(3.0.GA, 3.0 GA, 3.0 (GA)), 3e9),
        (Seq(3.0.TA, 3.0 TA, 3.0 (TA)), 3e12),
        (Seq(3.0.PA, 3.0 PA, 3.0 (PA)), 3e15),
        (Seq(3.0.EA, 3.0 EA, 3.0 (EA)), 3e18),
        (Seq(3.0.ZA, 3.0 ZA, 3.0 (ZA)), 3e21),
        (Seq(3.0.YA, 3.0 YA, 3.0 (YA)), 3e24)
      )
    __Verify__
    forAll(conversions){ (suts: Seq[Current[Double]], expected: Double) =>
      suts.foreach{ sut =>
        (sut A) should equal (%%%%(expected))
      }
    }
  }

  "3.0 A should be converted to the equivalent value in other current units" in {
    __SetUp__
    val q = 3.0 (A)
    __Exercise__
    val conversions =
      Table(
        ("currents", "expected"),
        (Seq(q.yA, q yA, q (yA)), 3e24),
        (Seq(q.zA, q zA, q (zA)), 3e21),
        (Seq(q.aA, q aA, q (aA)), 3e18),
        (Seq(q.fA, q fA, q (fA)), 3e15),
        (Seq(q.pA, q pA, q (pA)), 3e12),
        (Seq(q.nA, q nA, q (nA)), 3e9),
        (Seq(q.microAmpere, q microAmpere, q (microAmpere)), 3e6),
        (Seq(q.microA, q microA, q (microA)), 3e6),
        (Seq(q.μA, q μA, q (μA)), 3e6),
        (Seq(q.mA, q mA, q (mA)), 3e3),
        (Seq(q.cA, q cA, q (cA)), 3e2),
        (Seq(q.dA, q dA, q (dA)), 3e1),
        (Seq(q.A , q A , q (A)) , 3.0),
        (Seq(q.daA, q daA, q (daA)), 3e-1),
        (Seq(q.hA, q hA, q (hA)), 3e-2),
        (Seq(q.kA, q kA, q (kA)), 3e-3),
        (Seq(q.MA, q MA, q (MA)), 3e-6),
        (Seq(q.GA, q GA, q (GA)), 3e-9),
        (Seq(q.TA, q TA, q (TA)), 3e-12),
        (Seq(q.PA, q PA, q (PA)), 3e-15),
        (Seq(q.EA, q EA, q (EA)), 3e-18),
        (Seq(q.ZA, q ZA, q (ZA)), 3e-21),
        (Seq(q.YA, q YA, q (YA)), 3e-24)
      )
    __Verify__
    forAll(conversions){ (suts: Seq[Double], expected: Double) =>
      suts.foreach{ sut =>
        sut should equal (%%%%(expected))
      }
    }
  }
}
