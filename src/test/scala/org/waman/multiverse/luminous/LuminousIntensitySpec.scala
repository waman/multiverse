package org.waman.multiverse.luminous

import org.scalatest.prop.PropertyChecks
import org.waman.multiverse.AbstractQuantityAndUnitSpec
import org.waman.multiverse.UnitSystem._

import scala.language.postfixOps

/**
  * Expected values are from
  * <a href="https://en.wikipedia.org/wiki/Conversion_of_units">Conversion of units</a>.
  */
class LuminousIntensitySpec
  extends AbstractQuantityAndUnitSpec[LuminousIntensityUnit]
    with PropertyChecks{

  override protected val getUnitClass = classOf[LuminousIntensityUnit]

  "3.0 <<luminous intensity unit>> should be converted to the equivalent value in candera" in {
    __Exercise__
    val conversions =
      Table(
        ("luminous intensities", "expected"),
        (Seq(3.0.ycd, 3.0 ycd, 3.0 (ycd)), 3e-24),
        (Seq(3.0.zcd, 3.0 zcd, 3.0 (zcd)), 3e-21),
        (Seq(3.0.acd, 3.0 acd, 3.0 (acd)), 3e-18),
        (Seq(3.0.fcd, 3.0 fcd, 3.0 (fcd)), 3e-15),
        (Seq(3.0.pcd, 3.0 pcd, 3.0 (pcd)), 3e-12),
        (Seq(3.0.ncd, 3.0 ncd, 3.0 (ncd)), 3e-9),
        (Seq(3.0.μcd, 3.0 μcd, 3.0 (μcd)), 3e-6),
        (Seq(3.0.mccd, 3.0 mccd, 3.0 (mccd)), 3e-6),
        (Seq(3.0.mcd, 3.0 mcd, 3.0 (mcd)), 3e-3),
        (Seq(3.0.ccd, 3.0 ccd, 3.0 (ccd)), 3e-2),
        (Seq(3.0.dcd, 3.0 dcd, 3.0 (dcd)), 3e-1),
        (Seq(3.0.cd , 3.0 cd , 3.0 (cd)) , 3.0),
        (Seq(3.0.dacd, 3.0 dacd, 3.0 (dacd)), 3e1),
        (Seq(3.0.hcd, 3.0 hcd, 3.0 (hcd)), 3e2),
        (Seq(3.0.kcd, 3.0 kcd, 3.0 (kcd)), 3e3),
        (Seq(3.0.Mcd, 3.0 Mcd, 3.0 (Mcd)), 3e6),
        (Seq(3.0.Gcd, 3.0 Gcd, 3.0 (Gcd)), 3e9),
        (Seq(3.0.Tcd, 3.0 Tcd, 3.0 (Tcd)), 3e12),
        (Seq(3.0.Pcd, 3.0 Pcd, 3.0 (Pcd)), 3e15),
        (Seq(3.0.Ecd, 3.0 Ecd, 3.0 (Ecd)), 3e18),
        (Seq(3.0.Zcd, 3.0 Zcd, 3.0 (Zcd)), 3e21),
        (Seq(3.0.Ycd, 3.0 Ycd, 3.0 (Ycd)), 3e24)
      )
    __Verify__
    forAll(conversions){ (suts: Seq[LuminousIntensity[Double]], expected: Double) =>
      suts.foreach{ sut =>
        (sut cd) should equal (%%%%(expected))
      }
    }
  }

  "3.0 cd should be converted to the equivalent value in other luminous intensity units" in {
    __SetUp__
    val q = 3.0 (cd)
    __Exercise__
    val conversions =
      Table(
        ("luminous intensities", "expected"),
        (Seq(q.ycd, q ycd, q (ycd)), 3e24),
        (Seq(q.zcd, q zcd, q (zcd)), 3e21),
        (Seq(q.acd, q acd, q (acd)), 3e18),
        (Seq(q.fcd, q fcd, q (fcd)), 3e15),
        (Seq(q.pcd, q pcd, q (pcd)), 3e12),
        (Seq(q.ncd, q ncd, q (ncd)), 3e9),
        (Seq(q.μcd, q μcd, q (μcd)), 3e6),
        (Seq(q.mccd, q mccd, q (mccd)), 3e6),
        (Seq(q.mcd, q mcd, q (mcd)), 3e3),
        (Seq(q.ccd, q ccd, q (ccd)), 3e2),
        (Seq(q.dcd, q dcd, q (dcd)), 3e1),
        (Seq(q.cd , q cd , q (cd)) , 3.0),
        (Seq(q.dacd, q dacd, q (dacd)), 3e-1),
        (Seq(q.hcd, q hcd, q (hcd)), 3e-2),
        (Seq(q.kcd, q kcd, q (kcd)), 3e-3),
        (Seq(q.Mcd, q Mcd, q (Mcd)), 3e-6),
        (Seq(q.Gcd, q Gcd, q (Gcd)), 3e-9),
        (Seq(q.Tcd, q Tcd, q (Tcd)), 3e-12),
        (Seq(q.Pcd, q Pcd, q (Pcd)), 3e-15),
        (Seq(q.Ecd, q Ecd, q (Ecd)), 3e-18),
        (Seq(q.Zcd, q Zcd, q (Zcd)), 3e-21),
        (Seq(q.Ycd, q Ycd, q (Ycd)), 3e-24)
      )
    __Verify__
    forAll(conversions){ (suts: Seq[Double], expected: Double) =>
      suts.foreach{ sut =>
        sut should equal (%%%%(expected))
      }
    }
  }
}
