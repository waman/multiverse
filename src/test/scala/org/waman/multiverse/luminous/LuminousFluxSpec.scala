package org.waman.multiverse.luminous

import org.scalatest.prop.PropertyChecks
import org.waman.multiverse.AbstractQuantityAndUnitSpec
import org.waman.multiverse.UnitSystem._

import scala.language.postfixOps

/**
  * Expected values are from
  * <a href="https://en.wikipedia.org/wiki/Conversion_of_units">Conversion of units</a>.
  */
class LuminousFluxSpec
  extends AbstractQuantityAndUnitSpec[LuminousFluxUnit]
    with PropertyChecks{

  override protected val getUnitClass = classOf[LuminousFluxUnit]

  "3.0 <<luminous flux unit>> should be converted to the equivalent value in lumen" in {
    __Exercise__
    val conversions =
      Table(
        ("luminous fluxes", "expected"),
        (Seq(3.0.ylm, 3.0 ylm, 3.0 (ylm)), 3e-24),
        (Seq(3.0.zlm, 3.0 zlm, 3.0 (zlm)), 3e-21),
        (Seq(3.0.alm, 3.0 alm, 3.0 (alm)), 3e-18),
        (Seq(3.0.flm, 3.0 flm, 3.0 (flm)), 3e-15),
        (Seq(3.0.plm, 3.0 plm, 3.0 (plm)), 3e-12),
        (Seq(3.0.nlm, 3.0 nlm, 3.0 (nlm)), 3e-9),
        (Seq(3.0.μlm, 3.0 μlm, 3.0 (μlm)), 3e-6),
        (Seq(3.0.mlm, 3.0 mlm, 3.0 (mlm)), 3e-3),
        (Seq(3.0.clm, 3.0 clm, 3.0 (clm)), 3e-2),
        (Seq(3.0.dlm, 3.0 dlm, 3.0 (dlm)), 3e-1),
        (Seq(3.0.lm , 3.0 lm , 3.0 (lm)) , 3.0),
        (Seq(3.0.dalm, 3.0 dalm, 3.0 (dalm)), 3e1),
        (Seq(3.0.hlm, 3.0 hlm, 3.0 (hlm)), 3e2),
        (Seq(3.0.klm, 3.0 klm, 3.0 (klm)), 3e3),
        (Seq(3.0.Mlm, 3.0 Mlm, 3.0 (Mlm)), 3e6),
        (Seq(3.0.Glm, 3.0 Glm, 3.0 (Glm)), 3e9),
        (Seq(3.0.Tlm, 3.0 Tlm, 3.0 (Tlm)), 3e12),
        (Seq(3.0.Plm, 3.0 Plm, 3.0 (Plm)), 3e15),
        (Seq(3.0.Elm, 3.0 Elm, 3.0 (Elm)), 3e18),
        (Seq(3.0.Zlm, 3.0 Zlm, 3.0 (Zlm)), 3e21),
        (Seq(3.0.Ylm, 3.0 Ylm, 3.0 (Ylm)), 3e24)
      )
    __Verify__
    forAll(conversions){ (suts: Seq[LuminousFlux[Double]], expected: Double) =>
      suts.foreach{ sut =>
        (sut lm) should equal (%%%%(expected))
      }
    }
  }

  "3.0 lm should be converted to the equivalent value in other luminus flux units" in {
    __SetUp__
    val q = 3.0 (lm)
    __Exercise__
    val conversions =
      Table(
        ("luminous fluxes", "expected"),
        (Seq(q.ylm, q ylm, q (ylm)), 3e24),
        (Seq(q.zlm, q zlm, q (zlm)), 3e21),
        (Seq(q.alm, q alm, q (alm)), 3e18),
        (Seq(q.flm, q flm, q (flm)), 3e15),
        (Seq(q.plm, q plm, q (plm)), 3e12),
        (Seq(q.nlm, q nlm, q (nlm)), 3e9),
        (Seq(q.μlm, q μlm, q (μlm)), 3e6),
        (Seq(q.mlm, q mlm, q (mlm)), 3e3),
        (Seq(q.clm, q clm, q (clm)), 3e2),
        (Seq(q.dlm, q dlm, q (dlm)), 3e1),
        (Seq(q.lm , q lm , q (lm)) , 3.0),
        (Seq(q.dalm, q dalm, q (dalm)), 3e-1),
        (Seq(q.hlm, q hlm, q (hlm)), 3e-2),
        (Seq(q.klm, q klm, q (klm)), 3e-3),
        (Seq(q.Mlm, q Mlm, q (Mlm)), 3e-6),
        (Seq(q.Glm, q Glm, q (Glm)), 3e-9),
        (Seq(q.Tlm, q Tlm, q (Tlm)), 3e-12),
        (Seq(q.Plm, q Plm, q (Plm)), 3e-15),
        (Seq(q.Elm, q Elm, q (Elm)), 3e-18),
        (Seq(q.Zlm, q Zlm, q (Zlm)), 3e-21),
        (Seq(q.Ylm, q Ylm, q (Ylm)), 3e-24)
      )
    __Verify__
    forAll(conversions){ (suts: Seq[Double], expected: Double) =>
      suts.foreach{ sut =>
        sut should equal (%%%%(expected))
      }
    }
  }
}
