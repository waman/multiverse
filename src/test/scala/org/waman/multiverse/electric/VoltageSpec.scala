package org.waman.multiverse.electric

import org.scalatest.prop.PropertyChecks
import org.waman.multiverse.AbstractQuantityAndUnitSpec
import org.waman.multiverse.UnitSystem._

import scala.language.postfixOps

/**
  * Expected values are from
  * <a href="https://en.wikipedia.org/wiki/Conversion_of_units">Conversion of units</a>.
  */
class VoltageSpec
  extends AbstractQuantityAndUnitSpec[VoltageUnit]
    with PropertyChecks{

  override protected val getUnitClass = classOf[VoltageUnit]

  "3.0 <<voltage unit>> should be converted to the equivalent value in Volt" in {
    __Exercise__
    val conversions =
      Table(
        ("voltages", "expected"),
        (Seq(3.0.yV, 3.0 yV, 3.0 (yV)), 3e-24),
        (Seq(3.0.zV, 3.0 zV, 3.0 (zV)), 3e-21),
        (Seq(3.0.aV, 3.0 aV, 3.0 (aV)), 3e-18),
        (Seq(3.0.fV, 3.0 fV, 3.0 (fV)), 3e-15),
        (Seq(3.0.pV, 3.0 pV, 3.0 (pV)), 3e-12),
        (Seq(3.0.nV, 3.0 nV, 3.0 (nV)), 3e-9),
        (Seq(3.0.microVolt, 3.0 microVolt, 3.0 (microVolt)), 3e-6),
        (Seq(3.0.microV, 3.0 microV, 3.0 (microV)), 3e-6),
        (Seq(3.0.μV, 3.0 μV, 3.0 (μV)), 3e-6),
        (Seq(3.0.mV, 3.0 mV, 3.0 (mV)), 3e-3),
        (Seq(3.0.cV, 3.0 cV, 3.0 (cV)), 3e-2),
        (Seq(3.0.dV, 3.0 dV, 3.0 (dV)), 3e-1),
        (Seq(3.0.V , 3.0 V , 3.0 (V)) , 3.0),
        (Seq(3.0.daV, 3.0 daV, 3.0 (daV)), 3e1),
        (Seq(3.0.hV, 3.0 hV, 3.0 (hV)), 3e2),
        (Seq(3.0.kV, 3.0 kV, 3.0 (kV)), 3e3),
        (Seq(3.0.MV, 3.0 MV, 3.0 (MV)), 3e6),
        (Seq(3.0.GV, 3.0 GV, 3.0 (GV)), 3e9),
        (Seq(3.0.TV, 3.0 TV, 3.0 (TV)), 3e12),
        (Seq(3.0.PV, 3.0 PV, 3.0 (PV)), 3e15),
        (Seq(3.0.EV, 3.0 EV, 3.0 (EV)), 3e18),
        (Seq(3.0.ZV, 3.0 ZV, 3.0 (ZV)), 3e21),
        (Seq(3.0.YV, 3.0 YV, 3.0 (YV)), 3e24)
      )
    __Verify__
    forAll(conversions){ (suts: Seq[Voltage[Double]], expected: Double) =>
      suts.foreach{ sut =>
        (sut V) should equal (%%%%(expected))
      }
    }
  }

  "3.0 Volt should be converted to the equivalent value in other voltage units" in {
    __SetUp__
    val q = 3.0 (V)
    __Exercise__
    val conversions =
      Table(
        ("voltages", "expected"),
        (Seq(q.yV, q yV, q (yV)), 3e24),
        (Seq(q.zV, q zV, q (zV)), 3e21),
        (Seq(q.aV, q aV, q (aV)), 3e18),
        (Seq(q.fV, q fV, q (fV)), 3e15),
        (Seq(q.pV, q pV, q (pV)), 3e12),
        (Seq(q.nV, q nV, q (nV)), 3e9),
        (Seq(q.microVolt, q microVolt, q (microVolt)), 3e6),
        (Seq(q.microV, q microV, q (microV)), 3e6),
        (Seq(q.μV, q μV, q (μV)), 3e6),
        (Seq(q.mV, q mV, q (mV)), 3e3),
        (Seq(q.cV, q cV, q (cV)), 3e2),
        (Seq(q.dV, q dV, q (dV)), 3e1),
        (Seq(q.V , q V , q (V)) , 3.0),
        (Seq(q.daV, q daV, q (daV)), 3e-1),
        (Seq(q.hV, q hV, q (hV)), 3e-2),
        (Seq(q.kV, q kV, q (kV)), 3e-3),
        (Seq(q.MV, q MV, q (MV)), 3e-6),
        (Seq(q.GV, q GV, q (GV)), 3e-9),
        (Seq(q.TV, q TV, q (TV)), 3e-12),
        (Seq(q.PV, q PV, q (PV)), 3e-15),
        (Seq(q.EV, q EV, q (EV)), 3e-18),
        (Seq(q.ZV, q ZV, q (ZV)), 3e-21),
        (Seq(q.YV, q YV, q (YV)), 3e-24)
      )
    __Verify__
    forAll(conversions){ (suts: Seq[Double], expected: Double) =>
      suts.foreach{ sut =>
        sut should equal (%%%%(expected))
      }
    }
  }
}
