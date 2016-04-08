package org.waman.multiverse.angle

import org.scalatest.prop.PropertyChecks
import org.waman.multiverse.AbstractQuantityAndUnitSpec
import org.waman.multiverse.UnitSystem._

import scala.language.postfixOps

/**
  * Expected values are from
  * <a href="https://en.wikipedia.org/wiki/Conversion_of_units">Conversion of units</a>.
  */
class AngleSpec
    extends AbstractQuantityAndUnitSpec[AngleUnit]
    with PropertyChecks{

  val degree = Math.PI / 180.0

  override protected val getUnitClass = classOf[AngleUnit]

  "3.0 <<angle unit>> should be converted to the equivalent value in radian" in {
    __Exercise__
    val conversions =
      Table(
        ("angles", "expected"),
        (Seq(3.0.rad, 3.0 rad, 3.0 (rad)), 3.0),
        (Seq(3.0.drad, 3.0 drad, 3.0 (drad)), 3e-1),
        (Seq(3.0.crad, 3.0 crad, 3.0 (crad)), 3e-2),
        (Seq(3.0.mrad, 3.0 mrad, 3.0 (mrad)), 3e-3),
        (Seq(3.0.μrad, 3.0 μrad, 3.0 (μrad)), 3e-6),
        (Seq(3.0.mcrad, 3.0 mcrad, 3.0 (mcrad)), 3e-6),
        (Seq(3.0.nrad, 3.0 nrad, 3.0 (nrad)), 3e-9),
        (Seq(3.0.prad, 3.0 prad, 3.0 (prad)), 3e-12),
        (Seq(3.0.frad, 3.0 frad, 3.0 (frad)), 3e-15),
        (Seq(3.0.arad, 3.0 arad, 3.0 (arad)), 3e-18),
        (Seq(3.0.zrad, 3.0 zrad, 3.0 (zrad)), 3e-21),
        (Seq(3.0.yrad, 3.0 yrad, 3.0 (yrad)), 3e-24),

        (Seq(3.0.deg, 3.0 deg, 3.0 (deg)), 3.0 * degree),
        (Seq(3.0.°  , 3.0 °  , 3.0 (°))  , 3.0 * degree),
        (Seq(3.0.arcmin, 3.0 arcmin, 3.0 (arcmin)), 3.0 * degree / 60.0),
        (Seq(3.0.MOA   , 3.0 MOA   , 3.0 (MOA))   , 3.0 * degree / 60.0),
        (Seq(3.0.arcsec, 3.0 arcsec, 3.0 (arcsec)), 3.0 * degree / 3600.0),
        (Seq(3.0.mas, 3.0 mas, 3.0 (mas)), 3.0 * degree / 3600e3),
        (Seq(3.0.μas, 3.0 μas, 3.0 (μas)), 3.0 * degree / 3600e6),
        (Seq(3.0.mcas, 3.0 mcas, 3.0 (mcas)), 3.0 * degree / 3600e6),
        (Seq(3.0.nas, 3.0 nas, 3.0 (nas)), 3.0 * degree / 3600e9),
        (Seq(3.0.pas, 3.0 pas, 3.0 (pas)), 3.0 * degree / 3600e12),
        (Seq(3.0.fas, 3.0 fas, 3.0 (fas)), 3.0 * degree / 3600e15),
        (Seq(3.0.aas, 3.0 aas, 3.0 (aas)), 3.0 * degree / 3600e18),
        (Seq(3.0.zas, 3.0 zas, 3.0 (zas)), 3.0 * degree / 3600e21),
        (Seq(3.0.yas, 3.0 yas, 3.0 (yas)), 3.0 * degree / 3600e24),

        (Seq(3.0.gon, 3.0 gon, 3.0 (gon)), 3.0 * 2.0 * Math.PI / 400.0),
        (Seq(3.0.ᵍ  , 3.0 ᵍ  , 3.0 (ᵍ))   , 3.0 * 2.0 * Math.PI / 400.0),
        (Seq(3.0.tr , 3.0 tr , 3.0 (tr)) , 3.0 * 2.0 * Math.PI)
      )
    __Verify__
    forAll(conversions){ (suts: Seq[Angle[Double]], expected: Double) =>
      suts.foreach{ sut =>
        (sut rad) should equal (%%%%(expected))
      }
    }
  }

  "3.0 rad should be converted to the equivalent value in other angle units" in {
    __SetUp__
    val q = 3.0 (rad)
    __Exercise__
    val conversions =
      Table(
        ("angles", "expected"),
        (Seq(q.rad, q rad, q (rad)), 3.0),
        (Seq(q.drad, q drad, q (drad)), 3e1),
        (Seq(q.crad, q crad, q (crad)), 3e2),
        (Seq(q.mrad, q mrad, q (mrad)), 3e3),
        (Seq(q.μrad, q μrad, q (μrad)), 3e6),
        (Seq(q.mcrad, q mcrad, q (mcrad)), 3e6),
        (Seq(q.nrad, q nrad, q (nrad)), 3e9),
        (Seq(q.prad, q prad, q (prad)), 3e12),
        (Seq(q.frad, q frad, q (frad)), 3e15),
        (Seq(q.arad, q arad, q (arad)), 3e18),
        (Seq(q.zrad, q zrad, q (zrad)), 3e21),
        (Seq(q.yrad, q yrad, q (yrad)), 3e24),

        (Seq(q.deg, q deg, q (deg)), 3.0 / degree),
        (Seq(q.°  , q °  , q (°))  , 3.0 / degree),
        (Seq(q.arcmin, q arcmin, q (arcmin)), 3.0 * 60.0 / degree),
        (Seq(q.MOA   , q MOA   , q (MOA))   , 3.0 * 60.0 / degree),
        (Seq(q.arcsec, q arcsec, q (arcsec)), 3.0 * 3600.0 / degree),
        (Seq(q.mas, q mas, q (mas)), 3.0 * 3600e3 / degree),
        (Seq(q.μas, q μas, q (μas)), 3.0 * 3600e6 / degree),
        (Seq(q.mcas, q mcas, q (mcas)), 3.0 * 3600e6 / degree),
        (Seq(q.nas, q nas, q (nas)), 3.0 * 3600e9 / degree),
        (Seq(q.pas, q pas, q (pas)), 3.0 * 3600e12 / degree),
        (Seq(q.fas, q fas, q (fas)), 3.0 * 3600e15 / degree),
        (Seq(q.aas, q aas, q (aas)), 3.0 * 3600e18 / degree),
        (Seq(q.zas, q zas, q (zas)), 3.0 * 3600e21 / degree),
        (Seq(q.yas, q yas, q (yas)), 3.0 * 3600e24 / degree),

        (Seq(q.gon, q gon, q (gon)), 3.0 * 400.0 / (2.0 * Math.PI)),
        (Seq(q.ᵍ  , q ᵍ   , q (ᵍ))  , 3.0 * 400.0 / (2.0 * Math.PI)),
        (Seq(q.tr , q tr , q (tr)) , 3.0 / (2.0 * Math.PI))
      )
    __Verify__
    forAll(conversions){ (suts: Seq[Double], expected: Double) =>
      suts.foreach{ sut =>
        sut should equal (%%%%(expected))
      }
    }
  }
}
