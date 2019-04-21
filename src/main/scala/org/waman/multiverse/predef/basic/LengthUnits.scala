package org.waman.multiverse.predef.basic

import org.waman.multiverse.MetricPrefixes._
import org.waman.multiverse.NotExact
import org.waman.multiverse.predef.PhysicalUnitPredef
import org.waman.multiverse.units.basic.{LengthUnit, SimpleLengthUnit}
import spire.implicits._
import spire.math.Real

import scala.reflect.runtime.{universe => ru}

object LengthUnits extends PhysicalUnitPredef[LengthUnit]{

  final case object ym extends SimpleLengthUnit("yoctometre", yocto[Real])
  final case object zm extends SimpleLengthUnit("zeptometre", zepto[Real])
  final case object am extends SimpleLengthUnit("attometre", atto[Real])
  final case object fm extends SimpleLengthUnit("femtometre", femto[Real])
  final case object pm extends SimpleLengthUnit("picometre", pico[Real])
  final case object nm extends SimpleLengthUnit("nanometre", nano[Real])
  final case object μm extends SimpleLengthUnit("micrometre", micro[Real])
  final case object mm extends SimpleLengthUnit("millimetre", milli[Real])
  final case object cm extends SimpleLengthUnit("centimetre", centi[Real])
  final case object dm extends SimpleLengthUnit("decimetre", deci[Real])

  final case object m extends SimpleLengthUnit("metre", r"1")

  final case object dam extends SimpleLengthUnit("decametre", deca[Real])
  final case object hm extends SimpleLengthUnit("hectometre", hecto[Real])
  final case object km extends SimpleLengthUnit("kilometre", kilo[Real])

  // Atomic scale
  final case object Å extends SimpleLengthUnit("angstrom", r"1e-10")
  final case object a_0 extends SimpleLengthUnit("atomic unit of length", r"5.291772109217e-11") with NotExact

  final case object xu extends SimpleLengthUnit("x unit", r"1.0021e-13") with NotExact {
    def apply(c: XUnitContext): LengthUnit = c match {
      case XUnitContexts.CuKα1 => `xu(CuKα1)`
      case XUnitContexts.MoKα1 => `xu(MoKα1)`
    }
  }

  final case object `xu(CuKα1)` extends SimpleLengthUnit("x unit CuKα1", r"1.0020769928e-13") with NotExact
  final case object `xu(MoKα1)` extends SimpleLengthUnit("x unit MoKα1", r"1.0020995553e-13") with NotExact

  final case object l_p extends SimpleLengthUnit("Planck length", r"1.61624e-35") with NotExact

  // Astronomy
  final case object au extends SimpleLengthUnit("astronomical unit", r"149597870700")
  final case object ly extends SimpleLengthUnit("light year", r"9.4607304725808e15")
  final case object pc extends SimpleLengthUnit("parsec", r"3.08567782e16") with NotExact

  override protected def getUnitsType: ru.Type = ru.typeOf[this.type]
}

sealed trait XUnitContext

object XUnitContexts {
  final case object CuKα1 extends XUnitContext
  final case object MoKα1 extends XUnitContext
}

object XLengthUnits extends PhysicalUnitPredef[LengthUnit]{

  final case object Mm extends SimpleLengthUnit("megametre", mega[Real])
  final case object Gm extends SimpleLengthUnit("gigametre", giga[Real])
  final case object Tm extends SimpleLengthUnit("terametre", tera[Real])
  final case object Pm extends SimpleLengthUnit("petametre", peta[Real])
  final case object Em extends SimpleLengthUnit("exametre", exa[Real])
  final case object Zm extends SimpleLengthUnit("zettametre", zetta[Real])
  final case object Ym extends SimpleLengthUnit("yottametre", yotta[Real])

  override protected def getUnitsType: ru.Type = ru.typeOf[this.type]
}