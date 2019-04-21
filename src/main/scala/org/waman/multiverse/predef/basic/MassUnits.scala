package org.waman.multiverse.predef.basic

import org.waman.multiverse.MetricPrefixes._
import org.waman.multiverse.predef.PhysicalUnitPredef
import org.waman.multiverse.units.basic.{MassUnit, SimpleMassUnit}
import spire.implicits._
import spire.math.Real

import scala.reflect.runtime.{universe => ru}

object MassUnits extends PhysicalUnitPredef[MassUnit]{

  // NOTE: SI Unit is 'kilo'gram
  final case object yg extends SimpleMassUnit("yoctogram", yocto[Real](r"1e-3"))
  final case object zg extends SimpleMassUnit("zeptogram", zepto[Real](r"1e-3"))
  final case object ag extends SimpleMassUnit("attogram", atto[Real](r"1e-3"))
  final case object fg extends SimpleMassUnit("femtogram", femto[Real](r"1e-3"))
  final case object pg extends SimpleMassUnit("picogram", pico[Real](r"1e-3"))
  final case object ng extends SimpleMassUnit("nanogram", nano[Real](r"1e-3"))
  final case object μg extends SimpleMassUnit("microgram", micro[Real](r"1e-3"))
  final case object mg extends SimpleMassUnit("milligram", milli[Real](r"1e-3"))
  final case object cg extends SimpleMassUnit("centigram", centi[Real](r"1e-3"))
  final case object dg extends SimpleMassUnit("decigram", deci[Real](r"1e-3"))
  final case object g extends SimpleMassUnit("gram", r"1e-3")
  final case object dag extends SimpleMassUnit("decagram", deca[Real](r"1e-3"))
  final case object hg extends SimpleMassUnit("hectogram", hecto[Real](r"1e-3"))
  
  final case object kg extends SimpleMassUnit("kilogram", r"1")
  final case object t extends SimpleMassUnit("ton", kilo[Real])

  override protected def getUnitsType: ru.Type = ru.typeOf[this.type]
}

object XMassUnits extends PhysicalUnitPredef[MassUnit]{

  final case object Mg extends SimpleMassUnit("megagram", mega[Real](r"1e-3"))
  final case object Gg extends SimpleMassUnit("gigagram", giga[Real](r"1e-3"))
  final case object Tg extends SimpleMassUnit("teragram", tera[Real](r"1e-3"))
  final case object Pg extends SimpleMassUnit("petagram", peta[Real](r"1e-3"))
  final case object Eg extends SimpleMassUnit("exagram", exa[Real](r"1e-3"))
  final case object Zg extends SimpleMassUnit("zettagram", zetta[Real](r"1e-3"))
  final case object Yg extends SimpleMassUnit("yottagram", yotta[Real](r"1e-3"))

  override protected def getUnitsType: ru.Type = ru.typeOf[this.type]
}


