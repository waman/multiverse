package org.waman.multiverse.predef.mechanics

import org.waman.multiverse.units.mechanics.SimpleTimeSquaredUnit
import spire.implicits._

trait TimeSquaredUnits{

  final case object s2 extends SimpleTimeSquaredUnit("second squared", 1)
}
