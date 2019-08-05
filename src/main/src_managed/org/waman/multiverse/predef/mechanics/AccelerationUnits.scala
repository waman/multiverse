package org.waman.multiverse.predef.mechanics

import spire.math.Real
import spire.implicits._

import org.waman.multiverse._
import org.waman.multiverse.predef._
import org.waman.multiverse.units.mechanics.AccelerationUnit

class DefaultAccelerationUnit(val name: String, val symbol: String, val aliases: Seq[String], val intervalInSIUnit: Real) extends AccelerationUnit


object AccelerationUnitObjects{
  final object standard_gravity extends DefaultAccelerationUnit("standard gravity", "g_0", Nil, r"9.80665")

  def getUnits: Seq[AccelerationUnit] = 
    Seq(standard_gravity)
}

object AccelerationUnits{
  def g_0: AccelerationUnit = AccelerationUnitObjects.standard_gravity

  def getUnits: Seq[AccelerationUnit] = AccelerationUnitObjects.getUnits
}
