package org.waman.multiverse.predef.basic

import spire.math.Real
import spire.implicits._

import org.waman.multiverse._
import org.waman.multiverse.predef._
import org.waman.multiverse.units.basic.VelocityUnit

class DefaultVelocityUnit(val name: String, val symbol: String, val aliases: Seq[String], val intervalInSIUnit: Real) extends VelocityUnit


object VelocityUnitObjects{
  final object speed_of_light extends DefaultVelocityUnit("speed of light", "c", Nil, Constants.SpeedOfLight)
  final object mach_number extends DefaultVelocityUnit("mach number", "M", Nil, r"340") with NotExact

  def getUnits: Seq[VelocityUnit] = 
    Seq(speed_of_light, mach_number)
}

object VelocityUnits{
  def c: VelocityUnit = VelocityUnitObjects.speed_of_light
  def M: VelocityUnit = VelocityUnitObjects.mach_number

  def getUnits: Seq[VelocityUnit] = VelocityUnitObjects.getUnits
}
