package org.waman.multiverse.unit

import org.waman.multiverse.unit.basic._
import scala.reflect.runtime.{universe => ru}

object BasicUnits extends PhysicalUnitPredefProxy{

  // Length Units
  def nm: LengthUnit = LengthUnits.nm
  def μm: LengthUnit = LengthUnits.μm
  /** Equivalent to μm */
  def micron: LengthUnit = LengthUnits.μm
  def mm: LengthUnit = LengthUnits.mm
  def cm: LengthUnit = LengthUnits.cm
  def m : LengthUnit = LengthUnits.m
  def km: LengthUnit = LengthUnits.km

  // Mass Units
  def μg: MassUnit = MassUnits.μg
  /** Equivalent to μg */
  def mcg: MassUnit = MassUnits.μg
  def mg: MassUnit = MassUnits.mg
  def g : MassUnit = MassUnits.g
  def kg: MassUnit = MassUnits.kg
  def t : MassUnit = MassUnits.t

  // Time Units
  def ps: TimeUnit = TimeUnits.ps
  def ns: TimeUnit = TimeUnits.ns
  def μs: TimeUnit = TimeUnits.μs
  def ms: TimeUnit = TimeUnits.ms
  def s : TimeUnit = TimeUnits.s
  def min: TimeUnit = TimeUnits.min
  def h : TimeUnit = TimeUnits.h

  // Velocity Units
  def M: VelocityUnit = VelocityUnits.M
  def c: VelocityUnit = VelocityUnits.c

  override protected def getUnitsType: ru.Type = ru.typeOf[this.type]
}
