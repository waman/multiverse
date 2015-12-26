package org.waman.multiverse

import scala.language.implicitConversions

class Per

class MetrePer(metre: Double){
  def s: Velocity = new UnitInterpreter(metre).`m/s`
}

trait UnitSystem{

  implicit def convertDoubleToUnitInterpreter(value: Double): UnitInterpreter =
    new UnitInterpreter(value)

  val / = new Per
}

object UnitSystem{

  // Length
  val mm = LengthUnit.mm
  val cm = LengthUnit.cm
  val m  = LengthUnit.m
  val km = LengthUnit.km
  
  
}

class UnitInterpreter(val value: Double)
  extends LengthUnitInterpreter
  with TimeUnitInterpreter
  with VelocityUnitInterpreter