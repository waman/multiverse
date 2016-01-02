package org.waman.multiverse

import spire.math._
import spire.implicits._

abstract class AngularVelocity[A: Fractional]{

  def `rad/s` : A

  def apply(unit: AngularVelocityUnit): A = unit.accept(this)
}

abstract class AngularVelocityUnit(inRadianPerSecond: Real = r"1"){
  def accept[A](av: AngularVelocity[A]): A = av.`rad/s`
  def accept[A](ui: AngularVelocityUnitInterpreter[A]): AngularVelocity[A] = ui.`rad/s`
}

object AngularVelocityUnit{
  case object `rad/s` extends AngularVelocityUnit()
}

trait AngularVelocityUnitInterpreter[A]{

  val value: A

  def `rad/s` : AngularVelocity[A]

  def apply(unit: AngularVelocityUnit): AngularVelocity[A] = unit.accept(this)
}
