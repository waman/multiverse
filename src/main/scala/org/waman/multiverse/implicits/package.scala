package org.waman.multiverse

import org.waman.multiverse.unit.basic._
import org.waman.multiverse.unit.electric._
import org.waman.multiverse.unit.mechanics._
import org.waman.multiverse.unit.thermal._
import spire.math._

import scala.language.implicitConversions

package object implicits {

  implicit class QuantityFactory[A: Fractional](val value: A){

    // Basics
    def apply(unit: LengthUnit): Length[A] = new Length(value, unit)
    def apply(unit: MassUnit): Mass[A] = new Mass(value, unit)
    def apply(unit: TimeUnit): Time[A] = new Time(value, unit)
    def apply(unit: VelocityUnit): Velocity[A] = new Velocity(value, unit)

    // Mechanics
    def apply(unit: TimeSquaredUnit): TimeSquared[A] = new TimeSquared(value, unit)
    def apply(unit: AccelerationUnit): Acceleration[A] = new Acceleration(value, unit)
    def apply(unit: EnergyUnit): Energy[A] = new Energy(value, unit)

    // Electric
    def apply(unit: ChargeUnit): Charge[A] = new Charge(value, unit)
    def apply(unit: CurrentUnit): Current[A] = new Current(value, unit)
    def apply(unit: VoltageUnit): Voltage[A] = new Voltage(value, unit)
    def apply(unit: ResistanceUnit): Resistance[A] = new Resistance(value, unit)
    def apply(unit: CapacitanceUnit): Capacitance[A] = new Capacitance(value, unit)
    def apply(unit: DipoleUnit): Dipole[A] = new Dipole(value, unit)

    // Thermal
    def apply(unit: TemperatureUnit): Temperature[A] = new Temperature(value, unit)
  }

//  implicit def convertFractionalToQuantityFactory[A: Fractional](value: A): QuantityFactory[A] =
//    new QuantityFactory(value)

  // Integral value (like 1(m), not 1.0(m)) create a Quantity[Real] instance
  implicit def convertIntToQuantityFactory(value: Int): QuantityFactory[Real] =
    new QuantityFactory(Real(value))

  implicit def convertLongToQuantityFactory(value: Long): QuantityFactory[Real] =
    new QuantityFactory(Real(value))

  implicit def convertSafeLongToQuantityFactory(value: SafeLong): QuantityFactory[Real] =
    new QuantityFactory(Real(value))

  implicit def convertBigIntToQuantityFactory(value: BigInt): QuantityFactory[Real] =
    new QuantityFactory(Real(value))
}
