package org.waman.multiverse

import org.waman.multiverse.mass.MassUnit
import org.waman.multiverse.metric.{AreaUnit, VolumeUnit}
import org.waman.multiverse.thermal.TemperatureUnit
import org.waman.multiverse.time.{TimeSquaredUnit, TimeUnit}

trait DivisibleByTimeUnit[R]{
  def /(timeUnit: TimeUnit): R
}

trait DivisibleByTimeSquaredUnit[R]{
  def /(timeSquaredUnit: TimeSquaredUnit): R
}

trait DivisibleByAreaUnit[R]{
  def /(areaUnit: AreaUnit): R
}

trait DivisibleByVolumeUnit[R]{
  def /(volumeUnit: VolumeUnit): R
}

trait DivisibleByMassUnit[R]{
  def /(massUnit: MassUnit): R
}

trait DivisibleByTemperatureUnit[R]{
  def /(temperatureUnit: TemperatureUnit): R
}