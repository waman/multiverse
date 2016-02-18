package org.waman.multiverse

import org.waman.multiverse.metric.VolumeUnit
import org.waman.multiverse.time.{TimeSquaredUnit, TimeUnit}

trait DivisibleByTime[R]{
  def /(timeUnit: TimeUnit): R
}

trait DivisibleByTimeSquared[R]{
  def /(timeSquaredUnit: TimeSquaredUnit): R
}

trait DivisibleByVolume[R]{
  def /(volumeUnit: VolumeUnit): R
}