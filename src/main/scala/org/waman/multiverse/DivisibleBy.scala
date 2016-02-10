package org.waman.multiverse

trait DivisibleByTime[R]{
  def /(timeUnit: TimeUnit): R
}

trait DivisibleByTimeSquared[R]{
  def /(timeSquaredUnit: TimeSquaredUnit): R
}

trait DivisibleByVolume[R]{
  def /(volumeUnit: VolumeUnit): R
}