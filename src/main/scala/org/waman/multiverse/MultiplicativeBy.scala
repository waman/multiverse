package org.waman.multiverse

import org.waman.multiverse.metric.LengthUnit
import org.waman.multiverse.time.TimeUnit

trait MultiplicativeByLengthUnit[R]{
  def *(lengthUnit: LengthUnit): R
}

trait MultiplicativeByTimeUnit[R]{
  def *(timeUnit: TimeUnit): R
}

trait CanSquare[R]{
  def square: R
}

trait CanCubic[R]{
  def cubic: R
}