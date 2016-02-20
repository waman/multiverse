package org.waman.multiverse

import org.waman.multiverse.metric.LengthUnit
import org.waman.multiverse.time.TimeUnit

trait MultiplicativeByLength[R]{
  def *(lengthUnit: LengthUnit): R
}

trait MultiplicativeByTime[R]{
  def *(timeUnit: TimeUnit): R
}