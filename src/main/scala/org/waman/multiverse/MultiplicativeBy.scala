package org.waman.multiverse

import org.waman.multiverse.metric.LengthUnit

trait MultiplicativeByLength[R]{
  def *(lengthUnit: LengthUnit): R
}