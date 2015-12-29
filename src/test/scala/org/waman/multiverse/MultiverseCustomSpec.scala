package org.waman.multiverse

import org.waman.scalatest_util.WamanCustomSpec

class MultiverseCustomSpec extends WamanCustomSpec{

  def %(value: Double) = value +- (value / 100.0)
}
