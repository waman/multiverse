package org.waman.multiverse

import spire.math.Real
import spire.implicits._

object MultiverseUtil {

  val twoPi: Real = Real.pi * 2
  
  val yocto: Real = r"1e-24"
  val zepto: Real = r"1e-21"
  val atto : Real = r"1e-18"
  val femto: Real = r"1e-15"
  val pico : Real = r"1e-12"
  val nano : Real = r"1e-9"
  val micro: Real = r"1e-6"
  val milli: Real = r"1e-3"
  val centi: Real = r"1e-2"
  val deci : Real = r"1e-1"
  val deca : Real = r"10"
  val hecto: Real = r"1e2"
  val kilo : Real = r"1e3"
  val mega : Real = r"1e6"
  val giga : Real = r"1e9"
  val tera : Real = r"1e12"
  val peta : Real = r"1e15"
  val exa  : Real = r"1e18"
  val zetta: Real = r"1e21"
  val yotta: Real = r"1e24"

  def yocto(value: Real): Real = yocto * value
  def zepto(value: Real): Real = zepto * value
  def atto (value: Real): Real = atto  * value
  def femto(value: Real): Real = femto * value
  def pico (value: Real): Real = pico  * value
  def nano (value: Real): Real = nano  * value
  def micro(value: Real): Real = micro * value
  def milli(value: Real): Real = milli * value
  def centi(value: Real): Real = centi * value
  def deci (value: Real): Real = deci  * value
  def deca (value: Real): Real = deca  * value
  def hecto(value: Real): Real = hecto * value
  def kilo (value: Real): Real = kilo  * value
  def mega (value: Real): Real = mega  * value
  def giga (value: Real): Real = giga  * value
  def tera (value: Real): Real = tera  * value
  def peta (value: Real): Real = peta  * value
  def exa  (value: Real): Real = exa   * value
  def zetta(value: Real): Real = zetta * value
  def yotta(value: Real): Real = yotta * value
}
