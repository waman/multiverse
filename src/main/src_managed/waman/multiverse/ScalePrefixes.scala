package waman.multiverse

import spire.math._
import spire.implicits._

object ScalePrefixes {

  private val yoctoReal: Real = r"1e-24"
  private val zeptoReal: Real = r"1e-21"
  private val attoReal: Real = r"1e-18"
  private val femtoReal: Real = r"1e-15"
  private val picoReal: Real = r"1e-12"
  private val nanoReal: Real = r"1e-9"
  private val microReal: Real = r"1e-6"
  private val milliReal: Real = r"1e-3"
  private val centiReal: Real = r"1e-2"
  private val deciReal: Real = r"1e-1"
  private val decaReal: Real = r"1e1"
  private val hectoReal: Real = r"1e2"
  private val kiloReal: Real = r"1e3"
  private val megaReal: Real = r"1e6"
  private val gigaReal: Real = r"1e9"
  private val teraReal: Real = r"1e12"
  private val petaReal: Real = r"1e15"
  private val exaReal: Real = r"1e18"
  private val zettaReal: Real = r"1e21"
  private val yottaReal: Real = r"1e24"

  private def fromReal[A: Fractional](value: Real): A = implicitly[Fractional[A]].fromReal(value)

  def yocto[A: Fractional]: A = fromReal(yoctoReal)
  def zepto[A: Fractional]: A = fromReal(zeptoReal)
  def atto[A: Fractional]: A = fromReal(attoReal)
  def femto[A: Fractional]: A = fromReal(femtoReal)
  def pico[A: Fractional]: A = fromReal(picoReal)
  def nano[A: Fractional]: A = fromReal(nanoReal)
  def micro[A: Fractional]: A = fromReal(microReal)
  def milli[A: Fractional]: A = fromReal(milliReal)
  def centi[A: Fractional]: A = fromReal(centiReal)
  def deci[A: Fractional]: A = fromReal(deciReal)
  def deca[A: Fractional]: A = fromReal(decaReal)
  def hecto[A: Fractional]: A = fromReal(hectoReal)
  def kilo[A: Fractional]: A = fromReal(kiloReal)
  def mega[A: Fractional]: A = fromReal(megaReal)
  def giga[A: Fractional]: A = fromReal(gigaReal)
  def tera[A: Fractional]: A = fromReal(teraReal)
  def peta[A: Fractional]: A = fromReal(petaReal)
  def exa[A: Fractional]: A = fromReal(exaReal)
  def zetta[A: Fractional]: A = fromReal(zettaReal)
  def yotta[A: Fractional]: A = fromReal(yottaReal)

  def yocto[A: Fractional](value: A): A = yocto * value
  def zepto[A: Fractional](value: A): A = zepto * value
  def atto[A: Fractional](value: A): A = atto * value
  def femto[A: Fractional](value: A): A = femto * value
  def pico[A: Fractional](value: A): A = pico * value
  def nano[A: Fractional](value: A): A = nano * value
  def micro[A: Fractional](value: A): A = micro * value
  def milli[A: Fractional](value: A): A = milli * value
  def centi[A: Fractional](value: A): A = centi * value
  def deci[A: Fractional](value: A): A = deci * value
  def deca[A: Fractional](value: A): A = deca * value
  def hecto[A: Fractional](value: A): A = hecto * value
  def kilo[A: Fractional](value: A): A = kilo * value
  def mega[A: Fractional](value: A): A = mega * value
  def giga[A: Fractional](value: A): A = giga * value
  def tera[A: Fractional](value: A): A = tera * value
  def peta[A: Fractional](value: A): A = peta * value
  def exa[A: Fractional](value: A): A = exa * value
  def zetta[A: Fractional](value: A): A = zetta * value
  def yotta[A: Fractional](value: A): A = yotta * value
}
