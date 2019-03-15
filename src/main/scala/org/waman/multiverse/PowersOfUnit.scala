package org.waman.multiverse

trait CanSquare[R]{
  def square: R
}

trait CanCubic[R] extends CanSquare[R]{
  def cubic: R
}