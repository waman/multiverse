package waman.multiverse.typeless

import scala.language.implicitConversions

package object implicits {

  implicit def convertOneToDimless(one: Int): TypelessLinearUnit = {
    require(one == 1)
    Dimless
  }

//  implicit class DimlessFactory(val one: Int){
//
//    def /(deno: TypelessLinearUnit): TypelessLinearUnit = Dimless / deno
//  }

}
