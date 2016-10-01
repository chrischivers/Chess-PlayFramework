package models

/**
 * Created by chrischivers on 01/10/16.
 */
case class Position(x:Int, y:Int) {
  override def toString:String = {
    "x:" + x + ", y:" + y
  }

}
