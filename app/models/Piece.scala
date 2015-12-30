package models

/**
 * Created by chrischivers on 29/12/15.
 */
trait Piece {

  val pieceName:String
  var ownedBy:Player
  var taken = false

  def isMoveValid(from:(Int,Int), to:(Int,Int))
  
  override def toString:String = pieceName


}
