package models

/**
 * Created by chrischivers on 29/12/15.
 */
trait Piece {

  val pieceName:String
  var owner:Player
  var taken = false

  def canPiecePerformMove(from:(Int,Int), to:(Int,Int)):Boolean
  
  override def toString:String = pieceName


}
