package models

/**
 * Created by chrischivers on 29/12/15.
 */
trait Piece {

  val pieceName:String
  val canJump:Boolean
  var owner:Player
  var taken = false

  def canPiecePerformMove(from:(Int,Int), to:(Int,Int)):Boolean
  
  override def toString:String = pieceName + " (" + owner +")"
}

case class Rook(var owner:Player) extends Piece {
  override val pieceName: String = "Rook"
  override val canJump = false
  override def canPiecePerformMove(from: (Int, Int), to: (Int, Int)): Boolean = {
    from._1 == to._1 ^ from._2 == to._2
  }
}

case class King(var owner:Player) extends Piece {
  override val pieceName: String = "King"
  override val canJump = false
  override def canPiecePerformMove(from: (Int, Int), to: (Int, Int)): Boolean = {
    Math.abs(from._1 - to._1) ==  1 || Math.abs(from._2 - to._2) == 1
  }
}



