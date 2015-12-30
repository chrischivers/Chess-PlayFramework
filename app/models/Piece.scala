package models

/**
 * Created by chrischivers on 29/12/15.
 */
trait Piece {

  val pieceName:String
  var owner:Player
  var taken = false

  def canPiecePerformMove(from:(Int,Int), to:(Int,Int)):Boolean
  
  override def toString:String = pieceName + " (" + owner +")"
}

class Castle(var owner:Player) extends Piece {
  override val pieceName: String = "Castle"
  override def canPiecePerformMove(from: (Int, Int), to: (Int, Int)): Boolean = {
    from._1 == to._1 ^ from._2 == to._2
  }

}
