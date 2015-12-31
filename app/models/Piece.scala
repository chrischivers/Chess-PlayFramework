package models

/**
 * Created by chrischivers on 29/12/15.
 */
trait Piece {

  val pieceName:String
  val canJump:Boolean
  var owner:Player
  var taken = false

  override def toString:String = pieceName + " (" + owner +")"

  def getPathOfMovement(from:(Int,Int), to:(Int,Int)):Option[Array[(Int,Int)]]

  protected def getStraightPath(from: (Int, Int), to: (Int, Int)):Array[(Int, Int)] = {
    var array:Array[(Int,Int)] = Array()
    val xLoopDirection = if (from._1 - to._1 < 0) 1 else -1
    val yLoopDirection = if (from._2 - to._2 < 0) 1 else -1
    for (x <- from._1 to to._1 by xLoopDirection) {
      for (y <- from._2 to to._2 by yLoopDirection) {
        array = array :+ (x,y)
      }
    }
    array
  }

}

case class Rook(var owner:Player) extends Piece {
  override val pieceName: String = "Rook"
  override val canJump = false

  override def getPathOfMovement(from: (Int, Int), to: (Int, Int)): Option[Array[(Int, Int)]] = {
    if (from._1 == to._1 ^ from._2 == to._2) {
      Option(getStraightPath(from,to))
    } else {
      None
    }
  }
}

case class King(var owner:Player) extends Piece {
  override val pieceName: String = "King"
  override val canJump = false
  override def getPathOfMovement(from: (Int, Int), to: (Int, Int)): Option[Array[(Int, Int)]] = {
    if (Math.abs(from._1 - to._1) ==  1 || Math.abs(from._2 - to._2) == 1) {
      Option(getStraightPath(from,to))
    } else {
      None
    }
  }
}

case class Queen(var owner:Player) extends Piece {
  override val pieceName: String = "Queen"
  override val canJump = false

  override def getPathOfMovement(from: (Int, Int), to: (Int, Int)): Option[Array[(Int, Int)]] = ???
}



