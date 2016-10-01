package models

import controllers.Assets

/**
 * Created by chrischivers on 29/12/15.
 */
trait Piece {

  val pieceName:String
  var owner:Player
  var taken = false
  val imageFileSuffix:String
  var currentPosition:Position = Position(-1, -1)  // -1, -1 is off board
  def getImageFileName = imageFileSuffix + "-" + owner.playerRef + ".png"

  override def toString:String = pieceName + " (" + owner +")"

  override def clone:Piece = this

  def getPathOfMovement(from:Position, to:Position):Option[Array[Position]]

  protected def getStraightPath(from: Position, to: Position):Array[Position] = {
    var array:Array[Position] = Array()
    val xLoopDirection = if (from.x - to.x < 0) 1 else -1
    val yLoopDirection = if (from.y - to.y < 0) 1 else -1
    for (x <- from.x to to.x by xLoopDirection) {
      for (y <- from.y to to.y by yLoopDirection) {
        array = array :+ Position(x,y)
      }
    }
    array
  }

  protected def getDiagonalPath(from: Position, to: Position):Array[Position] = {
    var array:Array[Position] = Array()
    val xLoopDirection = if (from.x - to.x < 0) 1 else -1
    val yLoopDirection = if (from.y - to.y < 0) 1 else -1
    val numberOfMoves = Math.abs(from.x - to.x)
    for (i <- 0 to numberOfMoves) {
        val x = from.x + (xLoopDirection * i)
        val y = from.y + (yLoopDirection * i)
        array = array :+ Position(x,y)
    }
    array
  }

}

case class Rook(var owner:Player) extends Piece {
  override val pieceName: String = "Rook"
  override val imageFileSuffix: String = "rook"

  override def getPathOfMovement(from: Position, to: Position): Option[Array[Position]] = {
    if (from.x == to.x ^ from.y == to.y) {
      Option(getStraightPath(from,to))
    } else {
      None
    }
  }


}

case class King(var owner:Player) extends Piece {
  override val pieceName: String = "King"
  override val imageFileSuffix: String = "king"
  override def getPathOfMovement(from: Position, to: Position): Option[Array[Position]] = {
    if ((Math.abs(from.x - to.x) != 0 || Math.abs(from.y - to.y) != 0) && Math.abs(from.x - to.x) <=  1 && Math.abs(from.y - to.y) <= 1) {
      Option(Array[Position](from,to)) // Path only consists of from and to, no inbetween as King can only move one square
    } else {
      None
    }
  }
}

case class Queen(var owner:Player) extends Piece {
  override val pieceName: String = "Queen"
  override val imageFileSuffix: String = "queen"

  override def getPathOfMovement(from: Position, to: Position): Option[Array[Position]] = {
    if (Math.abs(from.x - to.x) == Math.abs(from.y - to.y)) {
      Option(getDiagonalPath(from, to))
    } else if (from.x == to.x ^ from.y == to.y){
      Option(getStraightPath(from,to))
    } else {
      None
    }
  }
}

case class Bishop(var owner:Player) extends Piece {
  override val pieceName: String = "Bishop"
  override val imageFileSuffix: String = "bishop"

  override def getPathOfMovement(from: Position, to: Position): Option[Array[Position]] = {
    if (Math.abs(from.x - to.x) == Math.abs(from.y - to.y)) {
      Option(getDiagonalPath(from, to))
    } else {
      None
    }
  }
}

case class Knight(var owner:Player) extends Piece {
  override val pieceName: String = "Knight"
  override val imageFileSuffix: String = "knight"

  override def getPathOfMovement(from: Position, to: Position): Option[Array[Position]] = {
    if ((Math.abs(from.x - to.x) == 2 && Math.abs(from.y - to.y) == 1) || (Math.abs(from.x - to.x) == 1 && Math.abs(from.y - to.y) == 2)){
      Option(Array[Position](from,to)) // Path only consists of from and to, no inbetween as Knight can jump
    } else {
      None
    }
  }
}

case class Pawn(var owner:Player) extends Piece {
  override val pieceName: String = "Pawn"
  override val imageFileSuffix: String = "pawn"
  var firstMoveMade = false

  override def getPathOfMovement(from: Position, to: Position): Option[Array[Position]] = {
    if (to.y - from.y == owner.yDirection || (!firstMoveMade && to.y - from.y == (owner.yDirection * 2))) {
      if (from.x == to.x) Option(getStraightPath(from,to))
      else if (Math.abs(from.x - to.x) == 1) Option(getDiagonalPath(from,to))
      else None
    } else {
      None
    }
  }
}



