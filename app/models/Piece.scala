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
  def getImageFileName = imageFileSuffix + "-" + owner.playerRef + ".png"

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

  protected def getDiagonalPath(from: (Int, Int), to: (Int, Int)):Array[(Int, Int)] = {
    var array:Array[(Int,Int)] = Array()
    val xLoopDirection = if (from._1 - to._1 < 0) 1 else -1
    val yLoopDirection = if (from._2 - to._2 < 0) 1 else -1
    val numberOfMoves = Math.abs(from._1 - to._1)
    for (i <- 0 to numberOfMoves) {
        val x = from._1 + (xLoopDirection * i)
        val y = from._2 + (yLoopDirection * i)
        array = array :+ (x,y)
    }
    array
  }

}

case class Rook(var owner:Player) extends Piece {
  override val pieceName: String = "Rook"
  override val imageFileSuffix: String = "rook"

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
  override val imageFileSuffix: String = "king"
  override def getPathOfMovement(from: (Int, Int), to: (Int, Int)): Option[Array[(Int, Int)]] = {
    if (Math.abs(from._1 - to._1) ==  1 || Math.abs(from._2 - to._2) == 1) {
      Option(Array[(Int,Int)](from,to)) // Path only consists of from and to, no inbetween as King can only move one square
    } else {
      None
    }
  }
}

case class Queen(var owner:Player) extends Piece {
  override val pieceName: String = "Queen"
  override val imageFileSuffix: String = "queen"

  override def getPathOfMovement(from: (Int, Int), to: (Int, Int)): Option[Array[(Int, Int)]] = {
    if (Math.abs(from._1 - to._1) == Math.abs(from._2 - to._2)) {
      Option(getDiagonalPath(from, to))
    } else if (from._1 == to._1 ^ from._2 == to._2){
      Option(getStraightPath(from,to))
    } else {
      None
    }
  }
}

case class Bishop(var owner:Player) extends Piece {
  override val pieceName: String = "Bishop"
  override val imageFileSuffix: String = "bishop"

  override def getPathOfMovement(from: (Int, Int), to: (Int, Int)): Option[Array[(Int, Int)]] = {
    if (Math.abs(from._1 - to._1) == Math.abs(from._2 - to._2)) {
      Option(getDiagonalPath(from, to))
    } else {
      None
    }
  }
}

case class Knight(var owner:Player) extends Piece {
  override val pieceName: String = "Knight"
  override val imageFileSuffix: String = "knight"

  override def getPathOfMovement(from: (Int, Int), to: (Int, Int)): Option[Array[(Int, Int)]] = {
    if ((Math.abs(from._1 - to._1) == 2 && Math.abs(from._2 - to._2) == 1) || (Math.abs(from._1 - to._1) == 1 && Math.abs(from._2 - to._2) == 2)){
      Option(Array[(Int,Int)](from,to)) // Path only consists of from and to, no inbetween as Knight can jump
    } else {
      None
    }
  }
}

case class Pawn(var owner:Player) extends Piece {
  override val pieceName: String = "Pawn"
  override val imageFileSuffix: String = "pawn"
  var firstMoveMade = false

  override def getPathOfMovement(from: (Int, Int), to: (Int, Int)): Option[Array[(Int, Int)]] = {
    if (to._2 - from._2 == owner.yDirection || (!firstMoveMade && to._2 - from._2 == (owner.yDirection * 2))) {
      if (from._1 == to._1) Option(getStraightPath(from,to))
      else if (Math.abs(from._1 - to._1) == 1) Option(getDiagonalPath(from,to))
      else None
    } else {
      None
    }
  }
}



