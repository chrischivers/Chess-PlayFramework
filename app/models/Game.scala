package models

import java.util.Random


class Game(p1:Player1, p2:Player2) {

  val gameID = Math.abs(new Random().nextInt()).toString

  var nextPlayerToGo: Player = p1
  val board: Board = Game.setUpNewBoard(p1,p2)

  def isMoveValid(from: (Int, Int), to: (Int, Int)): Boolean = {
    var valid = false
    val piece = board.state(from._1)(from._2)
    if (piece != null) {
      if (isPieceOwnedByPlayer(piece) && piece.canPiecePerformMove(from, to) && isPathClear(from, to) && isLandingPositionValid(to)) {
        //TODO make allowances for pieces that can 'jump'
        valid = true
      }
    } else valid = false
    valid
  }

  def isPieceOwnedByPlayer(piece: Piece):Boolean = piece.owner == nextPlayerToGo

  def isPathClear(from: (Int, Int), to: (Int, Int)): Boolean = {
    var clear = true
    val xLoopDirection = if (from._1 - to._1 < 0) 1 else -1
    val yLoopDirection = if (from._2 - to._2 < 0) 1 else -1
    for (x <- from._1 to to._1 by xLoopDirection) {
      for (y <- from._2 to to._2 by yLoopDirection) {
        println("Checking square: x=" + x + ", y=" + y)
        if (board.state(x)(y) != null && (x, y) != from && (x, y) != to) clear = false
      }
    }
    clear
  }

  def isLandingPositionValid(to: (Int, Int)): Boolean = {
    var valid = false
    val piece = board.state(to._1)(to._2)
    if (piece == null) valid = true
    else if (piece.owner != nextPlayerToGo) valid = true
    else valid = false //If landing on own piece
    valid
  }

  def updateBoard(from: (Int, Int), to: (Int, Int)) = {
    val activePiece = board.state(from._1)(from._2)
    board.state(from._1)(from._2) = null
    board.state(to._1)(to._2) = activePiece
    assert(activePiece != null)
    nextPlayerToGo = if (nextPlayerToGo == p1) p2 else p1
  }
}

object Game {


  def setUpNewBoard(p1:Player1, p2:Player2): Board = {
    val castle1P1 = new Castle(p1)
    val castle2P1 = new Castle(p1)
    val castle1P2 = new Castle(p2)
    val castle2P2 = new Castle(p2)
    //TODO more

    val board = new Board
    board.addPieceToBoard(castle1P1, (0, 7))
    board.addPieceToBoard(castle2P1, (7, 7))
    board.addPieceToBoard(castle1P2, (7, 0))
    board.addPieceToBoard(castle2P2, (0, 0))
    board
  }
}
