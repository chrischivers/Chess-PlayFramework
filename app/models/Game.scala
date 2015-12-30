package models

import controllers.Application


class Game() {

  var gameID = "1234" //TODO sort this with UUID
  var nextPlayerToGo:Player = new Player1("Player 1")
  val board: Board = Game.setUpNewBoard()

  def isMoveValid(from:(Int,Int), to:(Int,Int)):Boolean = {
    var valid = false
    val piece = board.state(from._1)(from._2)
    if (piece != null) {
      if (piece.canPiecePerformMove(from, to) && isPathClear(from, to) && isLandingPositionValid(to)) {
        //TODO make allowances for pieces that can 'jump'
        valid = true
      }
    } else valid = false
    valid
  }

  def isPathClear (from:(Int,Int), to:(Int,Int)):Boolean = {
    var clear = true
    val xLoopDirection = if (from._1 - to._1 < 0) 1 else -1
    val yLoopDirection = if (from._2 - to._2 < 0) 1 else -1
    for (x <- from._1  to to._1 by xLoopDirection) {
      for (y <- from._2 to to._2 by yLoopDirection) {
        println("Checking square: x=" + x + ", y=" + y)
        if (board.state(x)(y) != null && (x,y) != from && (x,y) != to) clear = false
      }
    }
    clear
  }

  def isLandingPositionValid(to:(Int,Int)):Boolean = {
    var valid = false
    val piece = board.state(to._1)(to._2)
    if (piece == null) valid = true
    else if (piece.owner != nextPlayerToGo) valid = true
    else valid = false //If landing on own piece
    valid
  }

  def updateBoard(from:(Int,Int), to:(Int,Int)) = {
    val activePiece = board.state(from._1)(from._2)
    board.state(from._1)(from._2) = null
    board.state(to._1)(to._2) = activePiece
    assert(activePiece != null)
  }

}

object Game {
  def setUpNewBoard():Board = {
    val player1 = new Player1("Player 1")
    val player2 = new Player2("Player 2")

    val castle1P1 = new Castle(player1)
    val castle2P1 = new Castle(player1)
    //TODO more

    val board = new Board
    board.addPieceToBoard(castle1P1,(0,7))
    board.addPieceToBoard(castle2P1,(7,7))
    board
  }
}
