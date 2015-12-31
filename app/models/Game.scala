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
      if (isPieceOwnedByPlayer(piece) && piece.getPathOfMovement(from,to).isDefined) {
        if (isPathClear(piece.getPathOfMovement(from,to).get) && isLandingPositionValid(to)) {
          valid = true
        }
      }
    }
    valid
  }

  def isPieceOwnedByPlayer(piece: Piece):Boolean = piece.owner == nextPlayerToGo

  def isPathClear(path:Array[(Int,Int)]): Boolean = {
    var clear = true
    for (i <- 1 until path.length - 1) { //Does not check the first or last position on the path
        println("Checking square: x=" + path(i)._1 + ", y=" + path(i)._2)
        if (board.state(path(i)._1)(path(i)._2) != null) clear = false
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
    val rook1P1 = new Rook(p1)
    val rook2P1 = new Rook(p1)
    val rook1P2 = new Rook(p2)
    val rook2P2 = new Rook(p2)

    val kingP1 = new King(p1)
    val kingP2 = new King(p2)
    //TODO more

    val board = new Board
    board.addPieceToBoard(rook1P1, (0, 7))
    board.addPieceToBoard(rook2P1, (7, 7))
    board.addPieceToBoard(rook1P2, (7, 0))
    board.addPieceToBoard(rook2P2, (0, 0))

    board.addPieceToBoard(kingP1, (4, 7))
    board.addPieceToBoard(kingP2, (4, 0))

    board
  }
}
