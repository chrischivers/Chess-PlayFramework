package models

import java.util.Random

import akka.actor.{Actor, Props, ActorRef}
import controllers.WebSocketActor
import play.api.mvc.WebSocket

import scala.collection.mutable.ListBuffer


class Game(p1:Player1, p2:Player2) {

  private var openSockets: Array[WebSocketActor] = Array()
  var nextPlayerToGo: Player = p1
  val board: Board = Game.setUpNewBoard(p1,p2)
  var playersInCheck:Map[Player, Boolean] = Map(p1 -> false, p2 -> false)

  def addSocket(actor:WebSocketActor) = {
    openSockets = openSockets :+ actor
  }

  val gameID = {
    var iD = Math.abs(new Random().nextInt()).toString
    while (Game.activeGames.contains(iD)) {
      iD = Math.abs(new Random().nextInt()).toString
    }
    iD
  }

  def isMoveValid(playerMoving: Player, from: (Int, Int), to: (Int, Int)): Boolean = {
    var valid = false
    val piece = board.state(from._1)(from._2)
    if (piece.isDefined) {
      if (piece.get.owner == playerMoving) {
        val path = piece.get.getPathOfMovement(from, to)
        if (path.isDefined) {
          if (isPathClear(path.get) && isLandingPositionValid(playerMoving, from, to)) {

              valid = true

          }
        }
      }
    }
    valid
  }


  def isPathClear(path:Array[(Int,Int)]): Boolean = {
    var clear = true
    for (i <- 1 until path.length - 1) { //Does not check the first or last position on the path
        println("Checking square: x=" + path(i)._1 + ", y=" + path(i)._2)
        if (board.state(path(i)._1)(path(i)._2).isDefined) clear = false
        if (board.state(path(i)._1)(path(i)._2).isDefined) clear = false
    }
    clear
  }

  def isLandingPositionValid(playerMoving: Player, from: (Int, Int), to: (Int, Int)): Boolean = {
    var valid = false
    val thisPiece = board.state(from._1)(from._2)
    val landingOnPiece = board.state(to._1)(to._2)

    if (!thisPiece.orNull.isInstanceOf[Pawn]) {
      if (landingOnPiece.isEmpty) valid = true
      else if (landingOnPiece.get.owner != playerMoving) valid = true
      else valid = false //If landing on own piece
      valid

    } else {
      if (Math.abs(from._1 - to._1) == 1) {
        if (landingOnPiece.isEmpty) valid = false
        else if (landingOnPiece.get.owner != playerMoving) valid = true
        else valid = false //If landing on own piece
      } else {
        if (landingOnPiece.isEmpty) valid = true
        else valid = false //Pawn cannot take pieces in a straight line
      }
      valid
    }
  }

  def updateBoard(from: (Int, Int), to: (Int, Int)) = {
    board.updateBoard(from,to)
    nextPlayerToGo = if (nextPlayerToGo == p1) p2 else p1
    updatePlayersInCheck
    for (actor <- openSockets) {
      actor.boardUpdated()
    }
  }

  def updatePlayersInCheck = {
    for (player <- Seq(p1,p2)) {
      val kingPosition = board.getKingPosition(player)
      playersInCheck += player -> board.getAllPieces.exists(x => x.owner != player && isMoveValid(x.owner, x.currentPosition, kingPosition))
    }
  }

  def doesMovePutMovingPlayerInCheck(playerMoving: Player, from: (Int, Int), to: (Int, Int)): Boolean = {
    val simulateBoard:Board = board.createClone
    simulateBoard.updateBoard(from,to)
    val kingPosition = simulateBoard.getKingPosition(playerMoving)
    simulateBoard.getAllPieces.exists(x => x.owner != playerMoving && isMoveValid(x.owner, x.currentPosition, kingPosition))
  }
}

object Game {

  val p1 = new Player1("P1")
  val p2 = new Player2("P2")
  var activeGames:Map[String, Game] = Map()

  def setUpNewBoard(p1:Player1, p2:Player2): Board = {

    val board = new Board

    val rook1P1 = new Rook(p1)
    val rook2P1 = new Rook(p1)
    val rook1P2 = new Rook(p2)
    val rook2P2 = new Rook(p2)

    val kingP1 = new King(p1)
    val kingP2 = new King(p2)

    val queenP1 = new Queen(p1)
    val queenP2 = new Queen(p2)

    val bishop1P1 = new Bishop(p1)
    val bishop2P1 = new Bishop(p1)
    val bishop1P2 = new Bishop(p2)
    val bishop2P2 = new Bishop(p2)

    val knight1P1 = new Knight(p1)
    val knight2P1 = new Knight(p1)
    val knight1P2 = new Knight(p2)
    val knight2P2 = new Knight(p2)

    val pawn1P1 = new Pawn(p1)
    val pawn2P1 = new Pawn(p1)
    val pawn3P1 = new Pawn(p1)
    val pawn4P1 = new Pawn(p1)
    val pawn5P1 = new Pawn(p1)
    val pawn6P1 = new Pawn(p1)
    val pawn7P1 = new Pawn(p1)
    val pawn8P1 = new Pawn(p1)

    val pawn1P2 = new Pawn(p2)
    val pawn2P2 = new Pawn(p2)
    val pawn3P2 = new Pawn(p2)
    val pawn4P2 = new Pawn(p2)
    val pawn5P2 = new Pawn(p2)
    val pawn6P2 = new Pawn(p2)
    val pawn7P2 = new Pawn(p2)
    val pawn8P2 = new Pawn(p2)


    board.addPieceToBoard(rook1P1, (0, 7))
    board.addPieceToBoard(rook2P1, (7, 7))
    board.addPieceToBoard(rook1P2, (7, 0))
    board.addPieceToBoard(rook2P2, (0, 0))

    board.addPieceToBoard(kingP1, (4, 7))
    board.addPieceToBoard(kingP2, (4, 0))

    board.addPieceToBoard(queenP1, (3,7))
    board.addPieceToBoard(queenP2, (3,0))

    board.addPieceToBoard(bishop1P1, (2,7))
    board.addPieceToBoard(bishop2P1, (5,7))
    board.addPieceToBoard(bishop1P2, (2,0))
    board.addPieceToBoard(bishop2P2, (5,0))

    board.addPieceToBoard(knight1P1, (1,7))
    board.addPieceToBoard(knight2P1, (6,7))
    board.addPieceToBoard(knight1P2, (1,0))
    board.addPieceToBoard(knight2P2, (6,0))

    board.addPieceToBoard(pawn1P1, (0,6))
    board.addPieceToBoard(pawn2P1, (1,6))
    board.addPieceToBoard(pawn3P1, (2,6))
    board.addPieceToBoard(pawn4P1, (3,6))
    board.addPieceToBoard(pawn5P1, (4,6))
    board.addPieceToBoard(pawn6P1, (5,6))
    board.addPieceToBoard(pawn7P1, (6,6))
    board.addPieceToBoard(pawn8P1, (7,6))

    board.addPieceToBoard(pawn1P2, (0,1))
    board.addPieceToBoard(pawn2P2, (1,1))
    board.addPieceToBoard(pawn3P2, (2,1))
    board.addPieceToBoard(pawn4P2, (3,1))
    board.addPieceToBoard(pawn5P2, (4,1))
    board.addPieceToBoard(pawn6P2, (5,1))
    board.addPieceToBoard(pawn7P2, (6,1))
    board.addPieceToBoard(pawn8P2, (7,1))

    board
  }
}