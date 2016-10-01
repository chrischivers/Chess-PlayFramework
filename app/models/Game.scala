package models

import java.util.Random

import akka.actor.{Actor, Props, ActorRef}
import controllers.WebSocketActor
import play.api.mvc.WebSocket

import scala.collection.mutable.ListBuffer


class Game(gameID:String, p1:Player1, p2:Player2) {

  private var openSockets: Array[WebSocketActor] = Array()
  var nextPlayerToGo: Player = p1
  val liveBoard: Board = Game.setUpNewBoard(p1,p2)
  var playersInCheck:Map[Player, Boolean] = Map(p1 -> false, p2 -> false)

  def addSocket(actor:WebSocketActor) = {
    openSockets = openSockets :+ actor
  }

  def updateBoard(board:Board, from: Position, to: Position) = {
    board.updateBoard(from,to)
    nextPlayerToGo = if (nextPlayerToGo == p1) p2 else p1
    updatePlayersInCheck(board)
    for (actor <- openSockets) {
      actor.boardUpdated()
    }
  }

  def updatePlayersInCheck(board:Board) = {
    for (player <- Seq(p1,p2)) {
      val kingPosition = liveBoard.getKingPosition(player)
      playersInCheck += player -> liveBoard.getAllPieces.exists(x => x.owner != player && isMoveValid(board, x.owner, x.currentPosition, kingPosition))
    }
  }

  def getBoardState = liveBoard.state

  def getPiecesTaken = liveBoard.piecesTaken

  def isMoveValid(board: Board, playerMoving: Player, from: Position, to: Position): Boolean = {
    var valid = false
    val piece = board.state(from.x)(from.y)
    if (piece.isDefined) {
      if (piece.get.owner == playerMoving) {
        val path = piece.get.getPathOfMovement(from, to)
        if (path.isDefined) {
          if (isPathClear(board, path.get) && isLandingPositionValid(board, playerMoving, from, to)) {
            println(path.get.foreach(step => print("Step :" + step.x + ", " + step.y + ". ")))
            valid = true

          }
        }
      }
    }
    valid
  }


  def isPathClear(board:Board, path:Array[Position]): Boolean = {
    var clear = true
    for (i <- 1 until path.length - 1) { //Does not check the first or last position on the path
      println("Checking square: x=" + path(i).x + ", y=" + path(i).y)
      if (board.state(path(i).x)(path(i).y).isDefined) clear = false
      if (board.state(path(i).x)(path(i).y).isDefined) clear = false
    }
    clear
  }

  def isLandingPositionValid(board:Board, playerMoving: Player, from: Position, to: Position): Boolean = {
    var valid = false
    val thisPiece = board.state(from.x)(from.y)
    val landingOnPiece = board.state(to.x)(to.y)

    if (!thisPiece.orNull.isInstanceOf[Pawn]) {
      if (landingOnPiece.isEmpty) valid = true
      else if (landingOnPiece.get.owner != playerMoving) valid = true
      else valid = false //If landing on own piece
      valid

    } else {
      if (Math.abs(from.x - to.x) == 1) {
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


  def getPieceAtLocation(position:Position):Option[Piece] = getPieceAtLocation(liveBoard,position)
  def getPieceAtLocation(board:Board, position:Position) = {
    board.state(position.x)(position.y)
  }


  def doesMovePutMovingPlayerInCheck(board:Board, playerMoving: Player, from: Position, to: Position): Boolean = {
    val simulateBoard:Board = board.createClone
    simulateBoard.updateBoard(from,to)
    val kingPosition = simulateBoard.getKingPosition(playerMoving)
    val piecesCausingCheck = simulateBoard.getAllPieces.filter(x => x.owner != playerMoving && isMoveValid(simulateBoard, x.owner, x.currentPosition, kingPosition))
    piecesCausingCheck.foreach(println)
    if (piecesCausingCheck.length > 0 ) true else false
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


    board.addPieceToBoard(rook1P1, Position(0, 7))
    board.addPieceToBoard(rook2P1, Position(7, 7))
    board.addPieceToBoard(rook1P2, Position(7, 0))
    board.addPieceToBoard(rook2P2, Position(0, 0))

    board.addPieceToBoard(kingP1, Position(4, 7))
    board.addPieceToBoard(kingP2, Position(4, 0))

    board.addPieceToBoard(queenP1, Position(3,7))
    board.addPieceToBoard(queenP2, Position(3,0))

    board.addPieceToBoard(bishop1P1, Position(2,7))
    board.addPieceToBoard(bishop2P1, Position(5,7))
    board.addPieceToBoard(bishop1P2, Position(2,0))
    board.addPieceToBoard(bishop2P2, Position(5,0))

    board.addPieceToBoard(knight1P1, Position(1,7))
    board.addPieceToBoard(knight2P1, Position(6,7))
    board.addPieceToBoard(knight1P2, Position(1,0))
    board.addPieceToBoard(knight2P2, Position(6,0))

    board.addPieceToBoard(pawn1P1, Position(0,6))
    board.addPieceToBoard(pawn2P1, Position(1,6))
    board.addPieceToBoard(pawn3P1, Position(2,6))
    board.addPieceToBoard(pawn4P1, Position(3,6))
    board.addPieceToBoard(pawn5P1, Position(4,6))
    board.addPieceToBoard(pawn6P1, Position(5,6))
    board.addPieceToBoard(pawn7P1, Position(6,6))
    board.addPieceToBoard(pawn8P1, Position(7,6))

    board.addPieceToBoard(pawn1P2, Position(0,1))
    board.addPieceToBoard(pawn2P2, Position(1,1))
    board.addPieceToBoard(pawn3P2, Position(2,1))
    board.addPieceToBoard(pawn4P2, Position(3,1))
    board.addPieceToBoard(pawn5P2, Position(4,1))
    board.addPieceToBoard(pawn6P2, Position(5,1))
    board.addPieceToBoard(pawn7P2, Position(6,1))
    board.addPieceToBoard(pawn8P2, Position(7,1))

    board
  }

  def generateGameID:String = {
    var iD = Math.abs(new Random().nextInt()).toString
    while (Game.activeGames.contains(iD)) {
      iD = Math.abs(new Random().nextInt()).toString
    }
    iD
  }


}