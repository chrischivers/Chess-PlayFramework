package controllers

import java.util.Random

import akka.actor.{Actor, Props, ActorRef}
import models.{Board, Player2, Player1, Game}
import play.api.mvc._
import play.api.Play.current


object Application extends Controller {

  def index = Action {
    Ok(views.html.start())
  }

  def openSocket(gameId: String)  = {
        WebSocket.acceptWithActor[String, String] { request => out =>
        WebSocketActor.props(Game.activeGames.get(gameId).get, out)
    }
  }

  def setUpNewGame:String = {
      val game = new Game(Game.p1, Game.p2)
      Game.activeGames += (game.gameID -> game)
    println("Game set up with ID: " + game.gameID)
      game.gameID
  }


  def loadGame(gameId:String) = Action{
    if (Game.activeGames.contains(gameId)) {
      Ok(views.html.main(gameId))
    } else {
      Ok("Error Invalid Game ID")
    }
  }

  def processMove(gameID:String, from:String, to:String) = Action {
    val moveFromSplit = from.split(",").map(_.toInt)
    val moveFrom = (moveFromSplit(0), moveFromSplit(1))
    val moveToSplit = to.split(",").map(_.toInt)
    val moveTo = (moveToSplit(0), moveToSplit(1))

    val gameOpt = Game.activeGames.get(gameID.trim)
    if (gameOpt.isDefined) {
      val game = gameOpt.get
      val piece = game.board.state(moveFrom._1)(moveFrom._2)
      if (game.isMoveValid(moveFrom,moveTo)) {
        println("Move valid")
        game.updateBoard(moveFrom,moveTo)
        println("Board updated")
        Ok(views.html.board(game)).withHeaders(
          ("RESULT", "SUCCESS"))
      } else {
        println("Move invalid")
        Ok("N/A").withHeaders(
          ("RESULT","INVALID_MOVE")
        )
      }
    } else {
      println("Not an active game")
      Ok("N/A").withHeaders(
        ("RESULT","INVALID_GAME")
      )
    }
  }

  def getBoard(gameID: String) = Action {
    val gameOpt = Game.activeGames.get(gameID.trim)
    if (gameOpt.isDefined) {
      val game = gameOpt.get
      Ok(views.html.board(game)).withHeaders(
        ("RESULT", "SUCCESS"))
    } else {
      Ok("Error: Cannot find sboard")
    }
  }
}


object WebSocketActor {
  def props(game: Game, out: ActorRef) = {
      Props(new WebSocketActor(game, out))

  }
}

class WebSocketActor(game: Game, out: ActorRef) extends Actor {

  game.addSocket(this)

  def boardUpdated() = {
    out ! "UPDATE_BOARD"
  }
  def receive = {
    case msg: String =>
      //TODO remove?
      out ! ("I received your message: " + msg)
  }
}
