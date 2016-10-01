package controllers

import java.util.Random

import akka.actor.{Actor, Props, ActorRef}
import models._
import play.api.mvc._
import play.api.Play.current

import scala.collection.mutable.ArrayBuffer


object Application extends Controller {

  def index = Action {
    Ok(views.html.start())
  }

  def openSocket(gameId: String) = {
    WebSocket.acceptWithActor[String, String] { request => out =>
        WebSocketActor.props(Game.activeGames.get(gameId).get, out)
    }
  }

  def setUpNewGame() = Action {
    val gameID = Game.generateGameID
    val game = new Game(gameID, Game.p1, Game.p2)
    Game.activeGames += (gameID -> game)
    Redirect("/games/" + gameID)
  }


  def loadGame(gameId: String) = Action {
    if (Game.activeGames.contains(gameId)) {
      Ok(views.html.main(gameId))
    } else {
      Ok("Error Invalid Game ID")
    }
  }


  def processMove(gameID: String) = Action {
    request =>

      val moveFrom = request.body.asFormUrlEncoded.get("move-from") match {
        case a: ArrayBuffer[String] => Position(a.head.split(",")(0).toInt, a.head.split(",")(1).toInt)
        case _ => throw new IllegalArgumentException
      }
      val moveTo = request.body.asFormUrlEncoded.get("move-to") match {
        case a: ArrayBuffer[String] => Position(a.head.split(",")(0).toInt, a.head.split(",")(1).toInt)
        case _ => throw new IllegalArgumentException
      }

      Game.activeGames.get(gameID.trim).isDefined match {
        case false => Ok("N/A").withHeaders(("RESULT", "Game with this ID does not exist."))
        case true =>
          val game = Game.activeGames.get(gameID.trim).get
          val piece = game.getPieceAtLocation(Position(moveFrom.x, moveFrom.y))
          game.isMoveValid(game.liveBoard, game.nextPlayerToGo, moveFrom, moveTo) match {
            case false => Ok("N/A").withHeaders(("RESULT", "Invalid move. Moving piece from " + moveFrom +  " to " + moveTo + " is invalid."))
            case true =>
              game.doesMovePutMovingPlayerInCheck(game.liveBoard, game.nextPlayerToGo, moveFrom, moveTo) match {
                case true => Ok("N/A").withHeaders(("RESULT", "Invalid Move. By making this move the player would be in check."))
                case false =>
                  game.updateBoard(game.liveBoard, moveFrom, moveTo)
                  Ok(views.html.board(game)).withHeaders(("RESULT", "SUCCESS"))
              }
          }
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
