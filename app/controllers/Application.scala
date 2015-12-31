package controllers

import models.{Player2, Player1, Game}
import play.api.mvc._


object Application extends Controller {

  def index = Action {
    Ok(views.html.start())
  }

  def loadGame(gameId: Option[String]) = Action{
    if (gameId.isEmpty) {
      val game = new Game(Game.p1, Game.p2)
      println("adding game ID: " + game.gameID)
      Game.activeGames += (game.gameID -> game)
      Ok(views.html.main(game.gameID))
    } else {
      val game = Game.activeGames.get(gameId.get)
      if (game.isDefined) {
        Ok(views.html.main(game.get.gameID))
      } else {
        Ok("Invalid route")
      }
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
      NotImplemented
    }
  }
}
